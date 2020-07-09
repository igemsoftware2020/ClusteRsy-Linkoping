#' enrichDO UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichDO_ui <- function(id){
  ns <- NS(id)
  tagList(
    #Description of the method "Finds disease assosciations and creates enrichment analysis objects"
    uiOutput(ns("module_input")),
    sliderInput(ns("pvalueCutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value"),
    sliderInput(ns("qvalueCutoff"), label = "Q-value cut-off", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value. Q-values are false discovery rate (FDR) adjusted p-values"),
    selectInput(ns("pAdjustMethod"), "Select an adjustment method",
                choices = c("holm",
                            "hochberg",
                            "hommel",
                            "bonferroni",
                            "BH",
                            "BY",
                            "fdr",
                            "none"), popup = "Correction methods used to control p-values and q-values",
                multiple = FALSE,
                selectize = TRUE),
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set", min = 0, max = 100, value = 5, popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximum size of each gene set", min = 0,  max = 5000, value = 500, popup = "Maximum size of each gene set used for analyzing"),
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich", onclick="loading_modal_open(); stopWatch();"),
              htmlOutput(ns("close_loading_modal"))  # Close modal with JS 
    )


  )}
#Question is to add the parameter ont,that could either be DO or DOlite, DO recommended by the author

#' enrichDO Server Function
#'
#' @noRd 
mod_enrichDO_server <- function(input, output, session, con, Description1_ui_1){
  ns <- session$ns
  
  enrichDO_module <- reactiveValues()
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  observeEvent(Description1_ui_1$module_name, {
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    updateSelectInput(session, "module_object", choices = module_objects)
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Identifying disease assosciation and creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    module_genes <- get_module_genes(input$module_object, con = con)
    background_genes <- get_background_genes(input$module_object, con = con)
    
    enrichment_object <- try(DOSE::enrichDO(gene = module_genes,
                                               ont = "DO",
                                               pvalueCutoff = input$pvalueCutoff,
                                               pAdjustMethod = input$pAdjustMethod,
                                               universe = background_genes,
                                               minGSSize = input$mingssize,
                                               maxGSSize = input$maxgssize,
                                               qvalueCutoff = input$qvalueCutoff,
                                               readable = FALSE
                                               
    )
    )
    if (class(enrichment_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object)
      })
    } else {
      enrichDO_module$enrich <- input$load_input 
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(enrichment_object,
                                          module_name = module_name, 
                                          enrichment_method = "enrichDO", 
                                          con = con)
    }
    # Close loading modal
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(enrichDO_module)
}


## To be copied in the UI
# mod_enrichDO_ui("enrichDO_ui_1")

## To be copied in the server
# callModule(mod_enrichDO_server, "enrichDO_ui_1")
