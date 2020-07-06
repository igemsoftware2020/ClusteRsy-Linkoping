#' enrichDGN UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichDGN_ui <- function(id){
  ns <- NS(id)
  tagList(
    #Description of the method "Enrichment analysis based on gene-disease associations from several public data sources and the literature".
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
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set", min = 0, max = 100, value = 10, popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximal size of each gene set", min = 0,  max = 5000, value = 500, popup = "Maximum size of each gene set used for analyzing"),
    tags$div( style = "text-align:center",
      actionButton(ns("load_input"), label = "Enrich") 
    )
    
    
  )}

#' enrichDGN Server Function
#'
#' @noRd 
# con should ne somewhere in the code?
mod_enrichDGN_server <- function(input, output, session, con, Description1_ui_1){
  ns <- session$ns
  
  enrichDGN_module <- reactiveValues()
  
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
    
    enrichment_object <- try(DOSE::enrichDGN(gene = module_genes,
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
      output$error <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object)
      })
    } else {
      enrichDGN_module$enrich <- enrichment_object
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(enrichment_object,
                                          module_name = module_name, 
                                          enrichment_method = "enrichDGN", 
                                          con = con)
    }
    })
  return(enrichDGN_module)
}


## To be copied in the UI
# mod_enrichDGN_ui("enrichDGN_ui_1")

## To be copied in the server
# callModule(mod_enrichDGN_server, "enrichDGN_ui_1")
