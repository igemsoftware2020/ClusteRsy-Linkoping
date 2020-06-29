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
    uiOutput(ns("error_p_value")),
    sliderInput(ns("pvalueCutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05, popup = "P-value cutoff"),
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
    sliderInput(ns("mingssize"), label = "Minimum size of genes", min = 0, max = 100, value = 5, popup = "Minimal size of genes for testing"),
    sliderInput(ns("maxgssize"), label = "Maximal size each gene set", min = 0,  max = 5000, value = 500, popup = "Maximal size of each geneSet for analyzing"),
    sliderInput(ns("qvalueCutoff"), label = "Q-value cut-off", min = 0, max = 1, value = 0.05, popup = "Q-value cutoff"),
    tags$div( style = "text-align:center",
              actionButton(ns("load_inputDO"), label = "Use method") 
    )

  )}
#Question is to add the parameter ont,that could either be DO or DOlite, DO recommended by the author

#' enrichDO Server Function
#'
#' @noRd 
mod_enrichDO_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
 
  
  
  observeEvent(input$load_inputDO, {
    id <- showNotification("Identifying disease assosciation and creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module_genes <- MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes
    ppi_name <- as.character(MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$settings$ppi_network)
    background_genes <- unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
    enrichment_objectONE <- try(DOSE::enrichDO(gene = module_genes,
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
    if (class(enrichment_objectONE) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_objectONE)
      })
    }})
}


## To be copied in the UI
# mod_enrichDO_ui("enrichDO_ui_1")

## To be copied in the server
# callModule(mod_enrichDO_server, "enrichDO_ui_1")
