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
    uiOutput(ns("module_input")),
    uiOutput(ns("error_p_value")),
    
    #Description of the method "Finds disease assosciations and creates enrichment analysis objects"
    #Question is to add the parameter ont,that could either be DO or DOlite, DO recommended by the author
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
    actionButton(ns("load_inputDO"), label = "Use method") #This button I want in the middle

  )}

#' enrichDO Server Function
#'
#' @noRd 
# con should ne somewhere in the code?
mod_enrichDO_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  #Unnecessary code? {
  # output$background_genes <- renderUI({
  #   print(input$module_object)
  #   ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
  #   selectInput(ns("ppi_object"), label = "Background genes", choices = ppi_networks, popup = "The background genes are all the genes present in the PPI network")
  # })
  # } Unneccessary code ?
  
  
  observeEvent(input$load_inputDO, {
    id <- showNotification("Identifying disease assosciation and creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module_genes <- MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes
    background_genes <- unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
    enrichment_objectONE <- try(DOSE::enrichDO(gene = module_genes,
                                               ont = "DO",
                                               pvalueCutoff = input$pvalueCutoff,
                                               pAdjustMethod = input$padj_method,
                                               universe = background_genes,
                                               minGSSize = input$mingssize,
                                               maxGSSize = input$maxgssize,
                                               qvalueCutoff = input$qvalue_cutoff,
                                               readable = FALSE,
                                               
    )
    )
    if (class(enrichment_objectONE) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_objectONE)
      })
    }})
}


## To be copied in the UI
# mod_DOSE_ui("enrichDO_ui_1")

## To be copied in the server
# callModule(mod_enrichDO_server, "enrich_ui_1")
