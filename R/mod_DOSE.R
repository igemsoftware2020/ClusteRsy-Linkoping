#' DOSE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DOSE_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    uiOutput(ns("error_p_value")),
    selectInput(ns("enrich"), "Choose a method", 
                 choices = c("Disease Ontology (DO)",
                             "Network of Cancer Gene (NCG)",
                             "DisGeNET (DGN)")),
    htmlOutput(ns("para"))
)}
    
#' DOSE Server Function
#'
#' @noRd 
mod_DOSE_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  output$background_genes <- renderUI({
    print(input$module_object)
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "Background genes", choices = ppi_networks, popup = "The background genes are all the genes present in the PPI network")
  })
  
  enrich <- reactive({
    input$enrich
  })
  
  observeEvent(input$enrich, {
   if (input$enrich == "Disease Ontology (DO)") {
     output$para <-renderUI({
         tagList(
           sliderInput(ns("pvalueCutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05),
           selectInput(ns("pAdjustMethod"), "Select an adjustment method",
                       choices = c("holm",
                                   "hochberg",
                                   "hommel",
                                   "bonferroni",
                                   "BH",
                                   "BY",
                                   "fdr",
                                   "none"),
                       multiple = FALSE,
                       selectize = TRUE),
           numericInput(ns("minGSSize"), label = "Minimum size of genes", value = 5, max = 100, min = 0),
           numericInput(ns("minGSSize"), label = "Maximal size each geneSet", value = 500, max = 5000, min = 0),
           sliderInput(ns("qvalueCutoff"), label = "Q-value cut-off", min = 0, max = 1, value = 0.05),
           helpText(ns("Select parameters for gene set enrichment analysis")),
           sliderInput(ns("nperm"), label = "Permutation number", min = 0, max = 1000, value = 50),
           selectInput(ns("by"), label = "Select algorithm",
                       choices = c("fgsea",
                                   "DOSE")),
           actionButton(ns("load_inputDO"), label = "Find disease assosciation and create enrichment analysis object")
   )})}})
   
  observeEvent(input$load_inputDO, {
    id <- showNotification("Identifying disease assosciation and creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module_genes <- MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes
    enrichment_objectONE <- try(DOSE::enrichDO(gene = module_genes,
                                                         ont = "DO",
                                                         pvalueCutoff = input$deg_cutoff,
                                                         pAdjustMethod = input$padj_method,
                                                         universe = input$ppi_object,
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
    
  #   module_genes <- MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes
  #   enrichment_objectTWO <- try(DOSE::gseDO(geneList = module_genes,
  #                                                      ont ="DO",
  #                                                      exponent = 1,
  #                                                      nperm = input$nperm,
  #                                                      pvalueCutoff = input$deg_cutoff,
  #                                                      pAdjustMethod = input$padj_method,
  #                                                      minGSSize = input$mingssize,
  #                                                      maxGSSize = input$maxgssize,
  #                                                      qvalueCutoff = input$qvalue_cutoff,
  #                                                      by = input$by,
  #                                                      seed = FALSE,
  #                                                      verbose = FALSE,
  #                                                      
  #   )
  #   )
  #   if (class(enrichment_objectTWO) == "try-error"){
  #     output$error_p_value <- renderUI({
  #       tags$p(class = "text-danger", tags$b("Error:"), enrichment_objectTWO)
  #     })
  #   }
  # })
  # #these two are the remaining ones
  #  observeEvent(input$enrich, {
  #    if (input$enrich == "Network of Cancer Gene (NCG)") {
  #      renderUI({
  #        tagList()
  #      })}})
  #    
  #    observeEvent(input$enrich, {
  #      if (input$enrich == "DisGeNET (DGN)") {
  #        renderUI({
  #          tagList()
  #        })}})
  # 
  #    
  # }
    
## To be copied in the UI
# mod_DOSE_ui("DOSE_ui_1")
    
## To be copied in the server
# callModule(mod_DOSE_server, "DOSE_ui_1")
 
