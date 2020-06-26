#' GO UI Function 
#' 
#' @description A shiny Module.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
#' 
#' @importFrom shiny NS tagList
mod_GO_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("module_input")),
    selectInput(
      ns("keyType"),
      label = "Select keytype of input gene",
      c("GO")),
    selectInput(
      ns("ont"),
      label = "Select subontologies",
      c("BP", "MF", "CC", "ALL")),
    sliderInput(
      ns("pvalueCutoff"),
      label = "Select p-value cutoff",
      min = 0, 
      max = 1, 
      value = 0.05), 
    selectInput(
      ns("pAdjustMethod"), 
      label = "Select method", 
      c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none ")),
    sliderInput(
      ns("qvalueCutoff"), 
      label = "Select q-value cutoff", 
      min = 0, 
      max = 1, 
      value = 0.2), 
    sliderInput(
      ns("minGSSize"), 
      label = "Select minimal size of genes for testing", 
      min = 1, 
      max = 100, 
      value = 10),
    sliderInput(
      ns("maxGSSize"),
      label = "Select maximal size of genes for testing", 
      min = 100, 
      max = 1000,
      value = 500), 
    prettySwitch(
      ns("readable"),
      label = "Readable",
      value = FALSE, 
      status = "warning"), 
    prettySwitch(
      ns("pool"), 
      label = "Pool",
      value = FALSE, 
      status = "warning"),
    
    tags$div(style = "text-align:center",
             actionButton(ns("load_input"), "Create enrichment analysis object")
    )
  )
}

#' GO Server Function 
#' 
#' @noRd
mod_GO_server <- function(input, output, session, con){
  ns <- session$ns
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    ppi_name <- as.character(MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$settings$ppi_network)
    background_genes <- unique(unlist(MODifieRDB::ppi_network_from_db(ppi_name, con = con)[,1:2]))
    module_genes <- MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes
    enrichment_object <- try(clusterProfiler::enrichGO(gene = module_genes,
                                                       OrgDb = org.Hs.eg.db, 
                                                       keyType = input$keyType, 
                                                       ont = input$ont, 
                                                       pvalueCutoff = input$pvalueCutoff, 
                                                       pAdjustMethod = input$pAdjustMethod, 
                                                       universe = background_genes, 
                                                       qvalueCutoff = input$pvalueCutoff, 
                                                       minGSSize = input$minGSSize, 
                                                       maxGSSize = input$maxGSSize, 
                                                       readable = input$readable,
                                                       pool = input$pool
    ))
    print(OrgDb)
  })
}

## To be copies in the UI 
# mod_GO_ui(ns("GO_ui_1"))

## To be copied in the server
# callModule(mod_GO_server, "GO_ui_1")


    
    
    
    