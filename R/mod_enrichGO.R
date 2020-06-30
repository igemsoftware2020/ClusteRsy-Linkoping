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
      ns("keytype"),
      label = "Select keytype of input gene",
      choices = c(keytypes(org.Hs.eg.db::org.Hs.eg.db))
      ),
    selectInput(
      ns("ont"),
      label = "Select subontologies",
      choices = c("BP", "MF", "CC", "ALL")
      ),
    sliderInput(
      ns("pvaluecutoff"),
      label = "Select p-value cutoff",
      min = 0, 
      max = 1, 
      value = 0.05
      ), 
    selectInput(
      ns("padjustmethod"), 
      label = "Select method", 
      choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none ")
      ),
    sliderInput(
      ns("qvaluecutoff"), 
      label = "Select q-value cutoff", 
      min = 0, 
      max = 1, 
      value = 0.2
      ), 
    sliderInput(
      ns("mingssize"), 
      label = "Select minimal size of genes for testing", 
      min = 1, 
      max = 100, 
      value = 10),
    sliderInput(
      ns("maxgssize"),
      label = "Select maximal size of genes for testing", 
      min = 100, 
      max = 1000,
      value = 500
      ), 
    prettySwitch(
      ns("readable"),
      label = "Readable",
      value = FALSE, 
      status = "warning"
      ), 
    prettySwitch(
      ns("pool"), 
      label = "Pool",
      value = FALSE, 
      status = "warning"
      ),
    
    actionButton(ns("load_input"), "Create enrichment analysis object")
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
    
    module_genes <- get_module_genes(input$module_object, con = con)
    background_genes <- get_background_genes(input$module_object, con = con)

    enrichment_object <- try(clusterProfiler::enrichGO(gene = module_genes,
                                                       OrgDb = 'org.Hs.eg.db', 
                                                       keyType = input$keytype, 
                                                       ont = input$ont, 
                                                       pvalueCutoff = input$pvaluecutoff, 
                                                       pAdjustMethod = input$padjustmethod, 
                                                       universe = background_genes, 
                                                       qvalueCutoff = input$pvaluecutoff, 
                                                       minGSSize = input$mingssize, 
                                                       maxGSSize = input$maxgssize, 
                                                       readable = input$readable,
                                                       pool = input$pool
    ))
    if (class(enrichment_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object)
      })
    }
  })
}

## To be copies in the UI 
# mod_GO_ui("GO_ui_1")

## To be copied in the server
# callModule(mod_GO_server, "GO_ui_1")


    
    
    
    