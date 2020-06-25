#' KEGG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_KEGG_ui <- function(id){
  ns <- NS(id)
  tagList(
          uiOutput(ns("module_input")),
          uiOutput(ns("error_p_value")),
          selectInput(ns("keytype"), 
                      label = "Select key type",
                      choices = c("kegg",
                                  "ncbi-proteinid",
                                  "uniprot")),
          sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05, popup = "P-value cutoff for enrichment tests to report"),
          selectInput(ns("padj_method"), 
                      label = "Select Adjusted p-value method",
                      choices = c("hochberg",
                                  "hommel",
                                  "bonferroni",
                                  "BH",
                                  "BY",
                                  "fdr",
                                  "none")),
          uiOutput(ns("background_genes")),
          sliderInput(ns("mingssize"), label = "Minimal size of genes annotated", min = 1, max = 100, value = 10, popup = "Miniman size of genes annotated by Ontology term for testing"),
          sliderInput(ns("maxgssize"), label = "Maximal size of genes annotated", min = 100, max = 1000, value = 500, popup = "Maximal size of genes annotated for testing"),
          sliderInput(ns("qvalue_cutoff"), label = "Q-value cutoff", min = 0, max = 1, value = 0.2, popup = "Q-value cutoff on enrichment tests to report as significant. Tests must pass I) P-value cutoff on unadjusted P-values, II) P-value cut off on unadjusted P-values and III) Q-value cut off on Q-values to be reported"),
          actionButton(ns("load_input"), label = "Create enrichment analysis object")
  )
}
    
#' KEGG Server Function
#'
#' @noRd 
mod_KEGG_server <- function(input, output, session, con){
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
    enrichment_object <- try(clusterProfiler::enrichKEGG(gene = module_genes,
                                                          organism = 'hsa', #Homo sapiens set as default.
                                                          keyType = input$keytype,
                                                          pvalueCutoff = input$deg_cutoff,
                                                          pAdjustMethod = input$padj_method,
                                                          universe = background_genes,
                                                          minGSSize = input$mingssize,
                                                          maxGSSize = input$maxgssize,
                                                          qvalueCutoff = input$qvalue_cutoff,
                                                          use_internal_data = FALSE
                                                          
                                                  )
    )
    if (class(enrichment_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object)
      })
    }
  })
  
 
}
    
## To be copied in the UI
# mod_KEGG_ui("KEGG_ui_1")
    
## To be copied in the server
# callModule(mod_KEGG_server, "KEGG_ui_1")
 
