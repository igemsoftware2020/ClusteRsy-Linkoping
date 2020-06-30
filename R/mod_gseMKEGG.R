#' gseMKEGG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gseMKEGG_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    selectInput(ns("keytype"), 
                label = "Select key type",
                choices = c("kegg",
                            "ncbi-proteinid",
                            "uniprot")),
    sliderInput(ns("exponent"), label ="Weight of each step", min = 0.01, max = 2, value = 1),
    sliderInput(ns("permutation"), label ="number of permutations", min = 1, max = 2000, value = 1000),
    sliderInput(ns("mingssize"), label = "Minimal size of genes annotated", min = 1, max = 100, value = 10, popup = "Miniman size of genes annotated by Ontology term for testing"),
    sliderInput(ns("maxgssize"), label = "Maximal size of genes annotated", min = 100, max = 1000, value = 500, popup = "Maximal size of genes annotated for testing"),
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
    prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning"),
    actionButton(ns("load_input"), label = "Create enrichment analysis object")
 
  )
}
    
#' gseMKEGG Server Function
#'
#' @noRd 
mod_gseMKEGG_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    
    gene_list <- get_sorted_module_genes(input$module_object, con = con)
    
    gse_object <- try(clusterProfiler::gseMKEGG(geneList = gene_list,
                                               organism = 'hsa',
                                               keyType = input$keytype,
                                               exponent = input$exponent,
                                               nPerm = input$permutation,
                                               minGSSize = input$mingssize,
                                               maxGSSize = input$maxgssize,
                                               pvalueCutoff = input$deg_cutoff,
                                               pAdjustMethod = input$padj_method,
                                               verbose = TRUE,
                                               seed = input$include_seed
    )
    )
    
    if (class(gse_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object)
      })
    }
  })
  
}
    
## To be copied in the UI
# mod_gseMKEGG_ui("gseMKEGG_ui_1")
    
## To be copied in the server
# callModule(mod_gseMKEGG_server, "gseMKEGG_ui_1")
 
