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
                            "uniprot"),
                popup = "Select the type of the input data"),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value"),
    selectInput(ns("padj_method"), 
                label = "Select Adjusted p-value method",
                choices = c("hochberg",
                            "hommel",
                            "bonferroni",
                            "BH",
                            "BY",
                            "fdr",
                            "none"),popup = "Correction methods used to control p-values and q-values",
                multiple = FALSE,
                selectize = TRUE),
    sliderInput(ns("exponent"), label ="Weight of each step", min = 0.01, max = 2, value = 1, popup = "Weight of each step"),
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set annotated", min = 1, max = 100, value = 10, popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximum size of each gene set annotated", min = 100, max = 1000, value = 500, popup = "Maximum size of each gene set used for analyzing"),
    sliderInput(ns("permutation"), label ="number of permutations", min = 1, max = 2000, value = 1000, popup = "Number of permutations that should be performed"),
    prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning", popup = "Get reproducible results"),
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich") 
    )
 
  )
}
    
#' gseMKEGG Server Function
#'
#' @noRd 
mod_gseMKEGG_server <- function(input, output, session, con, Description1_ui_1){
  ns <- session$ns
  
  gseMKEGG_module <- reactiveValues()
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  observeEvent(Description1_ui_1$module_name, {
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    updateSelectInput(session, "module_object", choices = module_objects)
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
                                               verbose = FALSE,
                                               seed = input$include_seed
    )
    )
    
    if (class(gse_object) == "try-error"){
      output$error <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), gse_object)
      })
    } else {
      gseMKEGG_module$enrich <- gse_object
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(gse_object,
                                          module_name = module_name, 
                                          enrichment_method = "gseMKEGG", 
                                          con = con)
    }
  })
  return(gseMKEGG_module)
}
    
## To be copied in the UI
# mod_gseMKEGG_ui("gseMKEGG_ui_1")
    
## To be copied in the server
# callModule(mod_gseMKEGG_server, "gseMKEGG_ui_1")
 
