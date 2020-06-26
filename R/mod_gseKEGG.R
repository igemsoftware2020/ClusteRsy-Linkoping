#' gseKEGG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gseKEGG_ui <- function(id){
  ns <- NS(id)
  tagList(
          uiOutput(ns("module_input")),
          actionButton(ns("load_input"), label = "Create enrichment analysis object"),
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
          prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning")
  )
}
    
#' gseKEGG Server Function
#'
#' @noRd 
mod_gseKEGG_server <- function(input, output, session, con){
  ns <- session$ns
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    input_name <- as.character(MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$settings$MODifieR_input)

    input_data <- MODifieRDB::MODifieR_input_from_db(input_name, con = con)$diff_genes

    #module_genes <- MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes
    gene_list <- input_data[order(input_data$pvalue),]

    enrichment_object <- try(clusterProfiler::gseKEGG(geneList = gene_list,
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
    if (class(enrichment_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object)
      })
    }
  })
                                                    
    
}
    
## To be copied in the UI
# mod_gseKEGG_ui("gseKEGG_ui_1")
    
## To be copied in the server
# callModule(mod_gseKEGG_server, "gseKEGG_ui_1")
 
