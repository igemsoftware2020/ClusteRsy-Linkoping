#' gseDO UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gseDO_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    uiOutput(ns("error_p_value")),
    numericInput(ns("exponent"), label = "Exponent", value = 5, max = 100, min = 0, popup = "Weight of each step"),
    numericInput(ns("minGSSize"), label = "Minimum size of genes", value = 5, max = 100, min = 0, popup = "Minimal size of genes for testing"),
    numericInput(ns("maxGSSize"), label = "Maximal size each geneSet", value = 500, max = 5000, min = 0, popup = "Maximal size of each geneSet for analyzing"),
    sliderInput(ns("nPerm"), label = "Permutation number", min = 0, max = 1000, value = 50, popup = "Number of permutations"),
    selectInput(ns("by"), label = "Select algorithm",
                choices = c("fgsea",
                            "DOSE"),
                popup = "Algorithm used for the gene set enrichment analysis"),
    sliderInput(ns("pvalueCutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value"),
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
                popup = "Correction methods used to control p-values and q-values"),
    actionButton(ns("load_input"), label = "Enrich")
  )
}
    
#' gseDO Server Function
#'
#' @noRd 
mod_gseDO_server <- function(input, output, session, con){
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
  module_genes <- sort(as.numeric(MODifieRDB::MODifieR_module_from_db(input$module_object, con = con)$module_genes))
  input_data <- data.frame(gene = c(as.numeric(input_data$gene)), pval = c(input_data$pval))
  input_data_sorted <- input_data[with(input_data, order(input_data$gene)),]
  gene_list <- subset(input_data_sorted, input_data_sorted$gene %in% module_genes )
  
  gse_object <- try(DOSE::gseDO(
                 geneList = gene_list,
                 exponent = input$exponent,
                 nPerm = input$nperm,
                 pvalueCutoff = input$deg_cutoff,
                 pAdjustMethod = input$padj_method,
                 minGSSize = input$mingssize,
                 maxGSSize = input$maxgssize,
                 by = input$by,
                 seed = FALSE,
                 verbose = FALSE  
                
  )
  )
  if (class(gse_object) == "try-error"){
    output$error_p_value <- renderUI({
      tags$p(class = "text-danger", tags$b("Error:"), gse_object)
    })
  }
  })
  
  
}

    
## To be copied in the UI
# mod_gseDO_ui("gseDO_ui_1")
    
## To be copied in the server
# callModule(mod_gseDO_server, "gseDO_ui_1")
 
