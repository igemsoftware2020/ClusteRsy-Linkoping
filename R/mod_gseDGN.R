#' gseDGN UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gseDGN_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    uiOutput(ns("error_p_value")),
    sliderInput(ns("pvaluecutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value"),
    selectInput(ns("padjustmethod"), "Select an adjustment method",
                choices = c("holm",
                            "hochberg",
                            "hommel",
                            "bonferroni",
                            "BH",
                            "BY",
                            "fdr",
                            "none"),
    numericInput(ns("exponent"), label = "Exponent", value = 5, max = 100, min = 0, popup = "Weight of each step"),
    numericInput(ns("mingssize"), label = "Minimum size of genes", value = 5, max = 100, min = 0, popup = "Minimum size of each gene set for analyzing"),
    numericInput(ns("maxgssize"), label = "Maximal size each geneSet", value = 500, max = 5000, min = 0, popup = "Maximum size of each gene set for analyzing"),
    sliderInput(ns("nperm"), label = "Permutation number", min = 1, max = 1000, value = 500, popup = "Number of permutations that should be performed"),
    selectInput(ns("by"), label = "Select algorithm",
                choices = c("fgsea",
                            "DOSE"),
                popup = "Algorithm used for the gene set enrichment analysis"),
    
                multiple = FALSE,
                popup = "Correction methods used to control p-values and q-values"),
    tags$div( style = "text-align:center",
              actionButton(ns("load_inputDO"), label = "Enrich") 
    )
  )
}
    
#' gseDGN Server Function
#'
#' @noRd 
mod_gseDGN_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })  
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    gene_list <- get_sorted_module_genes(input$module_object, con = con)
    
    gse_object <- try(DOSE::gseDGN(
                                  geneList = genes,
                                  exponent = input$exponent,
                                  nPerm = input$nperm,
                                  pvalueCutoff = input$pvaluecutoff,
                                  pAdjustMethod = input$padjustmethod,
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
# mod_gseDGN_ui("gseDGN_ui_1")
    
## To be copied in the server
# callModule(mod_gseDGN_server, "gseDGN_ui_1")
 
