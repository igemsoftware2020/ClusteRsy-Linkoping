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
                popup = "Correction methods used to control p-values and q-values",
                multiple = FALSE,
                selectize = TRUE),
    sliderInput(ns("exponent"), label = "Exponent", value = 5, max = 100, min = 0, popup = "Weight of each step"),
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set", value = 5, max = 100, min = 0, popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximal size of each gene set", value = 500, max = 5000, min = 0, popup = "Maximum size of each gene set used for analyzing"),
    sliderInput(ns("nperm"), label = "Permutation number", min = 1, max = 1000, value = 500, popup = "Number of permutations that should be performed"),
    prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning", popup = "Get reproducible results"),
    selectInput(ns("by"), label = "Select algorithm",
                choices = c("fgsea",
                            "DOSE"),
                popup = "Algorithm used for the gene set enrichment analysis"),
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich") 
    )
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

  gene_list <- get_sorted_module_genes(input$module_object, con = con)
  
  gse_object <- try(DOSE::gseDO(
                                geneList = genes,
                                exponent = input$exponent,
                                nPerm = input$nperm,
                                pvalueCutoff = input$pvaluecutoff,
                                pAdjustMethod = input$padjustmethod,
                                minGSSize = input$mingssize,
                                maxGSSize = input$maxgssize,
                                by = input$by,
                                seed = input$include_seed,
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
 
