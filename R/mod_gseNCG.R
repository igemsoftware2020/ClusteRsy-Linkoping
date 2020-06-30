#' gseNCG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gseNCG_ui <- function(id){
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
                multiple = FALSE,
                popup = "Correction methods used to control p-values"),
    sliderInput(ns("exponent"), label = "Exponent", value = 5, max = 100, min = 0, popup = "Weight of each step"),
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set", value = 5, max = 100, min = 0, popup = "Minimum size of each gene set for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximum size each gene set", value = 500, max = 5000, min = 0, popup = "Maximum size of each gene set for analyzing"),
    sliderInput(ns("nperm"), label = "Permutation number", min = 1, max = 1000, value = 500, popup = "Number of permutations that should be performed"),
    prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning"),
    selectInput(ns("by"), label = "Select algorithm",
                choices = c("fgsea",
                            "DOSE"),
                popup = "Algorithm used for the gene set enrichment analysis"),
    tags$div( style = "text-align:center",
              actionButton(ns("load_inputDO"), label = "Enrich") 
    )
  )
}
    
#' gseNCG Server Function
#'
#' @noRd 
mod_gseNCG_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })  
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    gene_list <- get_sorted_module_genes(input$module_object, con = con)
   
    gse_object <- try(DOSE::gseNCG(
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
# mod_gseNCG_ui("gseNCG_ui_1")
    
## To be copied in the server
# callModule(mod_gseNCG_server, "gseNCG_ui_1")
 
