#' results_para UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_results_para_ui <- function(id, enrichment_results_ui_1){
  ns <- NS(id)
  tagList(
    tags$div(style = "text-align:center",
             tags$br(),
             tags$p("Here you can inspect which genes that are invovled in the disease or pathway selected from the table"),
             actionButton(ns("inspect_disease"), label = "Inspect disease or pathway")
    ),
    tags$script(if (enrichment_results_ui_1$value$check) HTML("document.getElementById('main_page_v2_ui_1-visual_ui_1-enrichment_results_para_ui_1-inspect_disease').disabled = true;") else HTML("document.getElementById('main_page_v2_ui_1-visual_ui_1-enrichment_results_para_ui_1-inspect_disease').disabled = false;"))
  )
}
    
#' results_para Server Function
#'
#' @noRd 
mod_enrichment_results_para_server <- function(input, output, session){
  ns <- session$ns

  results_para_module <- reactiveValues()
  
  observeEvent(input$inspect_disease, {
  results_para_module$inspect_disease <- input$inspect_disease
  })

  return(results_para_module)
}
    
## To be copied in the UI
# mod_enrichment_results_para_ui("mod_enrichment_results_para_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_para_server, "mod_enrichment_results_para_ui_1")
 
