#' results_para UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_results_para_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$div(style = "text-align:center",
             tags$br(),
             tags$p("Please select one of the disease or pathways you would like to inspect further"),
             actionButton(ns("inspect_disease"), label = "Inspect disease or pathway")
    )
  )
}
    
#' results_para Server Function
#'
#' @noRd 
mod_results_para_server <- function(input, output, session, selected_enrichment_object){
  ns <- session$ns
  

   results_para_module <- reactiveValues()
   
   observeEvent(input$inspect_disease, {
   results_para_module$inspect_disease <- input$inspect_disease
   })

  return(results_para_module)
}
    
## To be copied in the UI
# mod_results_para_ui("results_para_ui_1")
    
## To be copied in the server
# callModule(mod_results_para_server, "results_para_ui_1")
 
