#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visual_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_enrichment_results_ui(ns("enrichment_results_ui_1"))
  )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session, con){
  ns <- session$ns
  callModule(mod_enrichment_results_server, "enrichment_results_ui_1", con = con)
  
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

