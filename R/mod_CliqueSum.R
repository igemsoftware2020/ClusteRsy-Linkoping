#' CliqueSum UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CliqueSum_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff for DEGs", min = 0, max = 1, value = 0.05),
  )
}
    
#' CliqueSum Server Function
#'
#' @noRd 
mod_CliqueSum_server <- function(input, output, session){
  ns <- session$ns
 
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
}
    
## To be copied in the UI
# mod_CliqueSum_ui("CliqueSum_ui_1")
    
## To be copied in the server
# callModule(mod_CliqueSum_server, "CliqueSum_ui_1")
 
