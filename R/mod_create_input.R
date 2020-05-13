#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_create_input_ui <- function(id){
  ns <- NS(id)
  tagList(
   
  )
}
    
#' create_input Server Function
#'
#' @noRd 
mod_create_input_server <- function(input, output, session){
  ns <- session$ns
 

  
}
    
## To be copied in the UI
# mod_create_input_ui("create_input_ui_1")
    
## To be copied in the server
# callModule(mod_create_input_server, "create_input_ui_1")
 
