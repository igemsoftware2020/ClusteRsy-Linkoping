#' DiffCoEx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DiffCoEx_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' DiffCoEx Server Function
#'
#' @noRd 
mod_DiffCoEx_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_DiffCoEx_ui("DiffCoEx_ui_1")
    
## To be copied in the server
# callModule(mod_DiffCoEx_server, "DiffCoEx_ui_1")
 
