#' choose_directory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choose_directory_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' choose_directory Server Function
#'
#' @noRd 
mod_choose_directory_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_choose_directory_ui("choose_directory_ui_1")
    
## To be copied in the server
# callModule(mod_choose_directory_server, "choose_directory_ui_1")
 
