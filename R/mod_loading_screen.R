#' loading_screen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_loading_screen_ui <- function(id){
  ns <- NS(id)
  tagList(
    )
}
    
#' loading_screen Server Function
#'
#' @noRd 
mod_loading_screen_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_loading_screen_ui('loading_screen_ui_1')
    
## To be copied in the server
# callModule(mod_loading_screen_server, 'loading_screen_ui_1')
 
