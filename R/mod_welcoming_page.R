#' welcoming_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcoming_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(),
    tags$img(id = "animated_div", src="www/igem.jpg")
  )
}
    
#' welcoming_page Server Function
#'
#' @noRd 
mod_welcoming_page_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_welcoming_page_ui("welcoming_page_ui_1")
    
## To be copied in the server
# callModule(mod_welcoming_page_server, "welcoming_page_ui_1")
 
