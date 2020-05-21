#' main_page_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_page_v2_ui <- function(id){
  ns <- NS(id)
  tagList(
 navbarPage(title = "MODifieRWeb",
            tabPanel("Input"))
  )
}
    
#' main_page_v2 Server Function
#'
#' @noRd 
mod_main_page_v2_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_main_page_v2_ui("main_page_v2_ui_1")
    
## To be copied in the server
# callModule(mod_main_page_v2_server, "main_page_v2_ui_1")
 
