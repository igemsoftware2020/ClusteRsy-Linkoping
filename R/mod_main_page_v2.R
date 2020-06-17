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
    navbarPage(title =  "MODifieRWeb", collapsible = TRUE,
               tabPanel("Input data", mod_Columns_ui(ns("Columns_ui_1"))),
               tabPanel("Visualization", mod_visual_ui(ns("visual_ui_1")))
               ))
}
    
#' main_page_v2 Server Function
#'
#' @noRd 
mod_main_page_v2_server <- function(input, output, session){
  ns <- session$ns
  callModule(mod_Columns_server, "Columns_ui_1")
  callModule(mod_visual_server, "visual_ui_1")
}
    
## To be copied in the UI
# mod_main_page_v2_ui("main_page_v2_ui_1")
    
## To be copied in the server
# callModule(mod_main_page_v2_server, "main_page_v2_ui_1")
 
