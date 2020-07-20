#' user_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_guide_ui <- function(id){
  ns <- NS(id)
  tagList(
 uiOutput(ns("user_guide"))
  )
}
    
#' user_guide Server Function
#'
#' @noRd 
mod_user_guide_server <- function(input, output, session){
  ns <- session$ns
 output$user_guide <- renderUI({
   
   tags$div(
     tags$h1("Welcome to the user guide!"),
     tags$p("Here you can find all the information you need about running the app"),
     tags$p("We're open source and you can find the code at our"),
           
   )
   
 })
}
    
## To be copied in the UI
# mod_user_guide_ui("user_guide_ui_1")
    
## To be copied in the server
# callModule(mod_user_guide_server, "user_guide_ui_1")
 
