#' input_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("refresh"), "Refresh database"),
    DT::dataTableOutput(ns("input_overview"))
  )
}

#' input_overview Server Function
#'
#' @noRd 
mod_input_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
  input_objects <- MODifieRDB::get_available_input_objects(con)
  
  output$input_overview <- DT::renderDataTable(input_objects)
  
  observeEvent(input$refresh, {
    input_objects <- MODifieRDB::get_available_input_objects(con)
    
    output$input_overview <- DT::renderDataTable(input_objects)
  })
  
}

## To be copied in the UI
# mod_input_overview_ui("input_overview_ui_1")

## To be copied in the server
# callModule(mod_input_overview_server, "input_overview_ui_1")
 
