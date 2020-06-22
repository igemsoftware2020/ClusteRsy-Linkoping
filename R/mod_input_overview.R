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
    tags$br(),
    tags$br(),
    DT::dataTableOutput(ns("input_overview")),
    downloadButton(ns("download_module"), "Download"),
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
    print(output$input_overview)
  })
  output$download_module <- downloadHandler(
    filename = input$input_name,
    content = MODifieRDB::MODifieR_input_from_db(""),
    contentType = ".rds"
  )
}

## To be copied in the UI
# mod_input_overview_ui("input_overview_ui_1")

## To be copied in the server
# callModule(mod_input_overview_server, "input_overview_ui_1")
 
