#' module_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_module_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(ns("refresh"), "Refresh database"),
    tags$br(),
    tags$br(),
    DT::dataTableOutput(ns("module_overview")),
    downloadButton(ns("download_module"), "Download")
  )
}

#' module_overview Server Function
#'
#' @noRd 
mod_module_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
  module_objects <- MODifieRDB::get_available_module_objects(con)
  
  output$module_overview <- DT::renderDataTable(module_objects)
  
  observeEvent(input$refresh, {
    module_objects <- MODifieRDB::get_available_module_objects(con)
    
    output$module_overview <- DT::renderDataTable(module_objects)
  })
  
  output$download_module <- downloadHandler(
    filename = function() {
      paste0("module_set", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(retrieve_module(), file)
    }
  )
  
  current_modules <- function() {
    module_objects$module_name[input$module_overview_rows_selected]
  }
  
  
  retrieve_module <- function(){
    if (length(input$module_overview_rows_selected) > 1){
      lapply(current_modules(), MODifieRDB::MODifieR_module_from_db, con = con)
    } else {
      MODifieRDB::MODifieR_module_from_db(module_objects$module_name[input$module_overview_rows_selected], con = con)
    }
    
  }
}

## To be copied in the UI
# mod_module_overview_ui("module_overview_ui_1")

## To be copied in the server
# callModule(mod_module_overview_server, "module_overview_ui_1")
