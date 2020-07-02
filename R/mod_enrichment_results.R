#' enrichment_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_results_ui <- function(id){
  ns <- NS(id)
  tagList(
 DT::dataTableOutput(ns("enrichment_overview")),
 actionButton(ns("refresh"), label = "Refresh")
  )
}
    
#' enrichment_results Server Function
#'
#' @noRd 
mod_enrichment_results_server <- function(input, output, session, con){
  ns <- session$ns
  observeEvent(input$refresh, {
    if(RSQLite::dbExistsTable(con, "enrichment_register")) {
      #This should be the input from the DT that Lucas is building. Needs to be reactive as well.
      enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
      
      output$enrichment_overview <- DT::renderDataTable({
        DT::datatable(enrichment_objects)
      })
      return(reactive(input$enrichment_overview_rows_selected))
      #selector <- input$enrichment_overview_rows_selected
      
    } else {
      print("Hej")

    }
  })
}
    
## To be copied in the UI
# mod_enrichment_results_ui("enrichment_results_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_server, "enrichment_results_ui_1")
 
