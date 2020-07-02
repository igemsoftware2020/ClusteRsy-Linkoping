#' enrichment_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("enrichment_overview")),
    actionButton(ns("refresh"), label = "Refresh"),
    actionButton(ns("analyze"), label = "Analyze")
  )
}
    
#' enrichment_overview Server Function
#'
#' @noRd 
mod_enrichment_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
  enrichment_object <- reactiveValues()
  
  observeEvent(input$refresh, {
    if(RSQLite::dbExistsTable(con, "enrichment_register")) {
      #This should be the input from the DT that Lucas is building. Needs to be reactive as well.
      enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
      
      output$enrichment_overview <- DT::renderDataTable({
        DT::datatable(enrichment_objects)
      })

      
    } else {
      
      
    }
  })
  
  observeEvent(input$analyze, {
    enrichment_object$enrichment_object <- MODifieRDB::enrichment_object_from_db(con,
                                                                                   input$enrichment_overview_rows_selected
                                                                                 )
  })
  
  return(enrichment_object)
}
    
## To be copied in the UI
# mod_enrichment_overview_ui("enrichment_overview_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1")
 
