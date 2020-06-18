#' build_clique_db UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_build_clique_db_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("ppi_choice")),
    textInput(ns("db_name"), "Clique database name"),
    actionButton(ns("build_db"), "Build clique database"),
  )
}
    
#' build_clique_db Server Function
#'
#' @noRd 
mod_build_clique_db_server <- function(input, output, session, con){
  ns <- session$ns
  
  con_clique_db <- MODifieRDB::connect_to_db("./../testdb.db")
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
  })
  
  observeEvent(input$build_db, {
    clique_db <- MODifieRDB::build_clique_db( ppi_network = input$ppi_choice,
                                              db_folder =  con_clique_db, #This should be connected to the db.
                                              db_name = input$db_name,
                                              con = con
                                              )
 
})
}
    
## To be copied in the UI
# mod_build_clique_db_ui("build_clique_db_ui_1")
    
## To be copied in the server
# callModule(mod_build_clique_db_server, "build_clique_db_ui_1")
 
