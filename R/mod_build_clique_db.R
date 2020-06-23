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
  
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
  })
  
  observeEvent(input$build_db, {
    print(input$ppi_choice)
    
    print(input$db_name)
    clique_db <- MODifieRDB::build_clique_db_db(ppi_name = input$ppi_object,
                                              db_folder =  "." , #This should be connected to the db.
                                              db_name = input$db_name,
                                              con = con
                                              )
 
})
}
    
## To be copied in the UI
# mod_build_clique_db_ui("build_clique_db_ui_1")
    
## To be copied in the server
# callModule(mod_build_clique_db_server, "build_clique_db_ui_1")
 
