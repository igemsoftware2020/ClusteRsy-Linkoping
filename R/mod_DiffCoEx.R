#' DiffCoEx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DiffCoEx_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    textInput(ns("module_name"), "Module object name"),
    actionButton(ns("load_input"), "Infer DiffCoEx module"),
 
  )
}
    
#' DiffCoEx Server Function
#'
#' @noRd 
mod_DiffCoEx_server <- function(input, output, session){
  ns <- session$ns
 
   output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
  })
  observeEvent(input$load_input, {
    module_object <- MODifieRDB::diffcoex(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          deg_cutoff = .98, 
                                          module_name = input$module_name,
                                          con = con)
    
    
  })
}
    
## To be copied in the UI
# mod_DiffCoEx_ui("DiffCoEx_ui_1")
    
## To be copied in the server
# callModule(mod_DiffCoEx_server, "DiffCoEx_ui_1")
 
