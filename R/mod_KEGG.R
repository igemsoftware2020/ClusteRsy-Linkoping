#' KEGG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_KEGG_ui <- function(id){
  ns <- NS(id)
  tagList(
          uiOutput(ns("module_input")),
          textInput(ns("test"), "skriv något")
  )
}
    
#' KEGG Server Function
#'
#' @noRd 
mod_KEGG_server <- function(input, output, session, con){
  ns <- session$ns
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis.")
  })
 
}
    
## To be copied in the UI
# mod_KEGG_ui("KEGG_ui_1")
    
## To be copied in the server
# callModule(mod_KEGG_server, "KEGG_ui_1")
 
