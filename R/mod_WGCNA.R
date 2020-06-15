#' WGCNA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_WGCNA_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    sliderInput(ns("output_groups"), label= "Select number of groups to be included", min = 0, max = 2, value = 1),
    textInput(ns("module_name"), "Module object name"),
    actionButton(ns("load_input"), "Infer WGCNA trait-based module"),
  )
}
    
#' WGCNA Server Function
#'
#' @noRd 
mod_WGCNA_server <- function(input, output, session){
  ns <- session$ns
 
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  observeEvent(input$load_input, {
    module_object <- MODifieRDB::wgcna_db(input_name = input$input_object, 
                                            n_output_groups = input$output_groups,
                                            module_name = input$module_name)
    
    
  })
  
  
  
}
    
## To be copied in the UI
# mod_WGCNA_ui("WGCNA_ui_1")
    
## To be copied in the server
# callModule(mod_WGCNA_server, "WGCNA_ui_1")
 
