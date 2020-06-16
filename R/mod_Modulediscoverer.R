#' Modulediscoverer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Modulediscoverer_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    textInput(ns("module_name"), "Module object name"), 
    sliderInput(ns("permutations"), label= "Permutations", min = 0, max = 10000, value = 5000),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff for differentialy expressed genes", min = 0, max = 1, value = 0.05),
    sliderInput(ns("repeats"), label = "Repeats", min = 0, max = 30, value = 15),
    sliderInput(ns("clique_cutoff"), label = "P-value cutoff for significant cliques", min = 0, max = 1, value = 0.01),
    actionButton(ns("load_input"), "Infer Module discoverer module"),
    
  )
}
    
#' Modulediscoverer Server Function
#'
#' @noRd 
mod_Modulediscoverer_server <- function(input, output, session){
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
    module_object <- MODifieRDB::modulediscoverer(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          permutations = 100,
                                          deg_cutoff = 0.05,
                                          module_name = input$module_name,
                                          repeats = 3)
    
    
  })
}
    
## To be copied in the UI
# mod_Modulediscoverer_ui("Modulediscoverer_ui_1")
    
## To be copied in the server
# callModule(mod_Modulediscoverer_server, "Modulediscoverer_ui_1")
 
