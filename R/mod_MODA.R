#' MODA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MODA_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    
    radioButtons(
      ns("group_of_intrest"),
      label = "Group of Intrest",
      choices = c( 1, 2),
      selected = 1,
      inline = T,
    ),
    
    radioButtons(
      ns("cutmethod"),
      label = "Cutmethod",
      choices = list("Density" = 1, "Modularity" = 2),
      selected = 1,
      inline = T,
    ),
    
    sliderInput(ns("specificTheta"), label = "Select specific theta", min = 0, max = 1, value = 0.5),
    
    sliderInput(ns("conservedTheta"), label = "Select conserved theta", min = 0, max = 1, value = 0.5),
    
    textInput(ns("module_name"), "Module object name"),
    
    actionButton(ns("load_input"), "Infer MODA module")
  )
  
}
    
#' MODA Server Function
#'
#' @noRd 
mod_MODA_server <- function(input, output, session){
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
    module_object <- MODifieRDB::moda_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          deg_cutoff = .98, 
                                          group_of_interest = input$group_of_intrest,
                                          cutmethod = input$cutmethod,
                                          specificTheta = input$specificTheta,
                                          conservedTheta = input$conservedTheta,
                                          module_name = input$module_name,
                                          con = con)
    
    
  })
}
    
## To be copied in the UI
# mod_MODA_ui("MODA_ui_1")
    
## To be copied in the server
# callModule(mod_MODA_server, "MODA_ui_1")
 
