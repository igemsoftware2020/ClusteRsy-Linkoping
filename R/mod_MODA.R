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
    textInput(ns("module_name"), "Module object name"),
    radioButtons(
      ns("group_of_interest"),
      label = "Group of Interest",
      choices = c( 1, 2),
      selected = 1,
      inline = T,
    ),
    
    radioButtons(
      ns("cutmethod"),
      label = "Cutmethod",
      choices = c("Density", "Modularity"),
      selected = "Density",
      inline = T,
    ),
    
    sliderInput(ns("specificTheta"), label = "Select specific theta", min = 0, max = 1, value = 0.5),
    sliderInput(ns("conservedTheta"), label = "Select conserved theta", min = 0, max = 1, value = 0.5),
    
    actionButton(ns("load_input"), "Infer MODA module")
  )
  
}
    
#' MODA Server Function
#'
#' @noRd 
mod_MODA_server <- function(input, output, session, con){
  ns <- session$ns
 
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  observeEvent(input$load_input, {
    module_object <- MODifieRDB::moda_db(input_name = input$input_object, 
                                          group_of_interest = as.numeric(input$group_of_interest),
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
 
