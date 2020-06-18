#' CliqueSum UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CliqueSum_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    textInput(ns("module_name"), "Module object name"),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff for DEGs", min = 0, max = 1, value = 0.05),
    sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05),
    numericInput(ns("n_cores"), label = "Number of cores", value = 3, max = 50, min = 0),
    numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2),
    prettySwitch(ns("multiple_cores"), label = "Parallellize iterations", value = TRUE, status = "warning"),
    numericInput(ns("n_iterations"), label = "Iterations", value = , max = 10000, min = 0),
    prettySwitch(ns("to_db"), label = "Save result in database", value = TRUE, status = "warning"),
  )
}
    
#' CliqueSum Server Function
#'
#' @noRd 
mod_CliqueSum_server <- function(input, output, session){
  ns <- session$ns
 
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  observeEvent(input$load_input, {
    module_object <- MODifieRDB::DiffCoEx(input_name = input$input_object,
                                          ppi_name = input$ppi_object,
                                          n_iterations = input$n_iterations,
                                          deg_cutoff = input$deg_cutoff,
                                          clique_significance = input$clique_significance,
                                          min_clique_size = input$min_clique_size,
                                          multiple_cores = input$multiple_cores,
                                          n_cores = input$n_cores,
                                          module_name = input$module_name,
                                          to_db = input$to_db,
                                          con = con)
                                          
  })
}
    
## To be copied in the UI
# mod_CliqueSum_ui("CliqueSum_ui_1")
    
## To be copied in the server
# callModule(mod_CliqueSum_server, "CliqueSum_ui_1")
 
