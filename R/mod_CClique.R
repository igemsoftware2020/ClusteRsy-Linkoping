#' CClique UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CClique_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    textInput(ns("module_name"), "Module object name"),
    sliderInput(ns("frequency_cutoff"), label = "Select frequency cutoff", min = 0, max = 100, value = 50),
    sliderInput(ns("fraction_of_interactions"), label = "Fraction of interactions", min = 0, max = 1, value = 0.4),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05),
    sliderInput(ns("clique_significance"), label = "Cutoff for Fisher exact test for cliques", min = 0, max = 1, value = 0.05),
    shinyWidgets::prettySwitch(ns("to_db"), label = "Save result in database", value = TRUE, status = "warning"),
    numericInput(ns("iteration"), label = "Number of iterations", value = 50, max = 100, min = 0),
    numericInput(ns("n_cores"), label = "Number of cores", value = 3, max = 50, min = 0),
    shinyWidgets::prettySwitch(ns("multiple_cores"), label = "Parallellize iterations", value = TRUE, status = "warning"),
    actionButton(ns("load_input"), "Infer Correlation clique module"),
    
  )
}
    
#' CClique Server Function
#'
#' @noRd 
mod_CClique_server <- function(input, output, session, con){
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
    error <- try(module_object <- MODifieRDB::correlation_clique_db(input_name = input$input_object, 
                                            ppi_name = input$ppi_object, 
                                            frequency_cutoff = input$frequency_cutoff,
                                            to_db = input$to_db,
                                            fraction_of_interactions = input$fraction_of_interactions,
                                            deg_cutoff = input$deg_cutoff,
                                            iteration = input$iteration,
                                            n_cores = input$n_cores,
                                            multiple_cores = input$multiple_cores,
                                            clique_significance = input$clique_significance,
                                            module_name = input$module_name,
                                            con = con)
    )
    
    if (class(error) == "try-error"){
      print("Please increase your P-value cutoff")
    }
    
  })
}
    
## To be copied in the UI
# mod_CClique_ui("CClique_ui_1")
    
## To be copied in the server
# callModule(mod_CClique_server, "CClique_ui_1")
 
