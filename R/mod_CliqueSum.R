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
        uiOutput(ns("parameters")),
        uiOutput(ns("build_clique"))
  )
}
    
#' CliqueSum Server Function
#'
#' @noRd 
mod_CliqueSum_server <- function(input, output, session, con){
  ns <- session$ns
  
if (nrow(MODifieRDB::get_available_db_networks(con)) != 0 ) {
  
  output$parameters <- renderUI({
    tagList(
      uiOutput(ns("input_choice")),
      textInput(ns("module_name"), "Module object name"),
      sliderInput(ns("deg_cutoff"), label = "P-value cutoff for DEGs", min = 0, max = 1, value = 0.05),
      sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05),
      numericInput(ns("n_cores"), label = "Number of cores", value = 3, max = 50, min = 0),
      numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2),
      shinyWidgets::prettySwitch(ns("multiple_cores"), label = "Multiple cores", value = TRUE, status = "warning"),
      numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0),
      shinyWidgets::prettySwitch(ns("to_db"), label = "Save result in database", value = TRUE, status = "warning"),
      tags$div(style = "text-align:center",
      actionButton(ns("load_input"), label = "Infer Clique Sum module")
      )
    )
  })
} else {

    output$build_clique <- renderUI({
      tagList(
        uiOutput(ns("ppi_choice")),
        textInput(ns("db_name"), "Clique database name"),
        actionButton(ns("build_db"), "Build clique database"),
      )
    })
    
    output$ppi_choice <- renderUI({
      ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
      
      selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
    })
    
    observeEvent(input$build_db, {
      id <- showNotification("Creating clique database", duration = NULL, closeButton = FALSE)
      
      clique_db <- MODifieRDB::build_clique_db_db(ppi_name = input$ppi_object,
                                                  db_folder =  "." , #This should be connected to the db.
                                                  db_name = input$db_name,
                                                  con = con
      )
      on.exit(removeNotification(id), add = TRUE)
      
    })
    
}

  observeEvent(input$build_db, {
    output$build_clique <- NULL
    output$parameters <- renderUI({
      tagList(
        uiOutput(ns("input_choice")),
        textInput(ns("module_name"), "Module object name"),
        sliderInput(ns("deg_cutoff"), label = "P-value cutoff for DEGs", min = 0, max = 1, value = 0.05),
        sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05),
        numericInput(ns("n_cores"), label = "Number of cores", value = 3, max = 50, min = 0),
        numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2),
        shinyWidgets::prettySwitch(ns("multiple_cores"), label = "Multiple cores", value = TRUE, status = "warning"),
        numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0),
        shinyWidgets::prettySwitch(ns("to_db"), label = "Save result in database", value = TRUE, status = "warning"),
        actionButton(ns("load_input"), label = "Infer Clique Sum module")
      )
    })
  }
  )
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  observeEvent(input$load_input, {
    module_object <- MODifieRDB::clique_sum_db(input_name = input$input_object,
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
 
