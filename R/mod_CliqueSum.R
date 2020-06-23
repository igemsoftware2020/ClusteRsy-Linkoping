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
  
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
  })
  
  
  
  if (nrow(MODifieRDB::get_available_db_networks(con)) != 0 ) {
    output$parameters <- renderUI({
      tagList(
        uiOutput(ns("input_choice")),
        uiOutput(ns("ppi_choice")),
        textInput(ns("module_name"), "Module object name"),
        sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05),
        numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2),
        numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0),
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
    
    
    
    observeEvent(input$build_db, {
      id <- showNotification("Creating clique database", duration = NULL, closeButton = FALSE, type = "warning")
      
      clique_db <- MODifieRDB::build_clique_db_db(ppi_name = input$ppi_object,
                                                  db_folder =  "." , 
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
        uiOutput(ns("ppi_choice")),
        textInput(ns("module_name"), "Module object name"),
        sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.01),
        numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2),
        numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0),
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
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    output$error_p_value <- NULL # I CANNOT REMOVE THIS BUG, SO THIS IS A FEATURE NOW :)
    module_object <- try(MODifieRDB::clique_sum_db(input_name = input$input_object,
                                               ppi_name = input$ppi_object,
                                               n_iterations = input$n_iterations,
                                               clique_significance = input$clique_significance,
                                               min_clique_size = input$min_clique_size,
                                               multiple_cores = FALSE,
                                               n_cores = 1,
                                               module_name = input$module_name,
                                               con = con)
    )
    
    on.exit(removeNotification(id), add = TRUE)
    
    if (class(module_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), module_object)
      })
    }
  })
  
  
}

## To be copied in the UI
# mod_CliqueSum_ui("CliqueSum_ui_1")

## To be copied in the server
# callModule(mod_CliqueSum_server, "CliqueSum_ui_1")

