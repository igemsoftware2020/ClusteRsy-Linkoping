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
    
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks, popup = "Protein-Protein interaction network to overlay the differentially expressed genes on")
  })
  
  
  
  if (nrow(MODifieRDB::get_available_db_networks(con)) != 0 ) {
    output$parameters <- renderUI({
      tagList(
        uiOutput(ns("input_choice")),
        uiOutput(ns("ppi_choice")),
        tags$div(id = "error_name_DiffCoEx_js",
                 textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods")),
        uiOutput(ns("error_name_descrip")),
        uiOutput(ns("error_name_js")),
        sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05, popup ="P-value for cliques to be considered significant"),
        numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2, popup = "Minimal size of cliques"),
        numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0, popup = "Number of iterations to be performed for the permutation based P-value"),
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
        tags$div(id = "error_name_DiffCoEx_js",
                 textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods")),
        uiOutput(ns("error_name_descrip")),
        uiOutput(ns("error_name_js")),
        sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05, popup ="P-value for cliques to be considered significant"),
        numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2, popup = "Minimal size of cliques"),
        numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0, popup = "Number of iterations to be performed for the permutation based P-value"),
        tags$div(style = "text-align:center",
                 actionButton(ns("load_input"), label = "Infer Clique Sum module")
        )
      )
    })
  }
  )
  
  module_name <- reactive({
    input$module_name
  })
  
  observe({
    if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_DiffCoEx_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-DiffCoEx_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_DiffCoEx_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-DiffCoEx_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  }) 
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "The input used for analyzation")
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

