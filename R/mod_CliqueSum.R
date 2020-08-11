#' CliqueSum UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
mod_CliqueSum_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(`data-intro`="You then choose which of your formated input data you want to continue with, your PPI-network and what to name yor module object.", `data-step`=3,
             uiOutput(ns("input_choice")),
            uiOutput(ns("ppi_choice")),
          tags$div(id = "error_name_CliqueSum_js",
          textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods", placeholder = "Module name")),
          uiOutput(ns("error_name_descrip")),
          uiOutput(ns("error_name_js"))
          ),
          
    tags$div(`data-intro`="You can also adjust the parameters of the inference method under Advanced settings", `data-step`=4,
          tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_mod', "href"='#advanced_mod',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
          tags$br(),
          tags$div(id = "advanced_mod", class = "collapse",
                   tags$div(
          sliderInput(ns("clique_significance"), label = "Clique significance", min = 0, max = 1, value = 0.05, popup ="P-value for cliques to be considered significant"),
          numericInput(ns("min_clique_size"), label = "Minimal clique size", value = 2, max = 50, min = 2, popup = "Minimal size of cliques"),
          numericInput(ns("n_iterations"), label = "Iterations", value = 500, max = 10000, min = 0, popup = "Number of iterations to be performed for the permutation based P-value")
                   ))
          ),

          tags$div(style = "text-align:center",
                  actionButton(ns("load_input"), label = "Infer Clique Sum module", onclick="loading_modal_open(); stopWatch()"),
                  htmlOutput(ns("close_loading_modal")) # Close modal with JS
           )
  )
}

#“default”, “primary”, “success”, “info”, “warning”, “danger” btn-block

#' CliqueSum Server Function
#'
#' @noRd 
mod_CliqueSum_server <- function(input, output, session, con, upload_ui_1, input_overview_ui_1, ppi_networks_ui_1){
  ns <- session$ns
  
  CliqueSum_module <- reactiveValues()
  
  module_name <- reactive({
    input$module_name
  })
  
  # observeEvent(input$advanced, {
  #   advanced <-  tags$button(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced',  tags$div(class= "glyphicon glyphicon-menu-down"))
  # })
  
  
  # Check for module object names
  observe({
    if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
      output$error_name_js <- renderUI({

        tags$script(HTML("element = document.getElementById('error_name_CliqueSum_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-CliqueSum_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_CliqueSum_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-CliqueSum_ui_1-load_input').disabled = false;"))

      })
      output$error_name_descrip <- NULL
    }
  }) 
  

  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks, popup = "Protein-Protein interaction network to overlay the differentially expressed genes on")
  })
  
  observeEvent(ppi_networks_ui_1$upload_ppi$upload_ppi, {
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    updateSelectInput(session, "ppi_object", choices = ppi_networks)
  })
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "The input used for analyzation")
  })
  
  observeEvent(c(upload_ui_1$input_name, input_overview_ui_1$delete$delete), {
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    updateSelectInput(session, "input_object", choices = input_objects)
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
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
    if (class(module_object) == "try-error"){
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), module_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    }
    else {
      CliqueSum_module$module_name <- module_name()
      updateTextInput(session, "module_name", value = character(0))
    }
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  
  
  return(CliqueSum_module)
}

## To be copied in the UI
# mod_CliqueSum_ui("CliqueSum_ui_1")

## To be copied in the server
# callModule(mod_CliqueSum_server, "CliqueSum_ui_1")

