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
    tags$div(id = "error_name_Modulediscoverer_js",
    textInput(ns("module_name"), "Module object name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    sliderInput(ns("permutations"), label= "Permutations", min = 0, max = 10000, value = 5000),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff for differentialy expressed genes", min = 0, max = 1, value = 0.05),
    uiOutput(ns("error_p_value")),
    sliderInput(ns("repeats"), label = "Repeats", min = 0, max = 30, value = 15),
    sliderInput(ns("clique_cutoff"), label = "P-value cutoff for significant cliques", min = 0, max = 1, value = 0.01),
    numericInput(ns("n_cores"), label = "N cores", value = 4, max = 10, min = 1),
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer Module discoverer module")
    )
  )
}
    
#' Modulediscoverer Server Function
#'
#' @noRd 
mod_Modulediscoverer_server <- function(input, output, session, con){
  ns <- session$ns
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
  })
 
  module_name <- reactive({
    input$module_name
  })
  
  observe({
    if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_Modulediscoverer_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-Modulediscoverer_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_Modulediscoverer_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-Modulediscoverer_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  })
  
   observeEvent(input$load_input, {
     id <- showNotification("Creating input object", duration = NULL, closeButton = FALSE, type = "warning")
     on.exit(removeNotification(id), add = TRUE)
    output$error_p_value <- NULL # I CANNOT REMOVE THIS BUG, SO THIS IS A FEATURE NOW :)
    module_object <- try(MODifieRDB::modulediscoverer_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          permutations = input$permutations,
                                          deg_cutoff = input$deg_cutoff,
                                          repeats = input$repeats,
                                          clique_cutoff = input$clique_cutoff,
                                          module_name = input$module_name,
                                          n_cores = input$n_cores,
                                          con = con)
                 )
    
    if (class(module_object) == "try-error"){
        output$error_p_value <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), module_object)
        })
      }
    }
  )

}
    
## To be copied in the UI
# mod_Modulediscoverer_ui("Modulediscoverer_ui_1")
    
## To be copied in the server
# callModule(mod_Modulediscoverer_server, "Modulediscoverer_ui_1")
 
