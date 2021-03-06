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
    tags$div(id = "error_name_CClique_js",
    textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods", placeholder = "Module name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_mod',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_mod", class = "collapse",
             tags$div(
    sliderInput(ns("frequency_cutoff"), label = "Select frequency cutoff", min = 0, max = 1, value = 0.5, popup = "Fraction of the number of times a gene should be present in the iterations"),
    sliderInput(ns("fraction_of_interactions"), label = "Fraction of interactions", min = 0, max = 1, value = 0.4, popup = "Fraction of interactions from the original network that will be used in each iteration"),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05, popup = "P-value cutoff for differentially expressed genes"),
    uiOutput(ns("error_p_value")),
    sliderInput(ns("clique_significance"), label = "Cutoff for Fisher exact test for cliques", min = 0, max = 1, value = 0.05, popup = "Cutoff for Fisher exact test for cliques"),
    prettySwitch(ns("to_db"), label = "Save result in database", value = TRUE, status = "warning", popup = "Save the results in the local database"),
    numericInput(ns("iteration"), label = "Number of iterations", value = 50, max = 100, min = 0, popup = "Number of iterations performed"),
    numericInput(ns("n_cores"), label = "Number of cores", value = 3, max = 50, min = 0, popup = "If one parallellizes iteratios how many cores will the process be run on"),
    prettySwitch(ns("multiple_cores"), label = "Parallellize iterations", value = TRUE, status = "warning", popup = "Should the process run parallel using multiple CPU cores?"),
             )),
             
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer Correlation clique module", onclick="loading_modal_open(); stopWatch()"),
    htmlOutput(ns("close_loading_modal")), # Close modal with JS
    htmlOutput((ns("adv_settings")))
    )
  )
}
    
#' CClique Server Function
#'
#' @noRd 
mod_CClique_server <- function(input, output, session, con, upload_ui_1, input_overview_ui_1, ppi_networks_ui_1){
  ns <- session$ns
 
  CClique_module <- reactiveValues()
  x <- reactiveVal(1)  # Reactive value to record if the input buttion is pressed
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "The input used for analyzation")
  })
   
  observeEvent(c(upload_ui_1$input_name, input_overview_ui_1$value$delete, input_overview_ui_1$value$upload), {
     input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
     updateSelectInput(session, "input_object", choices = input_objects)
   })
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks, popup = "Protein-Protein interaction network to overlay the differentially expressed genes on")
  })
  
  observeEvent(ppi_networks_ui_1$upload_ppi$upload_ppi, {
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    updateSelectInput(session, "ppi_object", choices = ppi_networks)
  })
  
  module_name <- reactive({
    input$module_name
  })
  
  observe({
    if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_CClique_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-CClique_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_CClique_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-CClique_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    output$error_p_value <- NULL
    output$adv_settings <- renderUI({})
    
    module_object <- try(MODifieRDB::correlation_clique_db(input_name = input$input_object, 
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
    
    
    if (class(module_object) == "try-error"){
      output$adv_settings <- renderUI({
        tags$script("if ($('.collapsible.btn.btn-primary.btn-block').eq(0).attr('aria-expanded') === 'false') {
                            $('.collapsible.btn.btn-primary.btn-block').eq(0).click();
                    }")
      })
      if (grepl("Name", module_object)) {
        
        output$error_name_descrip <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), module_object,
                 style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
        })
      } else {
        output$error_p_value <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), module_object,
                 style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
        })
      }
    } else {
      x(x() + 1)
      CClique_module$infer <- x()
      updateTextInput(session, "module_name", value = character(0))
    }
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(CClique_module)
}
    
## To be copied in the UI
# mod_CClique_ui("CClique_ui_1")
    
## To be copied in the server
# callModule(mod_CClique_server, "CClique_ui_1")
 
