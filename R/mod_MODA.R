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
    tags$div(id = "error_name_MODA_js",
    textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods.", placeholder = "Module name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_mod',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_mod", class = "collapse",
             tags$div(
    radioButtons(
      ns("group_of_interest"),
      label = "Group of Interest",
      choices = c( 1, 2),
      selected = 1,
      inline = T,
      popup = "Define the group with the gene of intrest."
    ),
    
    radioButtons(
      ns("cutmethod"),
      label = "Cutmethod",
      choices = c("Density", "Modularity"),
      selected = "Density",
      inline = T,
      popup = "Decide whether the cut in the dendrogram should be calculated by using the maximal average density or the modularity."
    ),
    
    sliderInput(ns("specificTheta"), label = "Select specific theta", min = 0, max = 1, value = 0.5, popup = "The lowest value assumed that can still be considered a condition specific module."),
    sliderInput(ns("conservedTheta"), label = "Select conserved theta", min = 0, max = 1, value = 0.5, popup = "The highest value assumed that can still be considered a condition conserved module."),
             )),
    
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer MODA module", onclick="loading_modal_open(); stopWatch()"),
    htmlOutput(ns("close_loading_modal")),  # Close modal with JS
    htmlOutput((ns("adv_settings")))
    )
  )
  
}
    
#' MODA Server Function
#'
#' @noRd 
mod_MODA_server <- function(input, output, session, con, upload_ui_1, input_overview_ui_1){
  ns <- session$ns
  
  MODA_module <- reactiveValues()
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "The input used for analyzation.")
  })
  
  observeEvent(c(upload_ui_1$input_name, input_overview_ui_1$delete$delete), {
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    updateSelectInput(session, "input_object", choices = input_objects)
  })
  
  module_name <- reactive({
    input$module_name
  })
  
  observe({
    if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_MODA_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MODA_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_MODA_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MODA_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  }) 
  
  
  observeEvent(input$load_input,  {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    output$adv_settings <- renderUI({})
    
    module_object <- try(MODifieRDB::moda_db(input_name = input$input_object, 
                                          group_of_interest = as.numeric(input$group_of_interest),
                                          cutmethod = input$cutmethod,
                                          specificTheta = input$specificTheta,
                                          conservedTheta = input$conservedTheta,
                                          module_name = input$module_name,
                                          con = con)
    )
    
    if (class(module_object) == "try-error"){
      output$adv_settings <- renderUI({
        tags$script("if ($('.collapsible.btn.btn-primary.btn-block').eq(0).attr('aria-expanded') === 'false') {
                            $('.collapsible.btn.btn-primary.btn-block').eq(0).click();
                    }")
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), module_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    }
    else {
      MODA_module$module_name <- module_name()
      updateTextInput(session, "module_name", value = character(0))
    }
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(MODA_module)
}
    
## To be copied in the UI
# mod_MODA_ui("MODA_ui_1")
    
## To be copied in the server
# callModule(mod_MODA_server, "MODA_ui_1")
 
