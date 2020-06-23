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
    textInput(ns("module_name"), "Module object name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
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
    
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer MODA module")
    )
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
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_MODA_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MODA_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
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
 
