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
    textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods.")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
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
  
  MODA_module <- reactiveValues()
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "The input used for analyzation.")
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
  
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating input object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module_object <- try(MODifieRDB::moda_db(input_name = input$input_object, 
                                          group_of_interest = as.numeric(input$group_of_interest),
                                          cutmethod = input$cutmethod,
                                          specificTheta = input$specificTheta,
                                          conservedTheta = input$conservedTheta,
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
      MODA_module$module_object <- module_object
      updateTextInput(session, "module_name", value = character(0))
    }
  })
  return(MODA_module)
}
    
## To be copied in the UI
# mod_MODA_ui("MODA_ui_1")
    
## To be copied in the server
# callModule(mod_MODA_server, "MODA_ui_1")
 