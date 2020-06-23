#' MCODE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MCODE_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    tags$div(id = "error_name_MCODE_js",
    textInput(ns("module_name"), "Module object name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    radioButtons(
        ns("hierarchy"),
        label = "Hierarchy",
        choices= c(1, 2, 3),
        selected = 1,
        inline = T
    ),
    sliderInput(
      ns("vwp"),
      label = "Vertex weight percentage",
      min = 0.0,
      max = 1.0,
      value = 0.5,
      step = 0.01,
      round = T,
      ticks = T
    ),
    sliderInput(
      ns("fdt"),
      label = "Clust density cutoff",
      min = 0.0,
      max = 1.0,
      value = 0.5,
      step = 0.01,
      round = T,
      ticks = T
    ),
    sliderInput(
      ns("deg_cutoff"),
      label = "P-value cutoff",
      min = 0.0,
      max = 1.0,
      value = 0.05,
      step = 0.01,
      round = T,
      ticks = T
    ),
    sliderInput(
      ns("module_cutoff"),
      label = "Minimal score for a module to be returned",
      min = 0.0,
      max = 1.0,
      value = 0.5,
      step = 0.01,
      round = T,
      ticks = T
    ),
    shinyWidgets::prettySwitch(
      ns("haircut"),
      label = "Haircut",
      value = FALSE,
      status = "warning"
    ),
    shinyWidgets::prettySwitch(
      ns("fluff"),
      label = "Fluff",
      value = FALSE,
      status = "warning"
    ),
    shinyWidgets::prettySwitch(
      ns("loops"),
      label = "Loops",
      value = FALSE,
      status = "warning"
    ),
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer MCODE module")
    )
  )
}
    
#' MCODE Server Function
#'
#' @noRd 
mod_MCODE_server <- function(input, output, session, con){
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
        tags$script(HTML("element = document.getElementById('error_name_MCODE_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MCODE_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_MCODE_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MCODE_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  })
  
  observeEvent(input$load_input, {
    module_object <- MODifieRDB::mcode_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          hierarchy = as.numeric(input$hierarchy),
                                          vwp = input$vwp,
                                          haircut = input$haircut,
                                          fdt = input$fdt,
                                          loops = input$loops,
                                          module_cutoff = input$module_cutoff,
                                          deg_cutoff = input$deg_cutoff,
                                          module_name = input$module_name,
                                          con = con)
    
    
  })
 
   observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE
            
    )}
  )
  
}
    
## To be copied in the UI
# mod_MCODE_ui("MCODE_ui_1")
    
## To be copied in the server
# callModule(mod_MCODE_server, "MCODE_ui_1")
 
