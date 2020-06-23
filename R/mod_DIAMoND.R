#' DIAMoND UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DIAMoND_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    tags$div(id = "error_name_DIAMoND_js",
    textInput(ns("module_name"), "Module object name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    sliderInput(ns("seed_weight"), label = "Select Seed Weight", min = 0, max = 50, value = 25),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05),
    uiOutput(ns("error_p_value")),
    shinyWidgets::prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning"),
    sliderInput(ns("output_genes"), label= "Select maximum number of genes to be included", min = 0, max = 500, value = 250),
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer DIAMoND module")
    )
  )
}
    
#' DIAMoND Server Function
#'
#' @noRd 
mod_DIAMoND_server <- function(input, output, session, con){
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
        tags$script(HTML("element = document.getElementById('error_name_DIAMoND_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-DIAMoND_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_DIAMoND_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-DIAMoND_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  })
  
  observeEvent(input$load_input, {
    output$error_p_value <- NULL # I CANNOT REMOVE THIS BUG, SO THIS IS A FEATURE NOW :)
    module_object <- try(MODifieRDB::diamond_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          deg_cutoff = input$deg_cutoff,
                                          n_output_genes = input$output_genes,
                                          seed_weight = input$seed_weight,
                                          include_seed = input$include_seed,
                                          module_name = input$module_name,
                                          con = con)
        )
    if (class(module_object) == "try-error"){
      if(grepl("No differentially expressed genes below 0.05",module_object[1])){
        output$error_p_value <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), "Please increase your P-value cutoff")
        })
      }
    }
  })
  observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE
            
    )}
  )
  
}
    
## To be copied in the UI
# mod_DIAMoND_ui("DIAMoND_ui_1")
    
## To be copied in the server
# callModule(mod_DIAMoND_server, "DIAMoND_ui_1")
 
