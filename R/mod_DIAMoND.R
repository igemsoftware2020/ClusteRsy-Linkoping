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
    textInput(ns("module_name"), "Module object name"),
    sliderInput(ns("seed_weight"), label = "Select Seed Weight", min = 0, max = 50, value = 25),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05),
    uiOutput(ns("error")),
    shinyWidgets::prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning"),
    sliderInput(ns("output_genes"), label= "Select maximum number of genes to be included", min = 0, max = 500, value = 250),
    actionButton(ns("load_input"), "Infer DIAMoND module"),
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
  observeEvent(input$load_input, {
    output$error <- NULL # I CANNOT REMOVE THIS BUG, SO THIS IS A FEATURE NOW :)
    error <- try(module_object <- MODifieRDB::diamond_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          deg_cutoff = input$deg_cutoff,
                                          n_output_genes = input$output_genes,
                                          seed_weight = input$seed_weight,
                                          include_seed = input$include_seed,
                                          module_name = input$module_name,
                                          con = con)
        )
    if (class(error) == "try-error"){
      output$error <- renderUI({
        tags$p(style = "color:red;", tags$b("Error:"), "Please increase your P-value cutoff")
      })
    }
    
  })
 
  
}
    
## To be copied in the UI
# mod_DIAMoND_ui("DIAMoND_ui_1")
    
## To be copied in the server
# callModule(mod_DIAMoND_server, "DIAMoND_ui_1")
 
