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
    radioButtons(
        ns("hierarchy"),
        label = "Hierarchy",
        choices = c(0, 1, 2),
        selected = 1,
        inline = T,
    ),
    textInput(ns("module_name"), "Module object name"),
    actionButton(ns("load_input"), "Infer MCODE module"),
  )
}
    
#' MCODE Server Function
#'
#' @noRd 
mod_MCODE_server <- function(input, output, session){
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
    module_object <- MODifieRDB::mcode_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          hierarchy = input$hierarchy,
                                          vwp = input$vwp,
                                          haircut = input$haircut,
                                          fdt = input$fdt,
                                          loops = input$loops,
                                          module_cutoff = input$module_cutoff,
                                          deg_cutoff = input$deg_cutoff, 
                                          module_name = input$module_name,
                                          con = con)
    
    
  })
}
    
## To be copied in the UI
# mod_MCODE_ui("MCODE_ui_1")
    
## To be copied in the server
# callModule(mod_MCODE_server, "MCODE_ui_1")
 
