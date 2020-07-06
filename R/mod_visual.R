#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visual_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("results_ui")),
    DT::dataTableOutput(ns("enrichment_overview")),
    actionButton(ns("refresh"), label = "Refresh"),
    actionButton(ns("analyze"), label = "Analyze")
  )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session, con){
  ns <- session$ns
  
  selected <- reactiveValues()
  
  observeEvent(input$refresh, {
    if(RSQLite::dbExistsTable(con, "enrichment_register")) {
      
      enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
      
      output$enrichment_overview <- DT::renderDataTable({
        styling <- DT:::DT2BSClass(c('compact', 'cell-border', 'hover'))
        DT::datatable(enrichment_objects, 
                      class = styling,
                      selection = "single")
      })
      
    } else {
      return(NULL)
    }
  })
  

  observeEvent(input$analyze, {
    selected$selected_object <- input$enrichment_overview_rows_selected
    output$results_ui <- renderUI({
      tagList(
        mod_enrichment_results_ui(ns("enrichment_results_ui_1"))
      )
    })
    
  })
  
  


  
  callModule(mod_enrichment_results_server, "enrichment_results_ui_1", selected, con = con)
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

