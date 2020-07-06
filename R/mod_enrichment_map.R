#' enrichment_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_map_ui <- function(id){
  ns <- NS(id)
  tagList(
 plotOutput(ns("enrichment_plot")),
 # selectInput(ns("color"), 
 #             choices = c())
  )
}
    
#' enrichment_map Server Function
#'
#' @noRd 
mod_enrichment_map_server <- function(input, output, session, selected, con){
  ns <- session$ns
  
  
  
  output$enrichment_plot <- renderPlot({
    enrichment_object <- MODifieRDB::enrichment_object_from_db(con,
                                                               selected$selected_object
    )
    
    enrichplot::emapplot(enrichment_object, showCategory = 30, color = "p.adjust",  layout = "kk")
  })
}
    
## To be copied in the UI
# mod_enrichment_map_ui("enrichment_map_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_map_server, "enrichment_map_ui_1")
 
