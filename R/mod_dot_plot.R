#' dot_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dot_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
 plotOutput(ns("dot_plot"))
  )
}
    
#' dot_plot Server Function
#'
#' @noRd 
mod_dot_plot_server <- function(input, output, session, selected, con){
  ns <- session$ns
 
  output$dot_plot <- renderPlot({
    enrichment_object <<- MODifieRDB::enrichment_object_from_db(con,
                                                               selected$selected_object
    )
   
     clusterProfiler::dotplot(enrichment_object,
                               showCategory = 10,
                               color = "pvalue")
  })
}
    
## To be copied in the UI
# mod_dot_plot_ui("dot_plot_ui_1")
    
## To be copied in the server
# callModule(mod_dot_plot_server, "dot_plot_ui_1")
 
