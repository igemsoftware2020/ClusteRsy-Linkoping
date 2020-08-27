#' bar_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bar_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("bar_plot"))
  )
}
    
#' bar_plot Server Function
#'
#' @noRd 
mod_bar_plot_server <- function(input, output, session, bar_plot_para_ui_1, selected, con){
  ns <- session$ns
  
  barplot <- reactive({
    
    enrichment_object <- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
    
    p <- enrichplot::barplot(height = enrichment_object,
                             x=bar_plot_para_ui_1$xaxis,
                             showCategory = bar_plot_para_ui_1$showcategory,
                             color = bar_plot_para_ui_1$color,
                             title = bar_plot_para_ui_1$plot_title)
    return(p)
  })
 
  output$bar_plot <- plotly::renderPlotly({
    barplot() #calling the reactive plot
  })
  
}
    
## To be copied in the UI
# mod_bar_plot_ui("bar_plot_ui_1")
    
## To be copied in the server
# callModule(mod_bar_plot_server, "bar_plot_ui_1")
 
