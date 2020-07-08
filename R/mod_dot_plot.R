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
mod_dot_plot_server <- function(input, output, session, parameters, selected, con){
  ns <- session$ns
 
#Make the plot reactive with parameters() from visual module
    dotplot <- reactive({
      
      enrichment_object <<- MODifieRDB::enrichment_object_from_db(con,
                                                                  selected$selected_object)
      
     p <-clusterProfiler::dotplot(enrichment_object,
                              x =parameters$xaxis(),
                               showCategory = parameters$showcategory(),
                               color = parameters$color(),
                              title = parameters$plot_title())
     return(p)
    })
  
    output$dot_plot <- renderPlot({
      dotplot() #calling the reactive plot
    })
}
    
## To be copied in the UI
# mod_dot_plot_ui("dot_plot_ui_1")
    
## To be copied in the server
# callModule(mod_dot_plot_server, "dot_plot_ui_1")
 
