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
    plotOutput(ns("dot_plot"), height = '100vh', width = '100%') %>% shinycssloaders::withSpinner(color="#ffbd40", 
                                                                                                  type = 4,
                                                                                                  size = 0.8)
  )
}
    
#' dot_plot Server Function
#'
#' @noRd 
mod_dot_plot_server <- function(input, output, session, dot_plot_para_ui_1, selected, con){
  ns <- session$ns
 
    dotplot <- reactive({
      
    enrichment_object <- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
      
    p <- try(clusterProfiler::dotplot(enrichment_object,
                                  x=dot_plot_para_ui_1$xaxis,
                                  showCategory = dot_plot_para_ui_1$showcategory,
                                  color = dot_plot_para_ui_1$color,
                                  title = dot_plot_para_ui_1$title))
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
 
