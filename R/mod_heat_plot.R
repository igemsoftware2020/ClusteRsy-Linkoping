#' heat_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_heat_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("heat_plot"), height = '100vh', width = '100%') %>% shinycssloaders::withSpinner(color="#ffbd40", 
                                                                type = 4,
                                                                size = 0.8),
    downloadButton(ns("download_plot"), 
                   label = "Download Plot")
 
  )
}
    
#' heat_plot Server Function
#'
#' @noRd 
mod_heat_plot_server <- function(input, output, session, heat_plot_para_ui_1, selected, con){
  ns <- session$ns
  
  heatplot <- reactive({
    
    enrichment_object <<- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
    
    p <- try(clusterProfiler::heatplot(
                                      x = enrichment_object,
                                      showCategory = heat_plot_para_ui_1$showcategory
                                      ) + ggplot2::ggtitle(label = heat_plot_para_ui_1$title)
             )
    return(p)
  })
  
  output$heat_plot <- renderPlot({
    heatplot() 
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste(heat_plot_para_ui_1$title, '.png', sep='') }, 
    content = function(file) {
      ggplot2::ggsave(file, plot = heatplot(), device = "png")
    }
  )
 
}
    
## To be copied in the UI
# mod_heat_plot_ui("heat_plot_ui_1")
    
## To be copied in the server
# callModule(mod_heat_plot_server, "heat_plot_ui_1")
 
