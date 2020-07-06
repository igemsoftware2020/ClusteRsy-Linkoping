#' cnet_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cnet_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
          plotOutput(ns("testplot")),
          mod_enrichment_overview_ui(ns("enrichment_overview_ui_1"))
  )
}
    
#' cnet_plot Server Function
#'
#' @noRd 
mod_cnet_plot_server <- function(input, output, session, con){
  ns <- session$ns
  
  enrichment_overview_ui_1 <- callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1", con = con)
  observeEvent(enrichment_overview_ui_1$enrichment_object, {
    
    enrichment_results <- enrichment_overview_ui_1$enrichment_object@result
    output$testplot <- renderPlot({
    plot(enrichment_result$pvalue)
    })
})
}
    
## To be copied in the UI
# mod_cnet_plot_ui("cnet_plot_ui_1")
    
## To be copied in the server
# callModule(mod_cnet_plot_server, "cnet_plot_ui_1")
 
