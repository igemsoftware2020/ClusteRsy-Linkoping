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
    plotOutput(ns("enrichment_map")) %>% shinycssloaders::withSpinner(color= "#ffbd40", 
                                                     type = 4,
                                                     size = 0.8)
  )
}
    
#' enrichment_map Server Function
#'
#' @noRd 
mod_enrichment_map_server <- function(input, output, session, enrichment_map_para_ui_1, selected, con){
  ns <- session$ns
  
  
  enrichment_map <- reactive({
    
    enrichment_object <- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
    
    p <- try(enrichplot::emapplot(x = enrichment_object,
                              showCategory = enrichment_map_para_ui_1$showcategory,
                              color = enrichment_map_para_ui_1$color,
                              layout = enrichment_map_para_ui_1$layout
                              ) + ggplot2::ggtitle(enrichment_map_para_ui_1$title))
                              
    return(p)
  })
  
  output$enrichment_map <- renderPlot({
    enrichment_map() #calling the reactive plot
  })
  
}
    
## To be copied in the UI
# mod_enrichment_map_ui("enrichment_map_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_map_server, "enrichment_map_ui_1")
 
