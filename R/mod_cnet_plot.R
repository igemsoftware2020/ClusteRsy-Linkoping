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
          plotOutput(ns("cnet_plot")),
          #mod_enrichment_overview_ui(ns("enrichment_overview_ui_1"))
  )
}
    
#' cnet_plot Server Function
#'
#' @noRd 
mod_cnet_plot_server <- function(input, output, session, cnet_plot_para_ui_1, selected, con){
  ns <- session$ns
  
  cnetplot <- reactive({
    
    enrichment_object <<- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
    enrichment_result <- enrichment_object@result
    print(head(enrichment_result))
   # enrichment_objectx <- DOSE::setReadable(enrichment_result, org.Hs.eg.db::org.Hs.eg.db, 'ENTREZID')
    #print(head(enrichment_objectx))
    p <- enrichplot::cnetplot.enrichResult(x=enrichment_result,
                                  showCategory = cnet_plot_para_ui_1$showcategory,
                                  layout = cnet_plot_para_ui_1$layout,
                                  node_label = cnet_plot_para_ui_1$node_label)
    return(p)
  })
  
  output$cnet_plot <- renderPlot({
    cnetplot() #calling the reactive plot
  })
  
  
  
  
  
#   enrichment_overview_ui_1 <- callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1", con = con)
#   observeEvent(enrichment_overview_ui_1$enrichment_object, {
#     
#     enrichment_results <- enrichment_overview_ui_1$enrichment_object@result
#     output$testplot <- renderPlot({
#     plot(enrichment_result$pvalue)
#     })
# })
}
    
## To be copied in the UI
# mod_cnet_plot_ui("cnet_plot_ui_1")
    
## To be copied in the server
# callModule(mod_cnet_plot_server, "cnet_plot_ui_1")
 
