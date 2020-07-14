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
          plotOutput(ns("cnet_plot"), height = '100vh', width = '100%') %>% shinycssloaders::withSpinner(color="#ffbd40", 
                                                                                                         type = 4,
                                                                                                         size = 0.8),
          downloadButton(ns("download_plot"), 
                         label = "Download Plot")
  )
}
    
#' cnet_plot Server Function
#'
#' @noRd 
mod_cnet_plot_server <- function(input, output, session, cnet_plot_para_ui_1, selected, con){
  ns <- session$ns
  
  cnetplot <- reactive({
    
    enrichment_object <<- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
    enrichment_object_readable <- DOSE::setReadable(enrichment_object, OrgDb = 'org.Hs.eg.db', keyType = "ENTREZID") #Not sure if KeyType should be selected from the enrichment object
    
    p <- try(enrichplot::cnetplot(x = enrichment_object_readable,
                              showCategory = cnet_plot_para_ui_1$showcategory,
                              foldChange = enrichment_object,
                              layout = cnet_plot_para_ui_1$layout,
                              circular = cnet_plot_para_ui_1$circular,
                              colorEdge = cnet_plot_para_ui_1$colorEdge,
                              node_label = cnet_plot_para_ui_1$node_label,
                              title = cnet_plot_para_ui_1$title))
    
    return(p)
  })
  
  output$cnet_plot <- renderPlot({
    cnetplot() #calling the reactive plot
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste(cnet_plot_para_ui_1$title, '.png', sep='') }, 
    content = function(file) {
      ggplot2::ggsave(file, plot = cnetplot(), device = "png")
    }
  )
  
  
  
  
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
 
