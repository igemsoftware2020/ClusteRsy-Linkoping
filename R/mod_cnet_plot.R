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
                                                                                                         size = 0.8)
  )
}
    
#' cnet_plot Server Function
#'
#' @noRd 
mod_cnet_plot_server <- function(input, output, session, cnet_plot_para_ui_1, selected, con){
  ns <- session$ns
  
  cnetplot <- reactive({
    
    enrichment_object <- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
    enrichment_object_readable <- DOSE::setReadable(enrichment_object, OrgDb = 'org.Hs.eg.db', keyType = "ENTREZID") #Not sure if KeyType should be selected from the enrichment object
    
   input_name <- MODifieRDB::get_input_name_by_enrichment_row(selected$selected_object, con)
   edgeR_deg_table <- MODifieRDB::MODifieR_input_from_db(input_name, con)$edgeR_deg_table
   logFC <- edgeR_deg_table$logFC %>% set_names(. , rownames(edgeR_deg_table))
   
   
   
   
    
    p <- try(enrichplot::cnetplot(x = enrichment_object_readable,
                              showCategory = cnet_plot_para_ui_1$showcategory,
                              foldChange = logFC,
                              layout = cnet_plot_para_ui_1$layout,
                              circular = cnet_plot_para_ui_1$circular,
                              colorEdge = cnet_plot_para_ui_1$colorEdge,
                              node_label = cnet_plot_para_ui_1$node_label,
                              ) + ggplot2::ggtitle(label = cnet_plot_para_ui_1$title))
    
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
 
