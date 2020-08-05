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
    
    input_data_name <- MODifieRDB::get_input_name_by_enrichment_row(selected$selected_object, con)
    input_data <<- MODifieRDB::MODifieR_input_from_db(input_data_name, con)
    
    foldchange <- input_data$edgeR_deg_table$logFC
    names(foldchange) <- row.names(input_data$edgeR_deg_table) 
    
    if (cnet_plot_para_ui_1$foldchange == TRUE) {
    p <- try(enrichplot::cnetplot(x = enrichment_object_readable,
                              showCategory = cnet_plot_para_ui_1$showcategory,
                              foldChange = foldchange,
                              layout = cnet_plot_para_ui_1$layout,
                              circular = cnet_plot_para_ui_1$circular,
                              colorEdge = cnet_plot_para_ui_1$colorEdge,
                              node_label = cnet_plot_para_ui_1$node_label,
                              ) + ggplot2::ggtitle(label = cnet_plot_para_ui_1$title))
    
    return(p)
    } else {
      p <- try(enrichplot::cnetplot(x = enrichment_object_readable,
                                    showCategory = cnet_plot_para_ui_1$showcategory,
                                    layout = cnet_plot_para_ui_1$layout,
                                    circular = cnet_plot_para_ui_1$circular,
                                    colorEdge = cnet_plot_para_ui_1$colorEdge,
                                    node_label = cnet_plot_para_ui_1$node_label,
      ) + ggplot2::ggtitle(label = cnet_plot_para_ui_1$title))
      return(p)
    }
    
  })
  
  output$cnet_plot <- renderPlot({
    cnetplot() #calling the reactive plot
  })
  
}
    
## To be copied in the UI
# mod_cnet_plot_ui("cnet_plot_ui_1")
    
## To be copied in the server
# callModule(mod_cnet_plot_server, "cnet_plot_ui_1")
 
