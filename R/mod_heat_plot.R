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
                                                                                                             size = 0.8)
  )
}
    
#' heat_plot Server Function
#'
#' @noRd 
mod_heat_plot_server <- function(input, output, session, heat_plot_para_ui_1, selected, con){
  ns <- session$ns
  
  heatplot <- reactive({
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    enrichment_object <- MODifieRDB::enrichment_object_from_db(enrichment_objects$enrichment_name[selected$selected_object], con)

    #gene_heatmap can be found within fct_functions.R     
    p <- try(gene_heatmap(CPobj = enrichment_object,
                      NP = heat_plot_para_ui_1$pathways_displayed,
                      NG = heat_plot_para_ui_1$genes_displayed,
                      plot_title = heat_plot_para_ui_1$title,
                      pval_color = heat_plot_para_ui_1$pvalue_displayed))
    if (class(p)[1] == "try-error"){
      NULL
      }
    else {
      return(p) 
    }
  }) 

  output$heat_plot <- renderPlot({
    heatplot() 
  })
}
    
## To be copied in the UI
# mod_heat_plot_ui("heat_plot_ui_1")
    
## To be copied in the server
# callModule(mod_heat_plot_server, "heat_plot_ui_1")
 
