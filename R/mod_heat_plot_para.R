#' heat_plot_para UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_heat_plot_para_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h3(class = "text-center",
            "Parameters"),
    textInput(ns("title"), 
              label = "Title"),
    sliderInput(ns("pathways_displayed"), 
                label = "Number of pathways dispayled",
                min = 5,
                max = 50,
                value = 10),
    sliderInput(ns("genes_displayed"),
                label = "Number of genes displayed",
                min = 5,
                max = 100,
                value = 50),
    prettySwitch(ns("pvalue_displayed"),
                 label = "P-value as color",
                 value = TRUE,
                 status = "warning"),
    tags$div(style = "text-align:center",
             downloadButton(ns("download_graph"), "PDF"))
  )
}
    
#' heat_plot_para Server Function
#'
#' @noRd 
mod_heat_plot_para_server <- function(input, output, session, selected, con){
  ns <- session$ns
  heat_plot_para_module <- reactiveValues()
  observe({
    heat_plot_para_module$pathways_displayed <- input$pathways_displayed
    heat_plot_para_module$genes_displayed <- input$genes_displayed
    heat_plot_para_module$pvalue_displayed <- input$pvalue_displayed
    heat_plot_para_module$title <- input$title
  })
  
  # Download function
  output$download_graph <- downloadHandler(
    filename = function() {
      paste0("heat_plot.pdf", sep="")
    },
    content = function(file) {
      enrichment_object <- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
      p <- gene_heatmap(CPobj = enrichment_object,
                   NP = heat_plot_para_module$pathways_displayed,
                   NG = heat_plot_para_module$genes_displayed,
                   plot_title = heat_plot_para_module$title,
                   pval_color = heat_plot_para_module$pvalue_displayed)
      ggplot2::ggsave(file, plot = p, device = "pdf")
    }
  )
  
  return(heat_plot_para_module)
}
    
## To be copied in the UI
# mod_heat_plot_para_ui("heat_plot_para_ui_1")
    
## To be copied in the server
# callModule(mod_heat_plot_para_server, "heat_plot_para_ui_1")
 
