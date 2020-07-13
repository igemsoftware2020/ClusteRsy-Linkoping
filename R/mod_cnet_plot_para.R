#' cnet_plot_para UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cnet_plot_para_ui <- function(id){
  ns <- NS(id)
  tagList(
    #Parameters
    tags$h3(class = "text-center",
            "Select parameters"),
    #Cnet plot
    sliderInput(ns("showCategory"), 
                label = "Number of enriched terms to display",
                min = 5,
                max = 50,
                value = 5),
    selectInput(ns("layout"),
                label = "Layout",
                choices = c("kk", "nicely")),
    selectInput(ns("node_label"),
                label = "Node labels",
                choices = c("all", "gene", "category", "none")),
    
  )
}

#' cnet_plot_para Server Function
#'
#' @noRd 
mod_cnet_plot_para_server <- function(input, output, session){
  ns <- session$ns
  cnet_plot_para_module <- reactiveValues()
  observe({
    cnet_plot_para_module$showCategory <- input$showCategory
    cnet_plot_para_module$layout <- input$layout
    cnet_plot_para_module$node_label <- input$node_label
  })
  
  return(cnet_plot_para_module)
}

## To be copied in the UI
# mod_cnet_plot_para_ui("cnet_plot_para_ui_1")

## To be copied in the server
# callModule(mod_cnet_plot_para_server, "cnet_plot_para_ui_1")