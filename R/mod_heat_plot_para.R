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
    textInput(ns("title"), 
              label = "Title")
  )
}
    
#' heat_plot_para Server Function
#'
#' @noRd 
mod_heat_plot_para_server <- function(input, output, session){
  ns <- session$ns
  heat_plot_para_module <- reactiveValues()
  observe({
    heat_plot_para_module$pathways_displayed <- input$pathways_displayed
    heat_plot_para_module$genes_displayed <- input$genes_displayed
    heat_plot_para_module$pvalue_displayed <- input$pvalue_displayed
    heat_plot_para_module$title <- input$title
  })
  
  return(heat_plot_para_module)
}
    
## To be copied in the UI
# mod_heat_plot_para_ui("heat_plot_para_ui_1")
    
## To be copied in the server
# callModule(mod_heat_plot_para_server, "heat_plot_para_ui_1")
 
