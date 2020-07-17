#' bar_plot_para UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bar_plot_para_ui <- function(id){
  ns <- NS(id)
  tagList(
    #Parameters
    tags$h3(class = "text-center",
            "Select parameters"),
    #Dot plot
    selectInput(ns("xaxis"),
                label = "X-axis",
                choices = c("GeneRatio", "Count")),
    selectInput(ns("color"),
                label = "Color",
                choices = c("pvalue", "p.adjust", "qvalue")),
    sliderInput(ns("showcategory"), 
                label = "Number of enriched terms to display",
                min = 5,
                max = 50,
                value = 10),
    textInput(ns("title"), 
              label = "Title")
 
  )
}
    
#' bar_plot_para Server Function
#'
#' @noRd 
mod_bar_plot_para_server <- function(input, output, session){
  ns <- session$ns
  bar_plot_para_module <- reactiveValues()
  observe({
    bar_plot_para_module$xaxis <- input$xaxis
    bar_plot_para_module$color <- input$color
    bar_plot_para_module$showcategory <- input$showcategory
    bar_plot_para_module$title <- input$title
  })
  
  return(bar_plot_para_module)
 
}
    
## To be copied in the UI
# mod_bar_plot_para_ui("bar_plot_para_ui_1")
    
## To be copied in the server
# callModule(mod_bar_plot_para_server, "bar_plot_para_ui_1")
 
