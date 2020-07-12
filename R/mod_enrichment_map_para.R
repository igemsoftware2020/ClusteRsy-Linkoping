#' enrichment_map_para UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_map_para_ui <- function(id){
  ns <- NS(id)
  tagList(
    #Parameters
    tags$h3(class = "text-center",
            "Select parameters"),
   #Enrichment_map 
    selectInput(ns("color"),
                label = "Color",
                choices = c("pvalue", "p.adjust", "qvalue")),
    sliderInput(ns("showcategory"), 
                label = "Number of enriched terms to display",
                min = 5,
                max = 50,
                value = 10),
    selectInput(ns("layout"),
                label = "Layout",
                choices = c("kk",
                            "nicely")),
   textInput(ns("title"),
             label = "Title")
              
  )
  
}
    
#' enrichment_map_para Server Function
#'
#' @noRd 
mod_enrichment_map_para_server <- function(input, output, session){
  ns <- session$ns
  enrichment_map_para_module <- reactiveValues()
  observe({
    enrichment_map_para_module$color <- input$color
    enrichment_map_para_module$showcategory <- input$showcategory
    enrichment_map_para_module$layout <- input$layout
    enrichment_map_para_module$title <- input$title
  })
  
  return(enrichment_map_para_module)
}
    
## To be copied in the UI
# mod_enrichment_map_para_ui("enrichment_map_para_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_map_para_server, "enrichment_map_para_ui_1")
 
