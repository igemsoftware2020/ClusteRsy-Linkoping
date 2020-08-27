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
            "Parameters"),
   #Enrichment_map 
   textInput(ns("title"),
             label = "Title"),
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
   tags$div(style = "text-align:center",
            downloadButton(ns("download_graph"), "PDF"))
              
  )
  
}
    
#' enrichment_map_para Server Function
#'
#' @noRd 
mod_enrichment_map_para_server <- function(input, output, session, selected, con){
  ns <- session$ns
  enrichment_map_para_module <- reactiveValues()
  observe({
    enrichment_map_para_module$color <- input$color
    enrichment_map_para_module$showcategory <- input$showcategory
    enrichment_map_para_module$layout <- input$layout
    enrichment_map_para_module$title <- input$title
  })
  
  # Download function
  output$download_graph <- downloadHandler(
    filename = function() {
      paste0("enrichment_map_plot.pdf")
    },
    content = function(file) {
      erichment_object <- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
      p <-enrichplot::emapplot(x = enrichment_object,
                          showCategory = enrichment_map_para_module$showcategory,
                          color = enrichment_map_para_module$color,
                          layout = enrichment_map_para_module$layout
      ) + ggplot2::ggtitle(enrichment_map_para_module$title)
      ggplot2::ggsave(file, plot = p, device = "pdf")
    }
  )
  
  return(enrichment_map_para_module)
}
    
## To be copied in the UI
# mod_enrichment_map_para_ui("enrichment_map_para_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_map_para_server, "enrichment_map_para_ui_1")
 
