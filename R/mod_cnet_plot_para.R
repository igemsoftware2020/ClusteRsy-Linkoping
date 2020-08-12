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
            "Parameters"),
    #Cnet plot
    textInput(ns("title"), 
              label = "Title"),
    sliderInput(ns("showcategory"), 
                label = "Number of enriched terms to display",
                min = 3,
                max = 15,
                value = 5),
    selectInput(ns("layout"),
                label = "Layout",
                choices = c("kk", "nicely")),
    prettySwitch(ns("circular"), 
                 label = "Circular formation", 
                 value = FALSE, 
                 status = "warning"),
    prettySwitch(ns("coloredge"), 
                 label = "Add color", 
                 value = FALSE, 
                 status = "warning"),
    selectInput(ns("node_label"),
                label = "Node labels",
                choices = c("all", "gene", "category", "none")),
    tags$div(style = "text-align:center",
             downloadButton(ns("download_graph"), "PDF"))
  )
}

#' cnet_plot_para Server Function
#'
#' @noRd 
mod_cnet_plot_para_server <- function(input, output, session, selected, con){
  ns <- session$ns
  
  cnet_plot_para_module <- reactiveValues()
  observe({
    cnet_plot_para_module$showcategory <- input$showcategory
    cnet_plot_para_module$layout <- input$layout
    cnet_plot_para_module$circular <- input$circular
    cnet_plot_para_module$colorEdge <- input$coloredge
    cnet_plot_para_module$node_label <- input$node_label
    cnet_plot_para_module$title <- input$title
  })
  
  # Download function
  output$download_graph <- downloadHandler(
    filename = function() {
      paste0("cnet_plot.pdf", sep="")
    },
    content = function(file) {
      enrichment_object <<- MODifieRDB::enrichment_object_from_db(selected$selected_object, con)
      enrichment_object_readable <- DOSE::setReadable(enrichment_object, OrgDb = 'org.Hs.eg.db', keyType = "ENTREZID")
      p <- enrichplot::cnetplot(x = enrichment_object_readable,
                           showCategory = cnet_plot_para_module$showcategory,
                           foldChange = enrichment_object,
                           layout = cnet_plot_para_module$layout,
                           circular = cnet_plot_para_module$circular,
                           colorEdge = cnet_plot_para_module$colorEdge,
                           node_label = cnet_plot_para_module$node_label,
      ) + ggplot2::ggtitle(label = cnet_plot_para_module$title)
      ggplot2::ggsave(file, plot = p, device = "pdf")
    }
  )
  
  return(cnet_plot_para_module)
}

## To be copied in the UI
# mod_cnet_plot_para_ui("cnet_plot_para_ui_1")

## To be copied in the server
# callModule(mod_cnet_plot_para_server, "cnet_plot_para_ui_1")