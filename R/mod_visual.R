#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visual_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("results_ui")),
    absolutePanel(
      bottom = 20, right = 20, width = "35%", height = "60%",
      draggable = TRUE,
      wellPanel(
        HTML(markdownToHTML(fragment.only=TRUE, text=c(
          "Please choose your enrichment analysis object for visualization.

You can drag this panel around."


        ))),
        tags$form(`class` = "well", 
                  style = "background-color:#FFFFFF;",
    DT::dataTableOutput(ns("enrichment_overview"), width = "auto", height = "auto"),
        ),
    actionButton(ns("refresh"), label = "Refresh"),
    actionButton(ns("analyze"), label = "Analyze")
      ),
    style = "opacity: 0.90"
    ),
    )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session, con){
  ns <- session$ns
  
  selected <- reactiveValues()
  
  observeEvent(input$refresh, {
    if(RSQLite::dbExistsTable(con, "enrichment_register")) {
      
      enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
      
      output$enrichment_overview <- DT::renderDataTable({
        styling <- DT:::DT2BSClass(c('compact', 'hover'))
        DT::datatable(enrichment_objects, 
                      class = styling,
                      selection = "single",
                      options = list(
                        scrollX = TRUE,
                        scrollY = TRUE,
                        dom = 't')
        )
      })
      
    } else {
      return(NULL)
    }
  })
  

  observeEvent(input$analyze, {
    selected$selected_object <- input$enrichment_overview_rows_selected
    output$results_ui <- renderUI({
      tagList(
        fluidPage(
            tabsetPanel(type = "tab",
                        tabPanel("Dot plot", mod_dot_plot_ui(ns("dot_plot_ui_1"))),
                        tabPanel("Bar plot"),
                        tabPanel("Enrichment map", mod_enrichment_map_ui(ns("enrichment_map_ui_1"))),
                        tabPanel("Gene-concep network"),
                        tabPanel("Heatmap"),
                        tabPanel("Results",  mod_enrichment_results_ui(ns("enrichment_results_ui_1")))
            )
      )
      )
    })
  })
  
  callModule(mod_enrichment_results_server, "enrichment_results_ui_1", selected, con = con)
  callModule(mod_enrichment_map_server, "enrichment_map_ui_1", selected, con = con)
  callModule(mod_dot_plot_server, "dot_plot_ui_1", selected, con = con)
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

