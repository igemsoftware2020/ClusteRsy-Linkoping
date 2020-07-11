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
    # Floating panel with parameters. 
    absolutePanel(
      id = "controls",
      bottom = 20, 
      right = 20, 
      width = "25%", 
      height = "60%",
      draggable = TRUE,
      wellPanel( #Text in the floating panel.
        tags$h3(class = "text-center",
          "Pick an enrichment object for visualization"
        ),
        tags$p(class = "text-right",
               "You can drag this panel around."),
        
        tags$form(`class` = "well", 
                  style = "background-color:#FFFFFF;",
    DT::dataTableOutput(ns("enrichment_overview"), width = "auto", height = "auto"), #Data table for plot output
        ),
    actionButton(ns("analyze"), label = "Analyze"),
    
    #Renders the parameters when analyzed is triggered.
    uiOutput(ns("parameters"))  
      ),
    ),
  )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session, con, main_page_v2_module){
  ns <- session$ns
  
  selected <- reactiveValues()
  
  # Create an empty table
  enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
  
  output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                    rownames = FALSE,
                                                    class = 'compact hover',
                                                    selection = "single",
                                                    options =  list(scrollX = TRUE,
                                                                    scrollY = TRUE,
                                                                    dom = 't'))
  
  observeEvent(main_page_v2_module$enrich, {
      enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
      
      output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                        rownames = FALSE,
                                                        class = 'compact hover',
                                                        selection = "single",
                                                        options =  list(scrollX = TRUE,
                                                                        scrollY = TRUE,
                                                                        dom = 't'))
  })
  

  observeEvent(input$analyze, {
    selected$selected_object <- input$enrichment_overview_rows_selected
    if(RSQLite::dbExistsTable(con, "enrichment_register")) {
    output$results_ui <- renderUI({
      tagList(
        fluidPage(
          mainPanel( width = 12,
            tabsetPanel(type = "tabs",
                        tabPanel("Dot plot", mod_dot_plot_ui(ns("dot_plot_ui_1"))),
                        tabPanel("Bar plot"),
                        tabPanel("Enrichment map", mod_enrichment_map_ui(ns("enrichment_map_ui_1"))),
                        tabPanel("Gene-concep network"),
                        tabPanel("Heatmap"),
                        tabPanel("Results",  mod_enrichment_results_ui(ns("enrichment_results_ui_1")))))))
      })
    output$parameters <- renderUI({
      mod_dot_plot_para_ui(ns("dot_plot_para_ui_1"))
      })
    } else {
      #This should be replaced with the same container type as the "click me to learn more" one. 
      #There should be a link that sends the user back to the first page if they don't have any data.
      showNotification("No data to visualize", duration = NULL, closeButton = TRUE, type = "warning")
    }
    })
  
  dot_plot_para_ui_1 <- callModule(mod_dot_plot_para_server, "dot_plot_para_ui_1")
  callModule(mod_enrichment_results_server, "enrichment_results_ui_1", selected, con = con)
  callModule(mod_enrichment_map_server, "enrichment_map_ui_1", selected, con = con)
  callModule(mod_dot_plot_server, "dot_plot_ui_1", dot_plot_para_ui_1, selected, con = con)
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

