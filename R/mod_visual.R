#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import magrittr
#' 
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
      style = "z-index: 25;",
      wellPanel( #Text in the floating panel.
        tags$div(id = "collasp-panel",
          tags$h3(class = "text-center", "Visualization"),
          tags$form(`class` = "well", style = "background-color:#FFFFFF;",
                    DT::dataTableOutput(ns("enrichment_overview"), width = "auto", height = "auto")), #Data table for plot output
          tags$div(style = "text-align:center",
                  actionButton(ns("analyze"), label = "Analyze")),
          
          uiOutput(ns("disable")),
          #Renders the parameters when analyzed is triggered.
          uiOutput(ns("parameters")) 
        )
      )
    )
  )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session, con, main_page_v2_module){
  ns <- session$ns
  
  selected <- reactiveValues()
  
  # Create a table
  enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
  output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                    rownames = FALSE,
                                                    class = 'compact hover',
                                                    selection = list(mode = 'single', selected = c(1)),
                                                    options =  list(scrollX = TRUE,
                                                                    scrollY = TRUE,
                                                                    dom = 't'))
  # Updating the DT when a new enrichment object is created.
  observeEvent(main_page_v2_module$enrich, {
      enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
      output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                        rownames = FALSE,
                                                        class = 'compact hover',
                                                        selection = list(mode = 'single', selected = c(1)),
                                                        options =  list(scrollX = TRUE,
                                                                        scrollY = TRUE,
                                                                        dom = 't'))
  })
  
  
  # Observe if valid to download
  observe({
    if(is.null(input$enrichment_overview_rows_selected)) {
      output$disable <- renderUI({
        tags$script((HTML("document.getElementById('main_page_v2_ui_1-visual_ui_1-analyze').disabled = true;")))
      }) 
    } else {
      output$disable <- renderUI({
        tags$script((HTML("document.getElementById('main_page_v2_ui_1-visual_ui_1-analyze').disabled = false;")))
      }) 
    }
  })

  observeEvent(input$analyze, {
    selected$selected_object <- input$enrichment_overview_rows_selected
     output$results_ui <- renderUI({
       tagList(
         fluidPage(
             tabsetPanel(id = ns("tabs"),
                         type = "tabs",
                         tabPanel(title = "Dot plot", mod_dot_plot_ui(ns("dot_plot_ui_1"))),
                         tabPanel(title = "Enrichment map", mod_enrichment_map_ui(ns("enrichment_map_ui_1"))),
                         tabPanel(title = "Gene-concept network", mod_cnet_plot_ui(ns("cnet_plot_ui_1"))),
                         tabPanel(title = "Heatmap", mod_heat_plot_ui(ns("heat_plot_ui_1"))),
                         tabPanel(title = "Results",  mod_enrichment_results_ui(ns("enrichment_results_ui_1"))))))
    
       })
    })
  
  #Calling the parameters to the absolutePanel
  observeEvent(input$tabs, {
    output$parameters <- renderUI({
      
      if (input$tabs == "Dot plot") {
        mod_dot_plot_para_ui(ns("dot_plot_para_ui_1"))
      } else if (input$tabs == "Enrichment map") {
        mod_enrichment_map_para_ui(ns("enrichment_map_para_ui_1"))
      } else if (input$tabs == "Gene-concept network") {
        mod_cnet_plot_para_ui(ns("cnet_plot_para_ui_1"))
      } else if (input$tabs == "Heatmap") {
        mod_heat_plot_para_ui(ns("heat_plot_para_ui_1"))
      } else if (input$tabs == "Results") {
        NULL
      }
    }) 
  })
  
 
  
  #Parameter modules server call
  dot_plot_para_ui_1 <- callModule(mod_dot_plot_para_server, "dot_plot_para_ui_1")
  enrichment_map_para_ui_1 <- callModule(mod_enrichment_map_para_server, "enrichment_map_para_ui_1")
  cnet_plot_para_ui_1 <- callModule(mod_cnet_plot_para_server, "cnet_plot_para_ui_1")
  heat_plot_para_ui_1 <- callModule(mod_heat_plot_para_server, "heat_plot_para_ui_1")
  
  #Plot modules servercall
  callModule(mod_dot_plot_server, "dot_plot_ui_1", dot_plot_para_ui_1, selected, con = con)
  callModule(mod_enrichment_map_server, "enrichment_map_ui_1",enrichment_map_para_ui_1, selected, con = con)
  callModule(mod_cnet_plot_server, "cnet_plot_ui_1", cnet_plot_para_ui_1, selected, con = con)
  callModule(mod_heat_plot_server, "heat_plot_ui_1", heat_plot_para_ui_1, selected, con = con)
  callModule(mod_enrichment_results_server, "enrichment_results_ui_1", selected, con = con)
  
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

