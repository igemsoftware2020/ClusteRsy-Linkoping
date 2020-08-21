#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session) {
  app_servr <- reactiveValues()
  # Loading screen
  con <- MODifieRDB::connect_to_db("./data_example/igem.db")
  app_servr$loaded <- con
  # Load example
  enrichment_object <- readRDS("./data_example/breast_cancer_example.rds")
  MODifieRDB::enrichment_object_to_db(enrichment_object,
                                      module_name = "Breast cancer example", 
                                      enrichment_method = "enrichDGN", 
                                      con = con)
  # Listen to the beautiful button
  observeEvent(input$tool_button, {
    app_servr$tool_button <- input$tool_button
  })
  
  observeEvent(input$user_guide_btn, {
    app_servr$user_guide_btn <- input$user_guide_btn
  })
  
  observeEvent(input$tutorial_btn, {
    app_servr$tutorial_btn <- input$tutorial_btn
  })
  
  observeEvent(input$tutorial_start_btn, {
    app_servr$tutorial_start_btn <- input$tutorial_start_btn
  })
  
  # DT double click for input
  observeEvent(input$input_dbclick, {
    app_servr$input_dbclick <- input$input_dbclick
    app_servr$input_name <- input$input_name
  })
  
  # DT tooltip
  observeEvent(input$DT_tooltip, {
    app_servr$DT_tooltip <- input$DT_tooltip
  })
  
  # List the first level call Modules here
  callModule(mod_main_page_v2_server, "main_page_v2_ui_1", con = con, app_servr = app_servr)
  
  # information popups in data-tab
  observeEvent(input$information_btn_input, {
    app_servr$information_btn_input <- input$information_btn_input
  })
  observeEvent(input$information_btn_module, {
    app_servr$information_btn_module <- input$information_btn_module
  })
  observeEvent(input$information_btn_enrichment, {
    app_servr$information_btn_enrichment <- input$information_btn_enrichment
  })
  observeEvent(input$information_btn_ppi, {
    app_servr$information_btn_ppi <- input$information_btn_ppi
  })
}
