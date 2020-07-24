#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session) {
  app_servr <- reactiveValues()
  # Loading screen
  con <- MODifieRDB::connect_to_db("./data_example/testdb.db")
  app_servr$loaded <- con
  # Load example
  enrichment_object <- readRDS("./data_example/breast_cancer_example.rds")
  MODifieRDB::enrichment_object_to_db(enrichment_object,
                                      module_name = "Breast cancer example", 
                                      enrichment_method = "enrichDGN", 
                                      con = con)
  # Listen to the beautiful button
  observeEvent(input$blob_button, {
    app_servr$blob_button <- input$blob_button
  })
  
  observeEvent(input$user_guide_btn, {
    app_servr$user_guide_btn <- input$user_guide_btn
  })
  
  
  
  # List the first level call Modules here
  callModule(mod_main_page_v2_server, "main_page_v2_ui_1", con = con, app_servr = app_servr)
}

