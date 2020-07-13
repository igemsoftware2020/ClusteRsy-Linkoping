#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session) {
  # Loading screen
  con <- load_data()
  # Load example
  enrichment_object <- readRDS("./data_example/breast_cancer_example.rds")
  MODifieRDB::enrichment_object_to_db(enrichment_object,
                                      module_name = "Breast cancer example", 
                                      enrichment_method = "enrichDGN", 
                                      con = con)
  # List the first level call Modules here
  callModule(mod_main_page_v2_server, "main_page_v2_ui_1", con)
}

