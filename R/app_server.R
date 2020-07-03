#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Loading screen
  load_data()
  # List the first level call Modules here
  callModule(mod_main_page_v2_server, "main_page_v2_ui_1")
}

