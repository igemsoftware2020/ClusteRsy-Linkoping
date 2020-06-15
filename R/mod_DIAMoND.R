#' DIAMoND UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DIAMoND_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' DIAMoND Server Function
#'
#' @noRd 
mod_DIAMoND_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_DIAMoND_ui("DIAMoND_ui_1")
    
## To be copied in the server
# callModule(mod_DIAMoND_server, "DIAMoND_ui_1")
 
