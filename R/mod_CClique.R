#' CClique UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CClique_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' CClique Server Function
#'
#' @noRd 
mod_CClique_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_CClique_ui("CClique_ui_1")
    
## To be copied in the server
# callModule(mod_CClique_server, "CClique_ui_1")
 
