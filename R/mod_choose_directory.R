#' choose_directory UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_choose_directory_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyDirButton(ns("choose_directory"),'Save file', FALSE)
  )
}
    
#' choose_directory Server Function
#'
#' @noRd 
mod_choose_directory_server <- function(input, output, session){
  ns <- session$ns
  shinyDirChoose(input$choose_directory , 'folder', roots=c(wd='.'), filetypes=c('.RDS', 'txt'))
}
    
## To be copied in the UI
# mod_choose_directory_ui("choose_directory_ui_1")
    
## To be copied in the server
# callModule(mod_choose_directory_server, "choose_directory_ui_1")
 
