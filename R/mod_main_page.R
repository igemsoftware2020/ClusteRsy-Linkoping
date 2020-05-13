#' main_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "Menu"),
      shinydashboard::dashboardSidebar(shinydashboard::sidebarMenuOutput(ns("menu"))),
      shinydashboard::dashboardBody(shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "tab_two", mod_upload_ui(ns("upload_ui_1")))
      ))
    )
    
  )
}

#' main_page Server Function
#'
#' @noRd 
mod_main_page_server <- function(input, output, session){
  
  ns <- session$ns
  
  observeEvent(input$tabs,{
    if(input$tabs=="tab_two"){
      upload_ui_1 <- callModule(mod_upload_server, "upload_ui_1")
      observeEvent(upload_ui_1$module, {
       MODifieR_module <- upload_ui_1$module
      })
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$menu <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(id = ns("tabs"),
                                  shinydashboard::menuItem(
                                  "Create input",
                                  icon = icon("globe"),
                                  tabName = "tab_two"
                                )
    )
  })
  
}

## To be copied in the UI
# mod_main_page_ui("main_page_ui_1")

## To be copied in the server
# callModule(mod_main_page_server, "main_page_ui_1")

