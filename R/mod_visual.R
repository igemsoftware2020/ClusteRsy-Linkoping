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
    mod_DiffCoEx_ui(ns("DiffCoEx_ui_1")),
    tags$div(`class`="jumbotron",
             tags$h1("Hello,world!",class="display-3"),
             tags$hr(class="my-4"),
             tags$p("I made it!"),
             tags$b(),
             tags$p(class="lead",
                   tags$a("Learn more", class="btn btn-primary btn-lg", href="https://bootswatch.com/flatly/")
                   )
             )
    )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session){
  ns <- session$ns
  callModule(mod_DiffCoEx_server, "DiffCoEx_ui_1")
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

