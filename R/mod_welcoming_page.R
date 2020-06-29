#' welcoming_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_welcoming_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(style="border-style: dashed; color:black; background-image: url('www/headline_img.jpg'); background-size: contain",
             tags$div(class = "headline", style="border-style: dashed; margin-top: 1.2%;",
               tags$p(class= "center", "Headline")
                ),
             tags$br(),
             tags$div(class = "headline", style="border-style: dashed;",
                tags$p(class= "center", "Sub-headline")),
             tags$br(),
             tags$div(class = "headline", style="border-style: dashed; margin-bottom: 1.2%;",
                tags$p(class= "center", "Primary call to action"))
             ),
    tags$br(),
    tags$div(style="border-style: dashed; color:black",
             tags$p(class= "center", style="text-align:center", "Sub-headline"))
  )
}
    
#' welcoming_page Server Function
#'
#' @noRd 
mod_welcoming_page_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_welcoming_page_ui("welcoming_page_ui_1")
    
## To be copied in the server
# callModule(mod_welcoming_page_server, "welcoming_page_ui_1")
 
