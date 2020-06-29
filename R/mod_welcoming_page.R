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
    tags$div(style="height:100vh",
              tags$div(style="border-style: dashed; color:black; background-image: url('www/headline_img.jpg'); background-size: contain;",
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
              tags$div(style="border-style: dashed; color:black; height: 39vh",
                       tags$p(class= "center", style="text-align:center;", "Sub-headline"))),
    
              tags$div(style="height:100vh; padding-top: 1%;",
                      tags$div(style="border-style: dashed; color:black; height: 10vh;",
                               tags$p(class= "center", style="text-align:center;", "Benefist list")),
                      tags$br(),
                      tags$div(style="border-style: dashed; color:black; height: 26vh",
                               tags$p(class= "center", style="text-align:center;", "Feature highlights")),
                      tags$br(),
                      tags$div(style="border-style: dashed; color:black; height: 26vh",
                               tags$p(class= "center", style="text-align:center;", "Trust indicators")),
                      tags$br(),
                      tags$div(style="border-style: dashed; color:black; height: 26vh",
                               tags$p(class= "center", style="text-align:center;", "Trust indicators"))),
    
              tags$div(style="height:100vh; padding-top: 1%;",
                      tags$div(style="text-align:center; color:black",
                               tags$span(`class`="col-sm-4", style="border-style: dashed; height:100vh",
                                        tags$p("Useful contetn")),
                               tags$span(`class`="col-sm-4", style="border-style: dashed; height:100vh",
                                        tags$p("Company update")),
                               tags$span(`class`="col-sm-4", style="border-style: dashed; height:100vh",
                                        tags$p("Offers")),))
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
 
