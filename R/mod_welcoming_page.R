#' welcoming_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 


mod_welcoming_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(style="height: 100vh; width: 100%; background-image: url('www/front_page1.png');background-repeat:no-repeat;background-size:cover;; color:white", class= "row",
             tags$div(`class`="container", style = "margin: 17vh 10vw; padding: 0;",
                      tags$h1("Lorem ipsum", style = "margin: 0"),
                      rep_br(2),
                      tags$h2("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo "),
                      tags$h2("Lorem ipsum dolor sit amet, consectetuer"))),
    tags$div(style="height: 95vh; width: 100%;text-align:center;", class= "row",
             tags$div(`class`="col-sm-4", style="height: 100%;text-align: center;padding-left: 3%;",
                      tags$h3(style ="color:#615a5a;margin: 0;position: absolute; top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%);", "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. justo, fringilla vel, aliquet nec, vulputate ")),
             tags$div(`class`="col-sm-8", style = "height:100%",
                      tags$a(class="grid-item", style="background-image:url('www/grid_item1.jpg');",
                             tags$p("Lorem ipsum dolor sit amet")),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item2.jpg');",
                             tags$p("Lorem ipsum dolor sit amet")))),
    tags$div(style="height: 50vh; width: 100%; background-color:#ffbd40;",`class`="row",
             tags$div(`class`="col-sm-4", style="border-right: 1px solid #333c41;height: 80%;top:10%;text-align:center;padding-left:2%;padding-right:2%",
                      tags$h3(style="color:#333c41;margin-top:15%", "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. , rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae")),
             tags$div(`class`="col-sm-4", style="border-right: 1px solid #333c41;height: 80%;top:10%;text-align:center;padding-left:2%;padding-right:2%",
                      tags$h3(style="color:#333c41;margin-top:15%", "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. , rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae")),
             tags$div(`class`="col-sm-4", style= "top:10%;text-align:center;padding:0;padding-left:2%;padding-right:2%",
                      tags$h3(style="color:#333c41;margin-top:15%", "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. , rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae"))),
    tags$div(`class`="text-center", style = "color:black; height:50vh",
             tags$h3("Some cool panels", style = "position: relative;top: 45%; color: black;")),
    tags$div(style="height: 85vh; width: 100%; margin-bottom:2%;",`class`="swiper-container",
             tags$div(`class`="swiper-wrapper",
                     tags$div(`class`="swiper-slide",
                              tags$div(`class`="row",
                                       tags$div(`class`="col-sm-4", style="height:100vh;padding:0;",
                                                tags$a(class="swiper-item", style="background-image:url('www/slick_item1.jpg');")),
                                       tags$div(`class`="col-sm-4", style="height:100vh;padding:0;",
                                                tags$a(class="swiper-item", style="background-image:url('www/slick_item2.jpg');")),
                                       tags$div(`class`="col-sm-4", style="height:100vh;padding:0;",
                                                tags$a(class="swiper-item", style="background-image:url('www/slick_item3.jpg');")))),
                     tags$div(`class`="swiper-slide",
                              tags$div(`class`="row",
                                       tags$div(`class`="col-sm-4", style="height:100vh;padding:0;",
                                                tags$a(class="swiper-item", style="background-image:url('www/slick_item1.jpg');")),
                                       tags$div(`class`="col-sm-4", style="height:100vh;padding:0;",
                                                tags$a(class="swiper-item", style="background-image:url('www/slick_item2.jpg');")),
                                       tags$div(`class`="col-sm-4", style="height:100vh;padding:0;",
                                                tags$a(class="swiper-item", style="background-image:url('www/slick_item3.jpg');"))))),
             tags$div(`class`="swiper-button-prev"),
             tags$div(`class`="swiper-button-next"))
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
 
