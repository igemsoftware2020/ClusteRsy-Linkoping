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
button <- '<div  class="buttons">
  <button id= "blob_button" class="blob-btn">
    VIEW APP
    <span class="blob-btn__inner">
      <span class="blob-btn__blobs">
        <span class="blob-btn__blob"></span>
        <span class="blob-btn__blob"></span>
        <span class="blob-btn__blob"></span>
        <span class="blob-btn__blob"></span>
      </span>
    </span>
  </button>
  <br/>
<svg xmlns="http://www.w3.org/2000/svg" version="1.1">
  <defs>
    <filter id="goo">
      <feGaussianBlur in="SourceGraphic" result="blur" stdDeviation="10"></feGaussianBlur>
      <feColorMatrix in="blur" mode="matrix" values="1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 21 -7" result="goo"></feColorMatrix>
      <feBlend in2="goo" in="SourceGraphic" result="mix"></feBlend>
    </filter>
  </defs>
</svg>
</div>' 

mod_welcoming_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    # section 1
    tags$div(style="height: 100vh; width: 100%; background-image: url('www/front_page1.png');background-repeat:no-repeat;background-size:cover; color:#fff9f9", class= "row",
             tags$div(`class`="container", style = "margin: 17vh 10vw; padding: 0;",
                      tags$h1("Lorem ipsum", style = "margin: 0"),
                      rep_br(2),
                      tags$h2("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo "),
                      tags$h2("Lorem ipsum dolor sit amet, consectetuer")),
             HTML(button),
             HTML('<a class="button learn" href="#section2">&#8595;</a>')),
    ## section 2
    tags$div(id="section2", style="height: 100vh; width: 100%;text-align:center;", class= "row",
             tags$div(`class`="col-sm-4", style="height: 100%;text-align: center;padding-left: 3%;",
                      tags$h3(style ="color:#615a5a;margin: 0;position: absolute; top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%);", "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. justo, fringilla vel, aliquet nec, vulputate ")),
             tags$div(`class`="col-sm-8", style = "height:100%",
                      tags$a(class="grid-item", style="background-image:url('www/grid_item1.jpg');",
                             tags$p("Lorem ipsum dolor sit amet")),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item2.jpg');",
                             tags$p("Lorem ipsum dolor sit amet")))),
    ## section 3
    tags$div(style="height: 50vh; width: 100%; background-color:#fec961;",`class`="row",
             tags$div(`class`="col-sm-4 welcom-col",
                      tags$div(`class`="container con1",
                               tags$h4(`class`="conh3", "Hello World!"))),
             tags$div(`class`="col-sm-4 welcom-col",
                      tags$div(`class`="brline"),
                      tags$div(`class`="container con1",
                               tags$h4(`class`="conh3", "Far far away, behind the word mountains, far from the countries Vokalia and Consonantia, there live the blind texts. Separated they live in Bookmarksgrove right at the coast of the Semantics, a large language ocean. A small river named Duden flows by their place and supplies it with the necessary"))),
             tags$div(`class`="col-sm-4 welcom-col",
                      tags$div(`class`="container con1",
                               tags$h4(`class`="conh3", "Hello World!")))),
    ## section 4
    tags$div(`class`="row", style="height:110vh;width:100%;padding-top: 35vh;",
              tags$div(style="height: 57vh; width: 100%; margin-bottom:5%;",`class`="swiper-container",
                       tags$div(`class`="swiper-wrapper",
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item1.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item2.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item3.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item4.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item5.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item6.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item7.jpg');")),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item8.jpg');"))),
                       tags$div(`class`="swiper-button-prev"),
                       tags$div(`class`="swiper-button-next")))
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
 
