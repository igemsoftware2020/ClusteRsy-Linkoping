#' welcoming_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

tool_button <- '<button id = "tool_button" class ="front_button">
View app
</button>'
#This button doesn't work properly. Needs fix before beta goes live or remove it.
user_guide_button <- '<button id = "user_guide_btn" class = "front_button" style = "position:absolute; top:80%">
  User guide
</button>'
tutorial_button <- '<button id = "tutorial_btn" class = "front_button" style = "position:absolute; top:90%">
  Tutorial
</button>'



mod_welcoming_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    # section 1
    tags$div(style="height: 100vh; width: 100%; background-image: url('www/front_page1.gif');background-repeat:no-repeat;background-size:cover; color:#fff9f9", class= "row",
             tags$div(`class`="container", style = "margin: 17vh 10vw; padding: 0",
                      tags$h1("ClusteRsy", style = "margin: 0; font-family:Quicksand"),
                      rep_br(2),
                      tags$h3("Our belief is that ClusteRsy will help to streamline transcriptomics data processing and thus make science and bioinformatics in more approachable for every professional enthusiast.", style="font-family:Quicksand"),
                      tags$h3("As a team of young researchers we aspire to make this tool:", style="font-family:Quicksand"),
                      tags$h3("Simple", style="font-family:Quicksand"),
                      tags$h3("Enlightening", style="font-family:Quicksand"), 
                      tags$h3("Innovative", style="font-family:Quicksand")
                      ),
             HTML(tool_button, user_guide_button, tutorial_button),
             HTML('<a class="button learn" href="#section2">&#8595;</a>'),
             tags$a(icon("github-square", "fa-3x", lib = "font-awesome"), 
                    href = "https://github.com/iGEM-linkoping",
                    style = "position: absolute;
                             bottom: 5vh;
                             left: 5vh;")),
    ## section 2
    tags$div(id="section2", style="height: 100vh; width: 100%;text-align:center;", class= "row",
             tags$div(`class`="col-sm-4", style="height: 100%;text-align: center;padding-left: 3%;",
                      tags$h3(style ="color:#615a5a;margin: 0;position: absolute; top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%);", "ClusteRsy is a tool that analyses and elucidates the predominant disease genes in any high-throughput RNA-sequencing data. By the creation of disease modules the interplay and significance of genes are illuminated and the disease is identified.", style="font-family:Quicksand")),
             tags$div(`class`="col-sm-8", style = "height:100%",
                   
                      tags$a(class="grid-item", style="background-image:url('www/grid_item1.jpg');",
                             tags$p("MODifieR", style="font-family:Quicksand"),
                             tags$p("This is MODifieR", style="font-family:Quicksand; opacity:0")),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item2.jpg');",
                             tags$p("Enrichment analysis", style="font-family:Quicksand"),
                             tags$p("This is Enrichment analysis", style="font-family:Quicksand; opacity:0")),
             tags$br(),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item3.jpg');",
                             tags$p("Visualization", style="font-family:Quicksand"),
                             tags$p("This is Visualization", style="font-family:Quicksand; opacity:0")),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item4.jpg');",
                            tags$p("Database", style="font-family:Quicksand"),
                            tags$p("This is Database", style="font-family:Quicksand; opacity:0"))
                      )),
    tags$br(),
    ## section 3
    # tags$div(style="height: 50vh; width: 100%; background-color:#fec961;",`class`="row",
    #          tags$div(`class`="col-sm-4 welcom-col",
    #                   tags$div(`class`="container con1",
    #                            tags$h4(`class`="conh3", "Hello World!", style="font-family:Quicksand"))),
    #          tags$div(`class`="col-sm-4 welcom-col",
    #                   tags$div(`class`="brline"),
    #                   tags$div(`class`="container con1",
    #                            tags$h4(`class`="conh3", "Far far away, behind the word mountains, far from the countries Vokalia and Consonantia, there live the blind texts. Separated they live in Bookmarksgrove right at the coast of the Semantics, a large language ocean. A small river named Duden flows by their place and supplies it with the necessary", style="font-family:Quicksand"))),
    #          tags$div(`class`="col-sm-4 welcom-col",
    #                   tags$div(`class`="container con1",
    #                            tags$h4(`class`="conh3", "Hello World!", style="font-family:Quicksand")))),
    ## section 4
    tags$div(`class`="row", style="height:110vh;width:100%;padding-top: 35vh;",
              tags$div(style="height: 57vh; width: 100%; margin-bottom:5%;",`class`="swiper-container",
                       tags$div(`class`="swiper-wrapper",
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item1.jpg');",
                                               tags$p(class="IGEM", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is IGEM", ))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item2.jpg');",
                                               tags$p(class="Adam", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Adam"))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item3.jpg');",
                                               tags$p(class="Ronja", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Ronja"))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item4.jpg');",
                                               tags$p(class="Alexander", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Alexander"))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item5.jpg');",
                                               tags$p(class="Christina", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Christina"))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item6.jpg');",
                                               tags$p(class="Waluigi", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Waluigi"))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item7.jpg');",
                                               tags$p(class="Erika", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Erika"))),
                               tags$div(`class`="swiper-slide",
                                        tags$a(class="swiper-item", style="background-image:url('www/swiper_item8.jpg');",
                                               tags$p(class="Jake", style="background:transparent; opacity:0; font-family:Quicksand;", "My name is Jake")))),
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
 
