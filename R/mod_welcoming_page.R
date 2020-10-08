#' welcoming_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#'@import url('https://fonts.googleapis.com/css2?family=Quicksand&display=swap%27);
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
                      tags$h1("ClusteRsy", style = "margin: 0; font-family:Quicksand; font-size: 3vw;"),
                      rep_br(2),
                      tags$h3("Our belief is that ClusteRsy will help to streamline transcriptomics data processing and thus make science and bioinformatics in more approachable for every professional enthusiast.", style="font-family:Quicksand; font-size: 1.4vw;"),
                      tags$h3("As a team of young researchers we aspire to make this tool:", style="font-family:Quicksand; font-size: 1.4vw;"),
                      tags$h3("Simple", style="font-family:Quicksand; font-size: 1.4vw;"),
                      tags$h3("Enlightening", style="font-family:Quicksand; font-size: 1.4vw;"), 
                      tags$h3("Innovative", style="font-family:Quicksand; font-size: 1.4vw;")
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
                      tags$h3(style ="color:#615a5a;margin: 0;position: absolute; top: 50%;-ms-transform: translateY(-50%);transform: translateY(-50%); font-size: 1.8vw", "ClusteRsy is a tool that analyses and elucidates the predominant disease genes in transcriptome data. By the creation of disease modules the interplay and significance of genes are illuminated and the disease is identified.", style="font-family:Quicksand")),
             tags$div(`class`="col-sm-8", style = "height:100%",
                   
                      tags$a(class="grid-item", style="background-image:url('www/grid_item1.jpg');",
                             tags$p("MODifieR", style="font-family:Quicksand"),
                             tags$p("MODifieR is and acronym for Module Identification and contains 8 different algorithms for identification of disease modules from transcriptome data.",
                                     style="font-family:Quicksand; opacity:0; padding:20% 1%; font-size: 1vw;")),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item2.jpg');",
                             tags$p("Enrichment analysis", style="font-family:Quicksand"),
                             tags$p("We provide enrichment analysis using the clusterprofiler package.",
                                    tags$br(),
                                    tags$br(),
                                    tags$b("Available methods: "),
                                    tags$br(),
                                    tags$b("Disease analysis:"), "DOSE, DisGenNET and NCG repositories available",
                                    tags$br(),
                                    tags$br(),
                                    tags$b("Gene ontology:"), "GO bioconductor database available",
                                    tags$br(),
                                    tags$br(),
                                    tags$b("Pathway analysis:"), "KEGG databas available",
                                    style="font-family:Quicksand; opacity:0; padding: 5% 1%; font-size: 0.8vw;")),
             tags$br(),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item3.jpg');",
                             tags$p("Visualization", style="font-family:Quicksand"),
                             tags$p("We provide four different plots that are reactive, meaning you can adjust the looks and data being displayed. 
If you want to investigate a certain disease you can easily do so in the Results table. ",
                                    tags$br(),
                                    tags$br(),
                                    "All the plots and data can be downloaded to your local machine. ",
                                    style="font-family:Quicksand; opacity:0;     padding: 10% 1%; font-size: 0.9vw;")),
                      tags$a(class="grid-item", style="background-image:url('www/grid_item4.jpg');",
                            tags$p("Database", style="font-family:Quicksand"),
                            tags$p("We provide a database that stores input data, modules created with MODifieR, Enrichment analysis results as well as PPI networks. You can at any given time upload and download your data. ",
                                   style="font-family:Quicksand; opacity:0; padding:15% 1%; font-size: 1vw;")),
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
                                               tags$p())),
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
 
