#' Columns UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Columns_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Number container
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well", style = "background-color: inherit",
                                tags$h2(class = "text-center",
                                        tags$span(
                                          class="label", "1",
                                          style = "border-radius: 100%;background-color:#ffbd40")))),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well faded", style = "background-color: inherit",
                                id = "num_contain_2",
                                tags$h2(class = "text-center",
                                        tags$a(
                                          href = "javascript:void(0)",
                                          onclick = "col2();",
                                          class="label", "2",
                                          style = "border-radius: 100%;background-color:#ffbd40;")))),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well faded", style = "background-color: inherit",
                                id = "num_contain_3",
                                tags$h2(class = "text-center",
                                        tags$a(
                                          href = "javascript:void(0)",
                                          onclick = "col3();",
                                          class="label", "3",
                                          style = "border-radius: 100%;background-color:#ffbd40;")))),
             HTML("<dash id='arrow1' class='no1' style='left: 20.6vw;top: 60px;'></dash><dash id='arrow2' class='no1' style='left: 55vw;top: 60px;'></dash>"),
             tags$i(class = "fa fa-chevron-right", style="position: relative;left: 46.5vw;bottom: 76.01px;"),
             tags$i(class = "fa fa-chevron-right", style="position: relative;left: 80vw;bottom: 74px;")
             ),
    
    # Module container         
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well",
                                uiOutput(ns("input_choice")),
                                mod_upload_ui(ns("upload_ui_1"))
                                ),
                      ),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;
                       cursor: not-allowed;", id = "mod2",
                      tags$form(class = "well faded",
                                id = "mod_contain_2",
                                `style`="pointer-events :none;",
                                mod_Description1_ui(ns("Description1_ui_1")))),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;
                      cursor: not-allowed;", id = "mod3",
                      tags$form(class = "well faded",
                                id = "mod_contain_3",
                                `style`="pointer-events :none;",
                                mod_disease_analysis_ui(ns("disease_analysis_ui_1"))))
             ),
    htmlOutput(ns("fadein")),
    htmlOutput(ns("fadein1"))
  )
}
    
#' Columns Server Function
#'
#' @noRd 
mod_Columns_server <- function(input, output, session, con){
  ns <- session$ns
  
  Columns_module <- reactiveValues()

  upload_ui_1 <- callModule(mod_upload_server, "upload_ui_1", con = con)
  Description1_ui_1 <- callModule(mod_Description1_server, "Description1_ui_1", con = con, upload_ui_1)
  disease_analysis_ui_1 <- callModule(mod_disease_analysis_server, "disease_analysis_ui_1", con = con, Description1_ui_1)
  
  observeEvent(upload_ui_1$input_name,{
    Columns_module$input_name <- upload_ui_1$input_name
  })
  
  observeEvent(Description1_ui_1$module_name,{
    Columns_module$module_name <- Description1_ui_1$module_name
  })
  
  observeEvent(disease_analysis_ui_1$enrich, {
    Columns_module$enrich <- disease_analysis_ui_1$enrich
  })
  
  # Activate column 2 and 3
  output$fadein <- renderUI({
    req(upload_ui_1$input_name) 
    tagList(
      tags$script("col2();")
    )
  })
  
  output$fadein1 <- renderUI({
    req(Description1_ui_1$module_name)
    tagList(
      tags$script("col3();")
    )
  })
  
  return(Columns_module)
} # Closes server function
    
## To be copied in the UI
# mod_Columns_ui("Columns_ui_1")
    
## To be copied in the server
# callModule(mod_Columns_server, "Columns_ui_1")
 
