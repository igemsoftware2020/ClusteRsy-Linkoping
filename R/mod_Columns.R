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
    tags$div(`class`="row", style = "position: relative;",
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well",
                                tags$h2(class = "text-center",
                                        tags$span(
                                          class="label", "1",
                                          style = "border-radius: 100%;background-color:#ffbd40")))),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well faded",
                                id = "num_contain_2",
                                tags$h2(class = "text-center",
                                        tags$span(
                                          class="label", "2",
                                          style = "border-radius: 100%;background-color:#ffbd40")))),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well faded",
                                id = "num_contain_3",
                                tags$h2(class = "text-center",
                                        tags$span(
                                          class="label", "3",
                                          style = "border-radius: 100%;background-color:#ffbd40")))),
             HTML("<dash id='arrow1' class='no1' style='left: 18.4%;top: 47.8%;'></dash><dash id='arrow2' class='no1' style='left: 51.9%;top: 47.8%;'></dash>"),
             tags$i(class = "fa fa-chevron-right", style="position: absolute;left: 48%;top: 42.8%;"),
             tags$i(class = "fa fa-chevron-right", style="position: absolute;left: 81.5%;top: 42.8%;")
             ),
    
    # Module container         
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well",
                                `style`="background-color:#2c3e50;",
                                uiOutput(ns("input_choice")),
                                mod_upload_ui(ns("upload_ui_1"))
                                ),
                      ),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;
                       cursor: not-allowed;", id = "mod2",
                      tags$form(class = "well faded",
                                id = "mod_contain_2",
                                `style`="background-color:#2c3e50; pointer-events :none;",
                                mod_Description1_ui(ns("Description1_ui_1")))),
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;
                      cursor: not-allowed;", id = "mod3",
                      tags$form(class = "well faded",
                                id = "mod_contain_3",
                                `style`="background-color:#2c3e50; pointer-events :none;",
                                mod_disease_analysis_ui(ns("disease_analysis_ui_1"))))
             ),
    htmlOutput(ns("fadein"))
  )
}
    
#' Columns Server Function
#'
#' @noRd 
mod_Columns_server <- function(input, output, session, con){
  ns <- session$ns
  
  if (length(unlist((MODifieRDB::get_available_input_objects(con)$input_name))) != 0){
    output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    tagList(
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "Choose your input method"),
    tags$div(style = "text-align:center",
             actionButton(ns("select_input"), "Use this input object")),
    tags$br()
    )
  })
  }
  
  
  # Action button for creating input
  observeEvent(input$select_input, {
    upload_ui_1$input_object <- MODifieRDB::MODifieR_input_from_db(input$input_object, con = con)
  })
  
  upload_ui_1 <- callModule(mod_upload_server, "upload_ui_1", con = con)
  
  observeEvent(upload_ui_1$input_object, {
    MODifieR_module <- upload_ui_1$input_object
  }
  )
  
  output$fadein <- renderUI({
    req(upload_ui_1$input_object) 
    tagList(
      tags$head(tags$script(src = "www/fadein.js", type="text/javascript")),
      tags$script("col2(); col3()")
    )
  })
  
  callModule(mod_disease_analysis_server, "disease_analysis_ui_1", con = con)
  callModule(mod_Description1_server, "Description1_ui_1", con = con)
} # Closes server function
    
## To be copied in the UI
# mod_Columns_ui("Columns_ui_1")
    
## To be copied in the server
# callModule(mod_Columns_server, "Columns_ui_1")
 
