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
    # Number coantiners
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4",
                      tags$form(class = "well",
                                tags$h2(class = "text-center",
                                        tags$span(
                                          class="label", "1",
                                          style = "border-radius: 100%;background-color:#ffbd40")))),
             htmlOutput(ns("algorithm")),
             ),
    
    # Module containers          
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4",
                      tags$form(class = "well",
                                `style`="background-color:#2c3e50;",
                                uiOutput(ns("input_choice")),
                                mod_upload_ui(ns("upload_ui_1"))
                                ),
                      ),
             htmlOutput(ns("algorithm1"))))
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
             actionButton(ns("create_input"), "Create input object")),
    tags$br()
    )
  })
  }
  
  
  # Action button for creating input
  observeEvent(input$create_input, {
    upload_ui_1$input_object <- input$input_object
  })
  
  upload_ui_1 <- callModule(mod_upload_server, "upload_ui_1", con = con)
  
  observeEvent(upload_ui_1$input_object, {
    MODifieR_module <- upload_ui_1$input_object
  }
  )
  
  upload_algorithm <- reactive({
    req(upload_ui_1$input_object) 
    
    if (is.null(upload_ui_1$input_object)){
      return(NULL)
    }
  })

  # Number conainters
  output$algorithm <- renderUI({
    upload_algorithm()
    tags$div(`class`="col-sm-4",
             tags$form(class = "well",
                       tags$h2(class = "text-center",
                               tags$span(
                                 class="label", "2",
                                 style = "border-radius: 100%;background-color:#ffbd40"))))
})
  
  # Module conatiners
  output$algorithm1 <- renderUI({
    upload_algorithm()
    tags$div(`class`="col-sm-4",
             tags$form(class = "well",
                       `style`="background-color:#2c3e50;",
                       mod_Description1_ui(ns("Description1_ui_1"))))
})
  
  callModule(mod_Description1_server, "Description1_ui_1", con = con)
} # Closes server function
    
## To be copied in the UI
# mod_Columns_ui("Columns_ui_1")
    
## To be copied in the server
# callModule(mod_Columns_server, "Columns_ui_1")
 
