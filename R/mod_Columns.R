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
             htmlOutput(ns("algorithm"))),
    
    # Module containers          
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4",
                      tags$form(class = "well",
                                `style`="background-color:#2c3e50;",
                                mod_upload_ui(ns("upload_ui_1")))),
             htmlOutput(ns("algorithm1"))))
}
    
#' Columns Server Function
#'
#' @noRd 
mod_Columns_server <- function(input, output, session, con){
  ns <- session$ns
  upload_ui_1 <- callModule(mod_upload_server, "upload_ui_1", con = con)
  observeEvent(upload_ui_1$module, {
    MODifieR_module <- upload_ui_1$module
  }
  )
  
  upload_algorithm <- reactive({
    req((upload_ui_1$module))
    if (is.null(upload_ui_1$module)){
      
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
    algoirthm_matrix <- upload_algorithm()
    tags$div(`class`="col-sm-4",
             tags$form(class = "well",
                       `style`="background-color:#2c3e50;",
                       mod_Description_ui(ns("Description_ui_1"))))
})
  
  callModule(mod_Description_server, "Description_ui_1")
} # Closes server function
    
## To be copied in the UI
# mod_Columns_ui("Columns_ui_1")
    
## To be copied in the server
# callModule(mod_Columns_server, "Columns_ui_1")
 
