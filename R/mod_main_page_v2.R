#' main_page_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_page_v2_ui <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(title =  "MODifieRWeb", collapsable = TRUE,
                     tabPanel("Input data",
                              
                              # Number coantiners
                              tags$div(`class`="row",
                                      tags$div(`class`="col-sm-4",
                                               tags$form(class = "well",
                                                          tags$h2(class = "text-center",
                                                            tags$span(
                                                              class="label", "1",
                                                              style = "border-radius: 100%;background-color:#ffbd40")
                                                            )
                                                         )
                                               ),
                                                          htmlOutput(ns("algorithm")
                                                 )
                                      ),
                            
                              # Module containers          
                              tags$div(`class`="row",
                                       tags$div(`class`="col-sm-4",
                                                tags$form(class = "well",
                                                          `style`="background-color:#2c3e50;",
                                                          mod_upload_ui(ns("upload_ui_1")
                                                                        )
                                                          )
                                                ),
                                                          htmlOutput(ns("algorithm1")
                                                  )
                                       )
                              ),
                     tabPanel("Visualization", mod_visual_ui(ns("visual_ui_1")
                                                             )
                              )
               )
    )
}
    
#' main_page_v2 Server Function
#'
#' @noRd 
mod_main_page_v2_server <- function(input, output, session){
  ns <- session$ns
  
  upload_ui_1 <- callModule(mod_upload_server, "upload_ui_1")
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
    algoirthm_matrix <- upload_algorithm()
    tags$div(`class`="col-sm-4",
              tags$form(class = "well",
                        tags$h2(class = "text-center",
                                tags$span(
                                class="label", "2",
                                style = "border-radius: 100%;background-color:#ffbd40")
                     )
           )
  )
  }
)
  
  # Module conatiners
  output$algorithm1 <- renderUI({
    algoirthm_matrix <- upload_algorithm()
    tags$div(`class`="col-sm-4",
           tags$form(class = "well",
                     `style`="background-color:#2c3e50;"
           )
  )
  }
)
  
  visual_ui_1 <- callModule(mod_visual_server, "visual_ui_1")
 
}
    
## To be copied in the UI
# mod_main_page_v2_ui("main_page_v2_ui_1")
    
## To be copied in the server
# callModule(mod_main_page_v2_server, "main_page_v2_ui_1")
 
