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
    navbarPage(title =  "MODifieRWeb",
                     tabPanel("Input data",
                              tags$div(`class`="row",
                                      tags$div(`class`="col-sm-4",
                                               tags$form(class = "well",
                                                         `style`="background-color:#ecf0f1;",
                                                          tags$h2(class = "text-center",
                                                            tags$span(
                                                              class="label label-warning", "1",
                                                              style = "border-radius: 100%;")
                                                            )
                                                         )
                                               ),
                                      htmlOutput(ns("algorithm")
                                                 )
                                      ),
                                  
                              tags$div(`class`="row",
                                       tags$div(`class`="col-sm-4",
                                                tags$form(class = "well",
                                                          `style`="background-color:orange;",
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
  
  output$algorithm <- renderUI({
    algoirthm_matrix <- upload_algorithm()
    tags$div(`class`="col-sm-4",
              tags$form(class = "well",
                        `style`="background-color:#ecf0f1;",
                        tags$h2(class = "text-center",
                                tags$span(
                                class="label label-warning", "2")
                     )
           )
  )
  }
)
  output$algorithm1 <- renderUI({
    algoirthm_matrix <- upload_algorithm()
    tags$div(`class`="col-sm-4",
           tags$form(class = "well",
                     `style`="background-color:orange;"
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
 
