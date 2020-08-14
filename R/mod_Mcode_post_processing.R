#' Mcode_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Mcode_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("tables")),
    uiOutput(ns("post_processing"))
  )
}
    
#' Mcode_post_processing Server Function
#'
#' @noRd 
mod_Mcode_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, inspect_button, post_process_button, con){
  ns <- session$ns
  
  Mcode_post_process <- reactiveValues()
  
  observeEvent(inspect_button, {
    req(inspect_button)
  output$tables <- renderUI({
    tagList(
      showModal(modalDialog(
        title = selected_module_name$name,
        easyClose = TRUE,
        size = "l",
        fluidPage(
          tabsetPanel(id = ns("tabs"),
                      type = "tabs",
                      tabPanel(title = "Module genes",
                               DT::dataTableOutput(ns("module_genes_table"))),
                      tabPanel(title = "Module score table",
                               DT::dataTableOutput(ns("module_score_table"))),
                      tabPanel(title = "Settings table",
                               DT::dataTableOutput(ns("settings_table"))))),
        footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
        ),
      ))
    )
  })
  
  })
  
  observeEvent(post_process_button, {
    req(post_process_button)
    output$post_processing <- renderUI({
      tagList(
        showModal(modalDialog(
          title = selected_module_name$name,
          easyClose = TRUE,
          size = "l",
          footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
          )
        ))
      )
    })
  })
 
 
 
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  
  module_score <- data.frame(inspected_module$module_scores, lengths(inspected_module$modules))
  colnames(module_score) <- list("Module Score", "Number of genes in the module")
  
  settings <- as.matrix(inspected_module$settings)
  settings[1] <- as.character(settings[1])
  settings[2] <- as.character(settings[2])
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes},
                                                   filter = "top",
                                                   extensions = c('Buttons'),
                                                   options = list(
                                                     dom = "lfrtipB",
                                                     scrollX = TRUE,
                                                     scrollY = TRUE,
                                                     pageLength = 10,
                                                     paging = TRUE,
                                                     searching = TRUE,
                                                     lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")) ,
                                                     buttons = 
                                                       list('copy', 
                                                            list(
                                                              extend = 'collection',
                                                              buttons = c('pdf', 'csv', 'excel'),
                                                              text = 'Download'
                                                            ))
                                                   ))
  
  
  output$module_score_table <- DT::renderDataTable({module_score},
                                                   rownames = FALSE,
                                                   filter = "top",
                                                   extensions = c('Buttons'),
                                                   options = list(
                                                     dom = "lfrtipB",
                                                     scrollX = TRUE,
                                                     scrollY = TRUE,
                                                     pageLength = 10,
                                                     paging = TRUE,
                                                     searching = TRUE,
                                                     lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")) ,
                                                     buttons = 
                                                       list('copy', 
                                                            list(
                                                              extend = 'collection',
                                                              buttons = c('pdf', 'csv', 'excel'),
                                                              text = 'Download'
                                                            ))
                                                   ))
  
  output$settings_table <- DT::renderDataTable({settings},
                                               extensions = c('Buttons'),
                                               options = list(
                                                 dom = "lfrtipB",
                                                 scrollX = TRUE,
                                                 scrollY = TRUE,
                                                 pageLength = 10,
                                                 paging = TRUE,
                                                 searching = TRUE,
                                                 lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")) ,
                                                 buttons = 
                                                   list('copy', 
                                                        list(
                                                          extend = 'collection',
                                                          buttons = c('pdf', 'csv', 'excel'),
                                                          text = 'Download'
                                                        ))
                                               ))
  
  post_process_module_object <- reactive({
    input$post_process_module_object
  })
  
  
  observeEvent(input$post_process_module_object, {
    req(post_process_button)
    post_process_module_object <- post_process_module_object()
    Mcode_post_process$post_process_module_object <- post_process_module_object
    
  })
  
  return(Mcode_post_process)
 
}

    
## To be copied in the UI
# mod_Mcode_post_processing_ui("Mcode_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Mcode_post_processing_server, "Mcode_post_processing_ui_1")
 
