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
  uiOutput(ns("tables"))
  )
}
    
#' Mcode_post_processing Server Function
#'
#' @noRd 
mod_Mcode_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, con){
  ns <- session$ns
  
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
                      tabPanel(title = "Modules table",
                               DT::dataTableOutput(ns("modules_table"))),
                      tabPanel(title = "Module score table",
                               DT::dataTableOutput(ns("module_score_table"))),
                      tabPanel(title = "Settings table",
                               DT::dataTableOutput(ns("settings_table"))))),
        footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
        ),
      ))
    )
  })
 
 
 
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  modules <- as.matrix(inspected_module$modules)
  colnames(modules) <- list("Modules")
  
  module_score <- data.frame(inspected_module$module_scores)
  colnames(module_score) <- list("Module Score")
  
  settings <- as.matrix(inspected_module$settings)
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
  
  output$modules_table <- DT::renderDataTable({modules},
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
 
}
    
## To be copied in the UI
# mod_Mcode_post_processing_ui("Mcode_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Mcode_post_processing_server, "Mcode_post_processing_ui_1")
 
