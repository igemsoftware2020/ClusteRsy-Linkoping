#' Correlation_clique_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Correlation_clique_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("tables"))
    )
}
    
#' Correlation_clique_post_processing Server Function
#'
#' @noRd 
mod_Correlation_clique_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, con){
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
                      tabPanel(title = "Frequency table",
                               DT::dataTableOutput(ns("frequency_table"))),
                      tabPanel(title = "Settings table",
                               DT::dataTableOutput(ns("settings_table"))))),
        footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
        ),
      )
      )
    )
  })
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  frequency_table <- as.matrix(inspected_module$frequency_table)
  colnames(frequency_table) <- list("Frequency")
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes},
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
  
  output$frequency_table <- DT::renderDataTable({frequency_table},
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
# mod_Correlation_clique_post_processing_ui("Correlation_clique_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Correlation_clique_post_processing_server, "Correlation_clique_post_processing_ui_1")
 
