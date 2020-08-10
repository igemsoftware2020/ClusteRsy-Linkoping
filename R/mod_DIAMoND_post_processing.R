#' DIAMoND_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DIAMoND_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
 uiOutput(ns("tables"))
  )
}
    
#' DIAMoND_post_processing Server Function
#'
#' @noRd 
mod_DIAMoND_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, con){
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
                      tabPanel(title = "Seed genes table",
                               DT::dataTableOutput(ns("seed_genes_table"))),
                      tabPanel(title = "Ignored genes",
                               DT::dataTableOutput(ns("ignored_genes_table"))),
                      tabPanel(title = "Added genes",
                               DT::dataTableOutput(ns("added_genes_table"))),
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
  
  seed_genes <- as.matrix(inspected_module$seed_genes)
  colnames(seed_genes) <- list("Seed genes")
  
  ignored_genes <- as.matrix(inspected_module$ignored_genes)
  colnames(ignored_genes) <- list("Ignored genes")
  
  added_genes <- inspected_module$added_genes
  
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
  
  output$seed_genes_table <- DT::renderDataTable({seed_genes},
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
  
  output$ignored_genes_table <- DT::renderDataTable({ignored_genes},
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
  
  output$added_genes_table <- DT::renderDataTable({added_genes},
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
# mod_DIAMoND_post_processing_ui("DIAMoND_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_DIAMoND_post_processing_server, "DIAMoND_post_processing_ui_1")
 
