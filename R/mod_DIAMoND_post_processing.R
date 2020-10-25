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
    uiOutput(ns("tables")),
    uiOutput(ns("post_processing"))
  )
}
    
#' DIAMoND_post_processing Server Function
#'
#' @noRd 
mod_DIAMoND_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, inspect_button, post_process_button, con){
  ns <- session$ns
  
  DIAMoND_post_process <- reactiveValues()
  
  observeEvent(inspect_button, {
    req(inspect_button)
  output$tables <- renderUI({
    tagList(
      showModal(modalDialog(
        title = selected_module_name$name,
        top = 7,
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
                               DT::dataTableOutput(ns("settings_table")))),
          rep_br(2)),
                      
                      footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
                      ),
          )
        )
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
          top = 20,
          tags$h4("You can choose between adding seed genes or removing seed genes", style = "color:#2c3e50; text-align:center;"),
          selectInput(ns("post_processing_choises"),
                      label = "Select add or remove seed genes",
                      choices = c("Add seed genes",
                                  "Remove seed genes")),
          actionButton(ns("post_process_module_object"), 
                       label = "Process module"),
          footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
          )
        ))
      )
    })
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
  post_process_module_object <- reactive({
    input$post_process_module_object
  })
  
  
  observeEvent(input$post_process_module_object, {
    req(post_process_button)
    post_process_module_object <- post_process_module_object()
    DIAMoND_post_process$post_process_module_object <- post_process_module_object
    id <- showNotification("Saving module objects to database", duration = NULL, closeButton = FALSE, type = "warning")
    
    if (input$post_processing_choises == "Add seed genes") {
      
      diamond_add_seed_genes <- MODifieR::diamond_add_seed_genes(inspected_module)
      
      module_name <- paste(selected_module_name$name, 
                           "added_seed_genes",
                           Sys.time(), 
                           sep = "_") %>%  gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(diamond_add_seed_genes,
                                            object_name =  module_name,
                                            con = con))
      
    } else if (input$post_processing_choises == "Remove seed genes") {
      
      diamond_remove_seed_genes <- MODifieR::diamond_remove_seed_genes(inspected_module)
      
      module_name <- paste(selected_module_name$name, 
                           "removed_seed_genes",
                           Sys.time(), 
                           sep = "_") %>%  gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(diamond_add_seed_genes,
                                            object_name =  module_name,
                                            con = con))
      
    }
    
    on.exit(removeModal())
    on.exit(removeNotification(id), add = TRUE)
  })
  
  return(DIAMoND_post_process)
 
}
    
## To be copied in the UI
# mod_DIAMoND_post_processing_ui("DIAMoND_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_DIAMoND_post_processing_server, "DIAMoND_post_processing_ui_1")
 
