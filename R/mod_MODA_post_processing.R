#' MODA_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MODA_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("tables")),
    uiOutput(ns("post_processing"))
  )
}
    
#' MODA_post_processing Server Function
#'
#' @noRd 
mod_MODA_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, inspect_button, post_process_button, con){
  ns <- session$ns
  
  MODA_post_process <- reactiveValues()
  
  observeEvent(inspect_button, {
    req(inspect_button)
  output$tables <- renderUI({
    tagList(
      showModal(modalDialog(
        title = selected_module_name$name,
        top = 10,
        easyClose = TRUE,
        size = "l",
        fluidPage(
          tabsetPanel(id = ns("tabs"),
                      type = "tabs",
                      tabPanel(title = "Module genes", 
                               DT::dataTableOutput(ns("module_genes_table"))),
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
          tags$h3("Change the threshold for specific Theta", style = "color:#2c3e50"), 
          sliderInput(ns("specific_theta"),
                      label = "Specific Theta",
                      min = 0.001,
                      max = 1,
                      value = 0.05),
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
  
  settings <- as.matrix(inspected_module$settings)
  settings[1] <- as.character(settings[1])
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
    MODA_post_process$post_process_module_object <- post_process_module_object
    id <- showNotification("Saving module objects to database", duration = NULL, closeButton = FALSE, type = "warning")
    
    module_name <- paste(selected_module_name$name, 
                         "adjusted_specific_theta",
                         Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
    
    moda_changed_specific_threshold <- MODifieR::moda_change_specific_threshold(inspected_module,
                                                                                specificTheta = input$specific_theta)
    
    try(MODifieRDB::MODifieR_object_to_db(moda_changed_specific_threshold,
                                          object_name = module_name,
                                          con = con))
    
    on.exit(removeModal())
    on.exit(removeNotification(id), add = TRUE)
  })
  
  return(MODA_post_process)
}
    
## To be copied in the UI
# mod_MODA_post_processing_ui("MODA_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_MODA_post_processing_server, "MODA_post_processing_ui_1")
 
