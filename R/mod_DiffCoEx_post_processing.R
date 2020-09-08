#' DiffCoEx_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DiffCoEx_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
  uiOutput(ns("tables")),
  uiOutput(ns("post_processing"))
  )
}
    
#' DiffCoEx_post_processing Server Function
#'
#' @noRd 
mod_DiffCoEx_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, inspect_button, post_process_button, con){
  ns <- session$ns
  
  DiffCoEx_post_process <- reactiveValues()
  
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
                      tabPanel(title = "Module P-values table",
                               DT::dataTableOutput(ns("module_p_values_table"))),
                      tabPanel(title = "Colored vector table",
                               DT::dataTableOutput(ns("color_vector_table"))),
                      tabPanel(title = "Settings table",
                               DT::dataTableOutput(ns("settings_table")))),
          rep_br(2)),
        footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
        )
        
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
          top = 20,
          tags$h4("Split module by color", style = "color:#2c3e50; text-align:center;"),
          tags$p("This method will split the original DiffCoEx object into a series of DiffCoEx objects by color.", style = "color:#2c3e50; text-align:center;"),
          tags$p("Every significant color in the module will be it's own DiffCoEx module object", style = "color:#2c3e50; text-align:center;"),
          tags$br(),
          actionButton(ns("post_process_module_object"),
                       label = "Split module"),
          footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
          )
          
        ))
      )
    })
  })
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  module_p_values <- inspected_module$module_p_values
  
  color_vector <- data.frame(inspected_module$color_vector, names(inspected_module$color_vector))
  colnames(color_vector) <- list("Gene", "Color")
  
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
  
  output$module_p_values_table <- DT::renderDataTable({module_p_values},
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
 
  output$color_vector_table <- DT::renderDataTable({color_vector},
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
    DiffCoEx_post_process$post_process_module_object <- post_process_module_object
    
    split_module <- MODifieR::diffcoex_split_module_by_color(inspected_module)

    for (i in 1:length(split_module)) {
      
      if (i == 1) {
        id <- showNotification("Saving module objects to database", duration = NULL, closeButton = FALSE, type = "warning")
      }
      
      module_name <- paste(selected_module_name$name, 
                           names(split_module[i]), 
                           Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
      
      split_module_input <- split_module[[i]]

      try(MODifieRDB::MODifieR_object_to_db(split_module_input,
                                        object_name =  module_name,
                                        con = con))
      
      
    }
    
    on.exit(removeModal())
    on.exit(removeNotification(id), add = TRUE)
    
    })
  

  
  return(DiffCoEx_post_process)
}


    
## To be copied in the UI
# mod_DiffCoEx_post_processing_ui("DiffCoEx_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_DiffCoEx_post_processing_server, "DiffCoEx_post_processing_ui_1")
 
