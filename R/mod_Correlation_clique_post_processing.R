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
    uiOutput(ns("tables")),
    uiOutput(ns("post_processing"))
    )
}
    
#' Correlation_clique_post_processing Server Function
#'
#' @noRd 
mod_Correlation_clique_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, inspect_button, post_process_button, con){
  ns <- session$ns
  
  Correlation_clique_post_process <- reactiveValues()
  
  observeEvent(inspect_button, {
    req(is.null(post_process_button))
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
  
  })
  
  observeEvent(post_process_button, {
    req(is.null(inspect_button))
    output$post_processing <- renderUI({
      tagList(
        showModal(modalDialog(
          title = selected_module_name$name,
          easyClose = TRUE,
          size = "l",
          tags$h3("Choose between adjusting the frequency cutoff or adjusting the module size", style = "color:#2c3e50"),
          selectInput(ns("post_process_choices"),
                      label = "Pick a post-processing method",
                      choices = c("Adjust frequency cutoff",
                                  "Adjust module size")),
          uiOutput(ns("selected_method")),
          actionButton(ns("post_process_module_object"), 
                       label = "Process module"),
          footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
          )
        
        ))
      )
    })
    
  
    output$selected_method <- renderUI({
      if (input$post_process_choices == "Adjust frequency cutoff") {
        print("freq")
        tagList(
          tags$p("This method will allow you to change the fraction of the number of times a gene should be present in it's iterations.", style = "color:#2c3e50"),
          tags$p("The default is set to 0.5, meaning the gene has to be present in at least 50 percent of iterations", style = "color:#2c3e50"),
          tags$br(),
          sliderInput(ns("frequency_cutoff"), 
                      label = "Frequency Cuttoff",
                      min = 0,
                      max = 1,
                      value = 0.5),
        )
      } else if (input$post_process_choices == "Adjust module size") {
        print("module")
        tagList(
          tags$p("This method will allow you to change the module size", style = "color:#2c3e50"),
          tags$br(),
          sliderInput(ns("module_size"),
                      label = "Module size",
                      min = 1,
                      max = length(module_genes),
                      value = (length(module_genes))/2),
        )
      }
    })
  })
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  frequency_table <- as.matrix(inspected_module$frequency_table)
  colnames(frequency_table) <- list("Frequency")
  
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
  
  output$frequency_table <- DT::renderDataTable({frequency_table},
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
    Correlation_clique_post_process$post_process_module_object <- post_process_module_object
    id <- showNotification("Saving module objects to database", duration = NULL, closeButton = FALSE, type = "warning")
    
    if (input$post_process_choices == "Adjust frequency cutoff") {

      correlation_adjust_cutoff <- MODifieR::correlation_adjust_cutoff(frequency_cutoff =  input$frequency_cutoff,
                                                                       inspected_module)
      module_name <- paste(selected_module_name$name, "adjusted_frequency_cutoff", paste0(sample(letters, size = 10), collapse = ""), sep = "_") %>% 
        gsub(" ", "_", .)
      
      
      print(class(module_name))
      print(module_name)
      try(MODifieRDB::MODifieR_object_to_db(correlation_adjust_cutoff,
                                            object_name =  module_name,
                                            con = con))
      
    } else if (input$post_process_choices == "Adjust module size") {

      correlation_set_module_size <- MODifieR::correlation_set_module_size(size = input$module_size,
                                                                           inspected_module)
      
      module_name <- paste(selected_module_name$name, "adjusted_module_size", Sys.date(), sep = "_") %>% 
        gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(correlation_set_module_size,
                                            object_name =  module_name,
                                            con = con))
    }
    
    on.exit(removeModal())
    on.exit(removeNotification(id), add = TRUE)
  })
  
  return(Correlation_clique_post_process)
 
}
    
## To be copied in the UI
# mod_Correlation_clique_post_processing_ui("Correlation_clique_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Correlation_clique_post_processing_server, "Correlation_clique_post_processing_ui_1")
 
