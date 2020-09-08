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
        top = 10,
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
                               DT::dataTableOutput(ns("settings_table")))),
          rep_br(2)),
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
          top = 15, 
          tags$h3("Two post-processing functions are available for MCODE, you can either change the module score or split the module into different submodules above the cut off threshold", style = "color:#2c3e50"),
          selectInput(ns("post_process_choices"),
                      label = "Pick a post-processing method",
                      choices = c("Module Score", 
                                  "Split Module")),
          uiOutput(ns("selected_method")),
          actionButton(ns("post_process_module_object"), 
                       label = "Process module"),
          footer = tagList( tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
          )
        ))
      )
    })
  })
 
  output$selected_method <- renderUI({
   if (input$post_process_choices == "Module Score") {
     tagList(
       tags$p("Select a module score"),
       sliderInput(ns("module_score"),
                   label = "Module Scure",
                   min = 1,
                   max = 50,
                   value = 12)
     )
     
   } else if (input$post_process_choices == "Split Module") {
     tagList(
       tags$p("Select a cut off for the threshold"),
       sliderInput(ns("module_cutoff"),
                   label = "Module cut off",
                   min = 1,
                   max = 10,
                   value = 3)
     )
     
   }
  
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
    id <- showNotification("Saving module objects to database", duration = NULL, closeButton = FALSE, type = "warning")
    
    if (input$post_process_choices == "Module Score") {
      
      mcode_module_score <- MODifieR:::mcode_adjust_score(module_cutoff = input$module_score,
                                                         inspected_module)
      
      module_name <- paste(selected_module_name$name, 
                           "adjusted_module_score",
                           Sys.time(), 
                           sep = "_") %>%  gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(mcode_module_score,
                                            object_name =  module_name,
                                            con = con))
      
    } else if (input$post_process_choices == "Split Module") {
      
      mcode_split_modules <- MODifieR:::mcode_split_modules(module_cutoff = input$module_cutoff,
                                                           inspected_module)
      
      for (i in 1:length(mcode_split_modules)) {
        
        module_name <- paste(selected_module_name$name, 
                             "splitted_module",
                             i,
                             Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
        
        mcode_splitted_modules <- mcode_split_modules[[i]]
        
        try(MODifieRDB::MODifieR_object_to_db(mcode_splitted_modules,
                                             object_name =  module_name,
                                             con = con))
        
        
      }
    }
    
    on.exit(removeModal())
    on.exit(removeNotification(id), add = TRUE)
  })
  
  return(Mcode_post_process)
 
}

    
## To be copied in the UI
# mod_Mcode_post_processing_ui("Mcode_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Mcode_post_processing_server, "Mcode_post_processing_ui_1")
 
