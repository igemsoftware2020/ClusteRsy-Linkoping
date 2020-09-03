#' WGCNA_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_WGCNA_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("tables")),
    uiOutput(ns("post_processing"))
  )
}
    
#' WGCNA_post_processing Server Function
#'
#' @noRd 
mod_WGCNA_post_processing_server <- function(input, output, session, inspected_module, selected_module_name, inspect_button, post_process_button, con){
  ns <- session$ns
  
  WGCNA_post_process <- reactiveValues()
  
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
                      tabPanel(title = "Info table",
                               DT::dataTableOutput(ns("info_table_table"))),
                      tabPanel(title = "Correlation to trait table",
                               DT::dataTableOutput(ns("correlation_to_trait_table_table"), width = 'auto')),
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
          tags$h3("WGCNA offers four different post-processing functions.", style = "color:#2c3e50"),
          tags$p("Adjust significance allows to adjust the p-value for the inclusion of co-expression modules into the final disease module. The boolean parameter unadjusted denotes if the adjusted (FDR correction) or unadjusted p-value is to be used", style = "color:#2c3e50"),
          tags$p("The Get Modules Genes by Sign function returns a module object that either consists of co-expression colors positively or negatively correlated to the trait.  To get only colors positively correlated to trait in the resulting module, set the Mode parameter to Positive and to get only negatively correlated colors set Mode to Negative.", style = "color:#2c3e50"),
          tags$p("In the wgcna algorithm, co-expression modules are denoted by color. The final disease module can be composed of multiple colors. The function Split Module by color allows to split each of these colors into separate MODifieR module objects. Only colors significantly associated to the trait will be used.", style = "color:#2c3e50"),
          tags$p("The last method post-processing function Module Size allows for changing the module size", style = "color:#2c3e50"),
          selectInput(ns("post_process_choices"),
                      label = "Pick a post-processing method",
                      choices = c("Adjust significance", 
                                  "Get Modules Genes by Sign",
                                  "Split Module by color",
                                  "Module Size")),
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
    if (input$post_process_choices == "Adjust significance") {
      tagList(
        sliderInput(ns("pval_cutoff"),
                    label = "P-value cut off",
                    max = 1,
                    min = 0.0001,
                    value = 0.005),
        prettySwitch(ns("use_unadjusted"),
                     label = "Adjusted p-value",
                     value = FALSE,
                     status = "warning",
                     popup = "Default is set to false meaning that a unadjusted p-value will be used")
      )
    } else if (input$post_process_choices == "Get Modules Genes by Sign") {
      tagList(
        radioButtons(ns("mode"),
                     label = "Select between positive and negative correlation",
                     choiceNames = c("Positive", "Negative"),
                     choiceValues = c("p", "n"))
      )
    } else if (input$post_process_choices == "Split Module by color") {
      tagList(
        tags$p("By pressing Process module the module will get splitted by color into submodules")
      )
    } else if (input$post_process_choices == "Module Size") {
      tagList(
        sliderInput(ns("size"),
                    label = "Select the module size",
                    min = 1,
                    max = length(inspected_module$module_genes),
                    value = 50)
      )
    }
    
  })
  
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  info_table <- inspected_module$info_table
  colnames(info_table) <- list("Gene", "Module Lable", "Module Color")
  
  correlation_to_train_table <- inspected_module$correlation_to_trait_table
  
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
  
  output$info_table_table <- DT::renderDataTable({info_table},
                                                 filter = "top",
                                                 rownames = FALSE,
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
  
  output$correlation_to_trait_table_table <- DT::renderDataTable({correlation_to_train_table},
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
    WGCNA_post_process$post_process_module_object <- post_process_module_object
    id <- showNotification("Saving module objects to database", duration = NULL, closeButton = FALSE, type = "warning")
    
    if (input$post_process_choices == "Adjust significance") {
      
      wgcna_adjust_significance <- MODifieR::wgcna_adjust_significance(pval_cutoff = input$pval_cutoff,
                                                                       wgcna_module = inspected_module,
                                                                       use_unadjusted = input$use_unadjusted)
      
      module_name <- module_name <- paste(selected_module_name$name, 
                                          "adjusted_significance",
                                          Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(wgcna_adjust_significance,
                                            object_name = module_name,
                                            con = con))
      
    } else if (input$post_process_choices == "Get Modules Genes by Sign") {
      
      wgcna_get_module_genes_by_sign <- MODifieR::wgcna_get_module_genes_by_sign(wgcna_module = inspected_module,
                                                                                  mode = input$mode)
      
      module_name <- module_name <- paste(selected_module_name$name, 
                                          "module_genes_by_sign",
                                          input$mode,
                                          Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(wgcna_get_module_genes_by_sign,
                                            object_name = module_name,
                                            con = con))
      
    } else if (input$post_process_choices == "Split Module by color") {
      
      wgcna_split_module_by_color <- MODifieR::wgcna_split_module_by_color(inspected_module)
      
      for (i in 1:length(wgcna_split_module_by_color)) {
        
        module_name <- paste(selected_module_name$name, 
                             "splitted_module_by_color",
                             i,
                             Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
        
        wgcna_split_module_by_color_modules <- wgcna_split_module_by_color[[i]]

        try(MODifieRDB::MODifieR_object_to_db(wgcna_split_module_by_color_modules,
                                              object_name =  module_name,
                                              con = con))
      }
      
    } else if (input$post_process_choices == "Module Size") {
      
      wgcna_set_module_size <- MODifieR::wgcna_set_module_size(size = input$size,
                                                               inspected_module)
      
      module_name <- paste(selected_module_name$name, 
                           "adjusted_module_size",
                           Sys.time(), sep = "_") %>%  gsub(" ", "_", .)
      
      try(MODifieRDB::MODifieR_object_to_db(wgcna_set_module_size,
                                            object_name =  module_name,
                                            con = con))
      
    }
    
    on.exit(removeModal())
    on.exit(removeNotification(id), add = TRUE)
  })
  
  return(WGCNA_post_process)
  
}
    
## To be copied in the UI
# mod_WGCNA_post_processing_ui("WGCNA_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_WGCNA_post_processing_server, "WGCNA_post_processing_ui_1")
 
