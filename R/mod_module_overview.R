#' module_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_module_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(style= "margin-left: 10px; margin-right: 10px",
            tags$h1(style= "color: #2b3e50; ", "Module Objects"),
            actionLink(inputId = "information_btn_module", label = "Learn More"),
            tags$br(),
            tags$br(),
    DT::dataTableOutput(ns("module_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-8", style = "color:black",
             fileInput(ns("module_object"), label = "Upload a module object", accept =  ".rds"),
             tags$div(
             uiOutput(ns("module_name_chooser")),
             tags$br()),
             tags$div(
             actionButton(ns("inspect"), label = "Inspect the module object")),
             tags$br(),
             tags$div(
             uiOutput(ns("post_process_ui")))
             ),
             tags$br(),
             
             tags$div(`class`="col-sm-4", style = "text-align:right", id ="buttons_module_overview",
                      downloadButton(ns("download_module_cytoscape"), "Cytoscape", onclick="loading_modal_open(); stopWatch();"),
                      downloadButton(ns("download_module"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true")),
                      htmlOutput(ns("close_loading_modal")) # Close modal with JS 
                      )),
    uiOutput(ns("inspected_results")),
    uiOutput(ns("disable")),
    shinyjs::useShinyjs()
  ))
}

#' module_overview Server Function
#'
#' @noRd 
mod_module_overview_server <- function(input, output, session, con, Columns_ui_1){
  ns <- session$ns
  module_overview_module <- reactiveValues()
  selected_module_name <- reactiveValues()
  
  #Reactive funciton for fileinput
  upload_module <- reactive({
    req(input$module_object)
    infile <- (input$module_object$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    readRDS(file = infile)
  })

  output$module_name_chooser <- renderUI({
    module <- upload_module() #reactive pop up
    tagList( 
      textInput(ns("module_name"), "Module object name", placeholder = "Module name"),
      actionButton(ns("upload_module"), "Add module object to database")
    )
  })
  
  # Name reactive
  module_name <- reactive({
    input$module_name
  })
  
  # Upload module
  observeEvent(input$upload_module, {
    id <- showNotification("Saving module object to database", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module <- upload_module()
    module_name <- module_name()
    
    
    MODifieRDB::MODifieR_object_to_db(MODifieR_object = module,
                                      object_name = module_name,
                                      con = con)
    
    # Refresh
    module_objects <- MODifieRDB::get_available_module_objects(con)
    output$module_overview <- DT::renderDataTable(module_objects,
                                                  rownames = FALSE,
                                                  selection = list(selected = c(1)))
  })
  
  module_objects <- MODifieRDB::get_available_module_objects(con)
  # Render DT
  output$module_overview <- DT::renderDataTable(module_objects,
                                                rownames = FALSE,
                                                selection = list(selected = c(1)))
  
  # Refresh DT
  observeEvent(Columns_ui_1$module_name, {
    module_objects <- MODifieRDB::get_available_module_objects(con)
    output$module_overview <- DT::renderDataTable(module_objects,
                                                  rownames = FALSE,
                                                  selection = list(selected = c(1)))
  })
  
  # Choose multiple options
  current_modules <- function() {
    selected <- input$module_overview_rows_selected
    module_objects$module_name[selected]
  }
  
  retrieve_module <- function(){
    selected <- input$module_overview_rows_selected
    if (length(selected) > 1){
      lapply(current_modules(), MODifieRDB::MODifieR_module_from_db, con = con)
    } else {
      MODifieRDB::MODifieR_module_from_db(module_objects$module_name[selected], con = con)
    }
  }
  
  # Download function
  output$download_module_cytoscape <- downloadHandler(
    filename = function() {
      paste0("module_genes_interaction.tsv")
    },
    content = function(file) {
      module_genes <- MODifieRDB::MODifieR_module_from_db(module_objects$module_name[input$module_overview_rows_selected], con = con)$module_genes
      PPI_network <- read.delim("./inst/app/www/PPI_network.txt")
      module_genes_list <- c()
      for(i in 1:length(module_genes)){
        module_genes_list <- append(module_genes_list, grep(module_genes[i], PPI_network[,1]))
      }
      # Close loading modal
      output$close_loading_modal <- renderUI({
        tags$script("loading_modal_close(); reset();")
      })
      write.table(PPI_network[module_genes_list,], file=file, quote=FALSE, sep='\t', row.names = F)
    }
  )
  
  
  output$download_module <- downloadHandler(
    filename = function() {
      paste0("module_set_", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(retrieve_module(), file)
    }
  )
  
  # Observe if valid to download
  observe({
    if(is.null(input$module_overview_rows_selected)) { 
     output$disable <- renderUI({
       tags$script((HTML("document.getElementById('main_page_v2_ui_1-module_overview_ui_1-download_module').style.pointerEvents = 'none';
                         document.getElementById('main_page_v2_ui_1-module_overview_ui_1-delete').style.pointerEvents = 'none';
                         document.getElementById('buttons_module_overview').style.cursor = 'not-allowed';")))
     }) 
    } else {
      output$disable <- renderUI({
        tags$script((HTML("document.getElementById('main_page_v2_ui_1-module_overview_ui_1-download_module').style.pointerEvents = 'auto';
                          document.getElementById('main_page_v2_ui_1-module_overview_ui_1-delete').style.pointerEvents = 'auto';
                          document.getElementById('buttons_module_overview').style.cursor = 'default';")))
      }) 
    }
  })
  
  # Delete module object
  observeEvent(input$delete, {
    id <- showNotification("Deleting", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Required for selecting
    module_objects <- MODifieRDB::get_available_module_objects(con)
    output$module_overview <- DT::renderDataTable(module_objects,
                                                  rownames = FALSE,
                                                  selection = list(selected = c(1)))
    
    # Delete
    selected <- input$module_overview_rows_selected
    if (length(selected) > 1){
      lapply(current_modules(), MODifieRDB::delete_module_object, con = con)
    } else {
      MODifieRDB::delete_module_object(module_objects$module_name[selected] ,con = con)
    }
    
    # Refresh
    module_objects <- MODifieRDB::get_available_module_objects(con)
    output$module_overview <- DT::renderDataTable(module_objects,
                                                  rownames = FALSE,
                                                  selection = list(selected = c(1)))
    # Send refresh
    module_overview_module$delete <- input$delete
  })
  
  output$post_process_ui <- renderUI({
    tagList(
      actionButton(ns("post_process"),
                   label = "Post-process"),
    )
    
  })
  
  # Inspect current module
  
  inspected_result_list <- reactiveValues()
  module_objects_inspected <- MODifieRDB::get_available_module_objects(con) #This value is needed in order to retrieve the actual contet that is being displayed in the DT
  
  #Refresh
  observeEvent(inspected_result_list$list$server_output$post_process_module_object, { 
    ##### Important: This needs too be double arrowed in order for it too work! #####
    module_objects_inspected <<- MODifieRDB::get_available_module_objects(con) 
    
    output$module_overview <- DT::renderDataTable(module_objects_inspected,
                                                  rownames = FALSE,
                                                  selection = list(selected = c(1)))
  })
  
  #Listening the the module_type that is being selected in the DT.
  #This observeEvent use module_objects due to observers for new input data above.
  observeEvent(module_objects$module_type[input$module_overview_rows_selected],{
    selected_module_type <- module_objects$module_type[input$module_overview_rows_selected]

    if (length(selected_module_type) == 1) {
      shinyjs::enable("inspect")
      shinyjs::enable("download_module_cytoscape")
    } else {
      shinyjs::disable("inspect")
      shinyjs::disable("download_module_cytoscape")
    }

  })
  
  
  observeEvent(input$inspect, {
    post_process_button <- NULL
    inspect_button <- 1
    
    module_objects_inspected <- MODifieRDB::get_available_module_objects(con) 
    selected <- input$module_overview_rows_selected
    inspected_module <- MODifieRDB::MODifieR_module_from_db(module_objects_inspected$module_name[selected], con = con)
    selected_module_name$name <- module_objects_inspected$module_name[selected]
    
    if (is.null(inspected_module)) {
      showNotification("No module objects in the database", duration = 10, closeButton = TRUE, type = "warning") 
    } else {
      inspected_result_list$list <- inspect_module(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con)
      
      output$inspected_results <- renderUI({
        inspected_result_list$list$ui_output
      })
  
      
    }
  })
  
#Post processing of current module
  
  #Listening the the module_type that is being selected in the DT.
  #This observeEvent use module_objects due to observers for new input data above.
  observeEvent(module_objects$module_type[input$module_overview_rows_selected],{
    
    selected_module_type <- module_objects$module_type[input$module_overview_rows_selected]
    
      if (length(selected_module_type) == 1) {
        req(selected_module_type %in% c("Correlation_clique", "DIAMOnD", "DiffCoEx", "Mcode", "MODA", "WGCNA"))
          shinyjs::enable("post_process")
       } else {
         shinyjs::disable("post_process")   
       }

  })
  
  
  observeEvent(input$post_process, {
    inspect_button <- NULL
    post_process_button <- 1
    
    selected <- input$module_overview_rows_selected
    module_objects_inspected <- MODifieRDB::get_available_module_objects(con)
    inspected_module <- MODifieRDB::MODifieR_module_from_db(module_objects_inspected$module_name[selected], con = con)
    selected_module_name$name <- module_objects_inspected$module_name[selected]
    
    if (is.null(inspected_module)) {
      showNotification("No module objects in the database", duration = 10, closeButton = TRUE, type = "warning") 
    } else {
      
      inspected_result_list$list <- inspect_module(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con)
      
      output$inspected_results <- renderUI({
        inspected_result_list$list$ui_output
      })


    }
    
  })
  
  return(module_overview_module)
}

## To be copied in the UI
# mod_module_overview_ui("module_overview_ui_1")

## To be copied in the server
# callModule(mod_module_overview_server, "module_overview_ui_1")
