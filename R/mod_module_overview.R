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
    DT::dataTableOutput(ns("module_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-8", style = "color:black",
             fileInput(ns("module_object"), label = "Upload a module object", accept =  ".rds"),
             actionButton(ns("inspect"), label = "Inspect the module object"),
             uiOutput(ns("module_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-4", style = "text-align:right", id ="buttons_module_overview",
                      downloadButton(ns("download_module"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true")))),
    uiOutput(ns("inspected_results")),
    uiOutput(ns("disable"))
  )
}

#' module_overview Server Function
#'
#' @noRd 
mod_module_overview_server <- function(input, output, session, con, Columns_ui_1){
  ns <- session$ns
  module_overview_module <- reactiveValues()
  
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
  
  # Inspect current module
  observeEvent(input$inspect, {
    
    selected <- input$module_overview_rows_selected
    
    if (length(selected) > 1) {
      showNotification("Sorry, you can only inspect one object at a time", duration = NULL, closeButton = TRUE, type = "warning")
    } else if (length(selected) == 1 ) {
      inspected_module <<- MODifieRDB::MODifieR_module_from_db(module_objects$module_name[selected], con = con)
      selected_module_name <<- module_objects$module_name[selected]
      
      if (is.null(inspected_module)) { # Selected is not NULL here, not sure why. 
        showNotification("No module object selected", duration = 10, closeButton = TRUE, type = "warning") 
      } else {
        #Function, in the fct_functions.R, to call the different module objects tables.
        output$inspected_results <- inspect_module(inspected_module, selected_module_name, ns, con) 
      }
      
    }
    
  })
  return(module_overview_module)
}

## To be copied in the UI
# mod_module_overview_ui("module_overview_ui_1")

## To be copied in the server
# callModule(mod_module_overview_server, "module_overview_ui_1")
