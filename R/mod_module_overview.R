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
             uiOutput(ns("module_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-4", style = "text-align:right",
                      actionButton(ns("refresh"), HTML("<i class='fa fa-refresh' aria-hidden='true'></i> Refresh")),
                      downloadButton(ns("download_module"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true"))))
  )
}

#' module_overview Server Function
#'
#' @noRd 
mod_module_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
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
  })
  
  module_objects <- MODifieRDB::get_available_module_objects(con)
  
  # Render DT
  output$module_overview <- DT::renderDataTable(module_objects,
                                                selection = list(selected = c(1)))
  
  # Refresh DT
  observeEvent(input$refresh, {
    module_objects <- MODifieRDB::get_available_module_objects(con)
    
    output$module_overview <- DT::renderDataTable(module_objects,
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
  
  # Delete module object
  observeEvent(input$delete, {
    id <- showNotification("Deleting", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Required for selecting
    module_objects <- MODifieRDB::get_available_module_objects(con)
    output$module_overview <- DT::renderDataTable(module_objects,
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
                                                  selection = list(selected = c(1)))
  })
  
  
}

## To be copied in the UI
# mod_module_overview_ui("module_overview_ui_1")

## To be copied in the server
# callModule(mod_module_overview_server, "module_overview_ui_1")
