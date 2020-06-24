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
    fileInput(ns("module_object"), label = "Upload a module object"),
    uiOutput(ns("module_name_chooser")),
    actionButton(ns("refresh"), "Refresh database"),
    tags$br(),
    tags$br(),
    DT::dataTableOutput(ns("module_overview")),
    downloadButton(ns("download_module"), "Download")
  )
}

#' module_overview Server Function
#'
#' @noRd 
mod_module_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
  upload_module <- reactive({
    req(input$module_object)
    infile <- (input$module_object$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    read.table(file = infile, header = T)
  })
  
  output$module_name_chooser <- renderUI({
    module <- upload_module()
    tagList( 
      textInput(ns("module_name"), "Module object name"),
      actionButton(ns("upload_module"), "Add module object to database")
    )
  })
  
  observeEvent(input$upload_module, {
    id <- showNotification("Saving module object to database", duration = NULL, closeButton = FALSE)
    module <- upload_module()
    module_name <- input$module_name
    on.exit(removeNotification(id), add = TRUE)
    
    #MODifieRDB::ppi_network_to_db(ppi_network = ppi, ppi_name = ppi_name, con = con) 
    ## Need to implement in the package
    
  })
  
  module_objects <- MODifieRDB::get_available_module_objects(con)
  
  output$module_overview <- DT::renderDataTable(module_objects)
  
  observeEvent(input$refresh, {
    module_objects <- MODifieRDB::get_available_module_objects(con)
    
    output$module_overview <- DT::renderDataTable(module_objects)
  })
  
  output$download_module <- downloadHandler(
    filename = function() {
      paste0("module_set", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(retrieve_module(), file)
    }
  )
  
  current_modules <- function() {
    module_objects$module_name[input$module_overview_rows_selected]
  }
  
  
  retrieve_module <- function(){
    if (length(input$module_overview_rows_selected) > 1){
      lapply(current_modules(), MODifieRDB::MODifieR_module_from_db, con = con)
    } else {
      MODifieRDB::MODifieR_module_from_db(module_objects$module_name[input$module_overview_rows_selected], con = con)
    }
    
  }
}

## To be copied in the UI
# mod_module_overview_ui("module_overview_ui_1")

## To be copied in the server
# callModule(mod_module_overview_server, "module_overview_ui_1")
