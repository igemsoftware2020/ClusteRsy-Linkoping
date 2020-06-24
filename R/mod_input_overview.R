#' input_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("input_object"), label = "Upload an input object"),
    uiOutput(ns("input_name_chooser")),
    actionButton(ns("refresh"), "Refresh database"),
    tags$br(),
    tags$br(),
    DT::dataTableOutput(ns("input_overview")),
  )
}

#' input_overview Server Function
#'
#' @noRd 
mod_input_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
  upload_input <- reactive({
    req(input$input_object)
    infile <- (input$input_object$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    read.table(file = infile, header = T)
  })
  
  output$module_name_chooser <- renderUI({
    input <- upload_input()
    tagList( 
      textInput(ns("input_name"), "Input object name"),
      actionButton(ns("upload_input"), "Add input object to database")
    )
  })
  
  observeEvent(input$upload_input, {
    id <- showNotification("Saving input object to database", duration = NULL, closeButton = FALSE)
    input <- upload_input()
    input_name <- input$input_name
    on.exit(removeNotification(id), add = TRUE)
    
    #MODifieRDB::ppi_network_to_db(ppi_network = ppi, ppi_name = ppi_name, con = con) 
    ## Need to implement in the package
    
  })
  
  input_objects <- MODifieRDB::get_available_input_objects(con)
  
  output$input_overview <- DT::renderDataTable(input_objects)
  
  observeEvent(input$refresh, {
    input_objects <- MODifieRDB::get_available_input_objects(con)
    
    output$input_overview <- DT::renderDataTable(input_objects)
  })
 
}

## To be copied in the UI
# mod_input_overview_ui("input_overview_ui_1")

## To be copied in the server
# callModule(mod_input_overview_server, "input_overview_ui_1")
 
