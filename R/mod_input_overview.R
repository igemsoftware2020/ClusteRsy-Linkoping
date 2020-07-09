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
    DT::dataTableOutput(ns("input_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-8", style = "color:black",
             fileInput(ns("input_object"), label = "Upload an input object"),
             uiOutput(ns("input_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-4", style = "text-align:right",
                      downloadButton(ns("download_input"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true"))))
  )
}

#' input_overview Server Function
#'
#' @noRd 
mod_input_overview_server <- function(input, output, session, con, Columns_ui_1){
  ns <- session$ns
  
  # Reactive function for fileinput
  upload_input <- reactive({
    req(input$input_object)
    infile <- (input$input_object$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    read.table(file = infile, header = T)
  })
  
  # File input
  output$input_name_chooser <- renderUI({
    input <- upload_input() #reactive
    tagList( 
      textInput(ns("input_name"), "Input object name"),
      actionButton(ns("upload_input"), "Add input object to database")
    )
  })
  
  # Upload input object
  observeEvent(input$upload_input, {
    id <- showNotification("Saving input object to database", duration = NULL, closeButton = FALSE)
    input <- upload_input()
    input_name <- input$input_name
    on.exit(removeNotification(id), add = TRUE)
    
    MODifieRDB::MODifieR_object_to_db(MODifieR_object = input,
                                      object_name = input_name,
                                      con = con)
    
  })
  
  input_objects <- MODifieRDB::get_available_input_objects(con)
  
  # Render DT
  output$input_overview <- DT::renderDataTable(input_objects,
                                               selection = list(selected = c(1)))
  
  # Refresh DT
  observeEvent(Columns_ui_1$input_name, {
    print("Hello")
    input_objects <- MODifieRDB::get_available_input_objects(con)
    
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 selection = list(selected = c(1)))
  })
  
  # Download function
  output$download_input <- downloadHandler(
    filename = function() {
      paste0("input_set_", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(retrieve_input(), file)
    }
  )
  
  # Choose multiple options
  current_inputs <- function() {
    selected <- input$input_overview_rows_selected
    input_objects$input_name[selected]
  }
  
  retrieve_input <- function(){
    selected <- input$input_overview_rows_selected
    if (length(selected) > 1){
      lapply(current_modules(), MODifieRDB::MODifieR_input_from_db, con = con)
    } else {
      MODifieRDB::MODifieR_input_from_db(input_objects$input_name[selected], con = con)
    }
  }
  
  # Delete input object
  observeEvent(input$delete, {
    id <- showNotification("Deleting", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Required for selecting
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 selection = list(selected = c(1)))
    
    
    # Delete
    selected <- input$input_overview_rows_selected
    if (length(selected) > 1){
      lapply(current_inputs(), MODifieRDB::delete_input_object, con = con)
    } else {
      MODifieRDB::delete_input_object(input_objects$input_name[selected] ,con = con)
    }
    
    # Refresh
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                  selection = list(selected = c(1)))
  })
 
}

## To be copied in the UI
# mod_input_overview_ui("input_overview_ui_1")

## To be copied in the server
# callModule(mod_input_overview_server, "input_overview_ui_1")
 
