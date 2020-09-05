#' enrichment_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(style= "margin-left: 10px; margin-right: 10px",
             tags$h1(style= "color: #2b3e50; ", "Enrichment Objects"),
             actionLink(inputId = "information_btn_enrichment", label = "Learn More"),
             tags$br(),
             tags$br(),
    DT::dataTableOutput(ns("enrichment_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-10", style = "color:black",
                      fileInput(ns("enrichment_object"), label = "Upload an enrichment object", accept =  ".rds"),
                      uiOutput(ns("enrichment_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-2", style = "text-align:right", id ="buttons_enrichment_overview",
                      downloadButton(ns("download_enrichment"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true"))))
  ),
  uiOutput(ns("DT_tooltip"))
  )
}

#' enrichment_overview Server Function
#'
#' @noRd 
mod_enrichment_overview_server <- function(input, output, session, con, main_page_v2_module, app_servr){
  ns <- session$ns
  
  enrichment_overview_module <- reactiveValues()
  
  # Create a table
  enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
  output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                    rownames = FALSE,
                                                    selection = list(selected = c(1)),
                                                    callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_module_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });
                                                              Shiny.setInputValue("DT_tooltip2", "DT_tooltip2");
                                                                      '))
  
  #Reactive funciton for fileinput
  upload_enrichment <- reactive({
    req(input$enrichment_object)
    infile <- (input$enrichment_object$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    readRDS(file = infile)
  })
  
  output$enrichment_name_chooser <- renderUI({
    module <- upload_enrichment() #reactive pop up
    tagList( 
      textInput(ns("enrichment_module_name"), "Enrichment module name", placeholder = "Enrichment module name"),
      actionButton(ns("upload_enrichment"), "Add enrichment object to database")
    )
  })
  
  #Name reactive
  
  enrichment_module_name <- reactive({
    input$enrichment_overview_rows_selected #This will be NULL if no row is selected during double click
  })
  
  # Upload enrichment
  observeEvent(input$upload_enrichment, {
    id <- showNotification("Saving module object to database", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    enrichment <- upload_enrichment()
    
    # MODifieRDB::enrichment_object_to_db(enrichment_object = enrichment,
    #                                     module_name = module_name, 
    #                                     enrichment_method = enrichment@ontology, 
    #                                     con = con)
    
    # Refresh
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_module_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });'))
  })
  
  # Render DT
  observeEvent(main_page_v2_module$enrich, {
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_module_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });'))
  })
  
  retrieve_enrichment_object <- function(){
    selected <- input$enrichment_overview_rows_selected
    if (length(selected) > 1){
      # Choose multiple options
      current_enrichment_objects <- function() {
        selected <- input$enrichment_overview_overview_rows_selected
      }
      lapply(current_enrichment_objects(), MODifieRDB::enrichment_object_from_db, con = con)
    } else {
      MODifieRDB::enrichment_object_from_db(selected, con)
    }
  }
  
  # Download function
  output$download_enrichment <- downloadHandler(
    filename = function() {
      paste0("enrichment_set_", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(retrieve_enrichment_object(), file)
    }
  )
  
  # Observe if valid to download
  observe({
    if(is.null(input$enrichment_overview_rows_selected)) {
      shinyjs::disable("download_enrichment")
      shinyjs::disable("delete")
    } else {
      shinyjs::enable("download_enrichment")
      shinyjs::enable("delete")
    }
  })
  
  # Delete enrichment object
  observeEvent(input$delete, {
    id <- showNotification("Deleting", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Required for selecting
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_module_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });'))
    
    
    # Delete
    selected <- input$enrichment_overview_rows_selected
    if (length(selected) > 1){
      # Choose multiple options
      current_enrichment_objects <- function() {
        selected <- input$enrichment_overview_overview_rows_selected
      }
      lapply(current_enrichment_objects(), MODifieRDB::delete_enrichment_object, con = con)
    } else {
      MODifieRDB::delete_enrichment_object(enrichment_objects$module_name[selected], con = con) #Tried with selected as well, doesn't work either, probably error in the DB package.
    }
    
    # Refresh
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_module_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });
                                                              '))
    
    # Send refresh to Description1_ui_1
    enrichment_overview_module$delete <- input$delete
  })
  
  #Observe when DT is loaded 
  observeEvent(app_servr$DT_tooltip2, {
    output$DT_tooltip <- renderUI({
      tags$script('
                  $("#main_page_v2_ui_1-enrichment_overview_ui_1-enrichment_overview").find("tr").eq(1).attr("id", "DT_tooltip2");
                  Tipped.create("#DT_tooltip2",
                  "Double-click me to inspect the object!",
                  {shadow: false});
                  Tipped.show("#DT_tooltip2");
                  ')
    })
  })
  
  observeEvent(app_servr$enrichment_module_dbclick, {
    MODifieRDB::enrichment_object_from_db(selected, con)
    enrichment_module <<- MODifieRDB::enrichment_object_from_db(app_servr$enrichent_module_name, con = con)
    
    
    
  })
  
  return(enrichment_overview_module)
}


## To be copied in the UI
# mod_enrichment_overview_ui("enrichment_overview_ui_1")

## To be copied in the server
# callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1")

