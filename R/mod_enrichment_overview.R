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
  uiOutput(ns("inspect")),
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
  enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
  output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                    rownames = FALSE,
                                                    selection = list(selected = c(1)),
                                                    callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_name", data[0]);
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
  
  # Upload enrichment
  x <- reactiveVal(1)
  observeEvent(input$upload_enrichment, {
    id <- showNotification("Saving module object to database", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    enrichment <- upload_enrichment()
    
    MODifieRDB::enrichment_object_to_db(enrichment_object = enrichment,
                                        module_name = NULL, 
                                        enrichment_method = enrichment@ontology, 
                                        enrichment_name = input$enrichment_module_name,
                                        con = con)
    
    # Refresh
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });'))
    # Send refresh to Description1_ui_1
    x(x() + 1)
    enrichment_overview_module$upload <- x()
  })
  
  # Render DT
  observeEvent(main_page_v2_module$enrich, {
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });'))
  })
  
  retrieve_enrichment_object <- function(){
    selected <- input$enrichment_overview_rows_selected
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    if (length(selected) > 1){
      # Choose multiple options
      current_enrichment_objects <- function() {
        selected <- input$enrichment_overview_rows_selected
        enrichment_objects$enrichment_name[selected]
      }
      lapply(current_enrichment_objects(), MODifieRDB::enrichment_object_from_db, con = con)
    } else {
      MODifieRDB::enrichment_object_from_db(enrichment_objects$enrichment_name[selected], con)
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
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });'))
    
    
    # Delete
    selected <- input$enrichment_overview_rows_selected
    if (length(selected) > 1){
      # Choose multiple options
      current_enrichment_objects <- function() {
        selected <- input$enrichment_overview_rows_selected
        enrichment_objects$enrichment_name[selected]
      }
      lapply(current_enrichment_objects(), MODifieRDB::delete_enrichment_object, con = con)
    } else {
      MODifieRDB::delete_enrichment_object(enrichment_objects$enrichment_name[selected], con = con) #Tried with selected as well, doesn't work either, probably error in the DB package.
    }
    
    # Refresh
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    output$enrichment_overview <- DT::renderDataTable(enrichment_objects,
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("enrichment_name", data[0]);
                                                              Shiny.setInputValue("enrichment_module_dbclick", dbclick);
                                                             });
                                                              '))
    
    # Send refresh to Description1_ui_1
    enrichment_overview_module$delete <- input$delete
  })
  
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
    enrichment_module <- MODifieRDB::enrichment_object_from_db(app_servr$enrichment_name, con)
    output$inspect <- renderUI({
      tagList(
        showModal(modalDialog(
          top = 20,
          title = app_servr$enrichment_name,
          easyClose = TRUE,
          size = "l",
          fluidPage(
            tabsetPanel(id = ns("tabs"),
                        type = "tabs",
                        tabPanel(title = "Settings",
                                 DT::dataTableOutput(ns("settings")))),
            rep_br(2)),
          footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
          )
        ))
      )
    })
    output$settings <- DT::renderDataTable(
      {DT <- data.frame("Values" = c(enrichment_module@pvalueCutoff, enrichment_module@pAdjustMethod, enrichment_module@organism, enrichment_module@ontology, enrichment_module@keytype))
      row.names(DT) <- c("pvalueCutoff", "pAdjustMethod", "Organism", "Ontology", "Keytype")
      as.data.frame(DT)
      },
      extensions = c('Buttons'),
      options = list(
        dom = "lfrtipB",
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 10,
        paging = TRUE,
        searching = TRUE,
        lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")),
        buttons = 
          list('copy', 
               list(
                 extend = 'collection',
                 buttons = c('pdf', 'csv', 'excel'),
                 text = 'Download'
               ))
      )
    )
    
  })
  
  return(enrichment_overview_module)
}


## To be copied in the UI
# mod_enrichment_overview_ui("enrichment_overview_ui_1")

## To be copied in the server
# callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1")

