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
    tags$div(style= "margin-left: 10px; margin-right: 10px",
             tags$h1(style= "color: #2b3e50", "Input Objects"),
             actionLink(inputId = "information_btn_input", label = "Learn More"),
             tags$br(),
             tags$br(),
    DT::dataTableOutput(ns("input_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-10", style = "color:black",
             fileInput(ns("input_object"), label = "Upload an input object", accept = ".rds"),
             uiOutput(ns("input_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-2", style = "text-align:right", id ="buttons_input_overview",
                      downloadButton(ns("download_input"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true")))),
    uiOutput(ns("inspect")),
    uiOutput(ns("DT_tooltip"))
  ))
}

#' input_overview Server Function
#'
#' @noRd 
mod_input_overview_server <- function(input, output, session, con, Columns_ui_1, app_servr){
  ns <- session$ns
  input_overview_module <- reactiveValues()
  
  # Reactive function for fileinput
  upload_input <- reactive({
    req(input$input_object)
    infile <- (input$input_object$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    readRDS(file = infile)
  })
  
  # File input
  output$input_name_chooser <- renderUI({
    input <- upload_input() #reactive pop up
    tagList( 
      textInput(ns("input_name"), "Input object name", placeholder = "Input name"),
      actionButton(ns("upload_input"), "Add input object to database")
    )
  })
  
  # Name reactive
  input_name <- reactive({
    input$input_name
  })
  
  # Upload input object
  observeEvent(input$upload_input, {
    id <- showNotification("Saving input object to database", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    input <- upload_input()
    input_name <- input_name()
    
    MODifieRDB::MODifieR_object_to_db(MODifieR_object = input,
                                      object_name = input_name,
                                      con = con)
    
    # Refresh
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 rownames = FALSE,
                                                 selection = list(selected = c(1)),
                                                 callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("input_name", data[0]);
                                                              Shiny.setInputValue("input_dbclick", dbclick);
                                                             });'))
    
  })
  
  input_objects <- MODifieRDB::get_available_input_objects(con)
  
  # Render DT
  output$input_overview <- DT::renderDataTable(input_objects,
                                               rownames = FALSE,
                                               selection = list(selected = c(1)),
                                               callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("input_name", data[0]);
                                                              Shiny.setInputValue("input_dbclick", dbclick);
                                                             });
                                                             Shiny.setInputValue("DT_tooltip", "DT_tooltip");
                                                                 '))
  
  # Observer when DT is loaded
  observeEvent(app_servr$DT_tooltip, {
    output$DT_tooltip <- renderUI({
      tags$script('
                  $("#main_page_v2_ui_1-input_overview_ui_1-input_overview").find("tr").eq(1).attr("id", "DT_tooltip");
                  Tipped.create("#DT_tooltip",
                  "Double-click me to inspect the object!",
                  {shadow: false});
                  Tipped.show("#DT_tooltip");
                  ')
    })
  })
  
  # Refresh DT
  observeEvent(Columns_ui_1$input_name, {
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 rownames = FALSE,
                                                 selection = list(selected = c(1)),
                                                  callback = DT::JS('
                                                             table.on("dblclick.dt","tr", function() {
                                                               var data=table.row(this).data();;
                                                               dbclick++;
                                                               Shiny.setInputValue("input_name", data[0]);
                                                               Shiny.setInputValue("input_dbclick", dbclick);
                                                              });
                                                                    ')
                                                 )
  })
  
  # Refresh DT
  observeEvent(Columns_ui_1$upload_input_rds, {
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 rownames = FALSE,
                                                 selection = list(selected = c(1)),
                                                 callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("input_name", data[0]);
                                                              Shiny.setInputValue("input_dbclick", dbclick);
                                                             });'))
  })
  
  retrieve_input <- function(){
    selected <- input$input_overview_rows_selected
    if (length(selected) > 1){
      # Choose multiple options
      current_inputs <- function() {
        selected <- input$input_overview_rows_selected
        input_objects$input_name[selected]
      }
      lapply(current_inputs(), MODifieRDB::MODifieR_input_from_db, con = con)
    } else {
      MODifieRDB::MODifieR_input_from_db(input_objects$input_name[selected], con = con)
    }
  }
  
  # Download function
  output$download_input <- downloadHandler(
    filename = function() {
      paste0("input_set_", Sys.Date(), ".rds", sep="")
    },
    content = function(file) {
      saveRDS(retrieve_input(), file)
    }
  )
  
  observe({
    if(is.null(input$input_overview_rows_selected)) {
      shinyjs::disable("download_input")
      shinyjs::disable("delete")
    } else {
      shinyjs::enable("download_input")
      shinyjs::enable("delete")
    }
  })
  
  # Delete input object
  observeEvent(input$delete, {
    id <- showNotification("Deleting", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Required for selecting
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 rownames = FALSE,
                                                 selection = list(selected = c(1)),
                                                 callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("input_name", data[0]);
                                                              Shiny.setInputValue("input_dbclick", dbclick);
                                                             });'))
    
    
    # Delete
    selected <- input$input_overview_rows_selected
    if (length(selected) > 1){
      # Choose multiple options
      current_inputs <- function() {
        selected <- input$input_overview_rows_selected
        input_objects$input_name[selected]
      }
      lapply(current_inputs(), MODifieRDB::delete_input_object, con = con)
    } else {
      MODifieRDB::delete_input_object(input_objects$input_name[selected] ,con = con)
    }
    
    # Refresh
    input_objects <- MODifieRDB::get_available_input_objects(con)
    output$input_overview <- DT::renderDataTable(input_objects,
                                                 rownames = FALSE,
                                                 selection = list(selected = c(1)),
                                                 callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("input_name", data[0]);
                                                              Shiny.setInputValue("input_dbclick", dbclick);
                                                             });'))
    
    # Send refresh to Description1_ui_1
    input_overview_module$delete <- input$delete
  })
  
  # Observer doubleclick
  observeEvent(app_servr$input_dbclick, {
    input_obj <- MODifieRDB::MODifieR_input_from_db(input_name = app_servr$input_name, con = con)
    output$inspect <- renderUI({
      tagList(
        showModal(modalDialog(
          top = 8,
          title = app_servr$input_name,
          easyClose = TRUE,
          size = "l",
          fluidPage(
            tabsetPanel(id = ns("tabs"),
                        type = "tabs",
                        tabPanel(title = "Result",
                                 DT::dataTableOutput(ns("result"))),
                        tabPanel(title = "Settings",
                                 DT::dataTableOutput(ns("settings")))),
            rep_br(2)),
          footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
          )
        ))
      )
    })
    output$result <- DT::renderDataTable(
      input_obj$edgeR_deg_table,
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
               )),
        rowCallback = DT::JS(
          "function(row, data) {",
          "for (i = 1; i < data.length; i++) {",
          "$('td:eq('+i+')', row).html(data[i].toExponential(3));",
          "}",
          "}")
      )
    )
    output$settings <- DT::renderDataTable(
      {DT <- as.matrix(input_obj$settings[2:7])
      colnames(DT) <- "Values"},
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
      )
    )
  })
  
  
  return(input_overview_module)
}

## To be copied in the UI
# mod_input_overview_ui("input_overview_ui_1")

## To be copied in the server
# callModule(mod_input_overview_server, "input_overview_ui_1")
 
