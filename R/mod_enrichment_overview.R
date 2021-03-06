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
    shinyjs::useShinyjs(),
    tags$div(style= "margin-left: 10px; margin-right: 10px",
             tags$h1(style= "color: #2b3e50; ", "Enrichment Objects"),
             actionLink(inputId = "information_btn_enrichment", label = "Learn More"),
             tags$br(),
             tags$br(),
    DT::dataTableOutput(ns("enrichment_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-10", style = "color:black",
                      fileInput(ns("enrichment_object"), label = "Upload an enrichment object", accept =  ".rds", popup = "For now we only include single object upload. If you have downloaded a list of enrichment objects, please save them to multiple .rds files and upload one by one"),
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
  try(enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con))
  output$enrichment_overview <- DT::renderDataTable({enrichment_objects},
                                                    rownames = FALSE,
                                                    selection = list(selected = c(1)),
                                                    colnames = c("Enrichment name", "Module name", "Enrichment method"),
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
    enrichment <- upload_enrichment()
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    
    if (length(enrichment) > 1) {
      tags$p("It seems that you have tried to upload a list of enrichment objects, try to upload a single object instead.", style = "color:#eb4034")
    
    } else if (is.null(names(enrichment))) {
    tagList(
      showModal(modalDialog(
        top = 20,
        title = "Upload a new object to the database",
        easyClose = F,
        size = "l",
        tags$div(id = "error_name_enrichment_overview_js",
                 textInput(ns("enrichment_module_name"), "Select an enrichment module name", popup = "Enter a name for this object", placeholder = "Enrichment module name")),
        uiOutput(ns("error_name_descrip")),
        uiOutput(ns("error_name_js")),
        textInput(ns("module_name"), "What module was used to produce this object?", placeholder = "Module name"),
        textInput(ns("enrichment_type"), "What enrichment type was used to produce this object?", placeholder = "Enrichment type"),
        actionButton(ns("upload_enrichment"), "Add enrichment object to database"),
        footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
        )))
    )
    } else if (any(enrichment_objects$enrichment_name == names(enrichment))) {
      tagList(
        showModal(modalDialog(
          top = 20,
          title = paste("Upload an already existing object to the DB"),
          easyClose = F,
          size = "l",
          tags$h3("There was already an object in the database with this name, please select a new name below or delete the object already in the database"),
          tags$div(id = "error_name_enrichment_overview_js",
                  textInput(ns("enrichment_module_name"), "Enrichment module name", popup = "Enter a name for this object", placeholder = "Enrichment module name")),
          uiOutput(ns("error_name_descrip")),
          uiOutput(ns("error_name_js")),
          textInput(ns("module_name"), "What module was used to produce this object?", popup= "Name the MODifieR method that was used"),
          textInput(ns("enrichment_type"), "What enrichment type was used to produce this object?", popup = "Name the enrichment analysis that was used"),
          actionButton(ns("upload_enrichment"), "Add enrichment object to database"),
          footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
          )))
      )
    } else {
      tagList(
        showModal(modalDialog(
          top = 20,
          title = paste(names(enrichment), "are being loaded to the database"),
          easyClose = F,
          size = "l",
          textInput(ns("module_name"), "What module was used to produce this object?", placeholder = "Module name"),
          textInput(ns("enrichment_type"), "What enrichment type was used to produce this object?", placeholder = "Enrichment type"),
          actionButton(ns("upload_enrichment"), "Add enrichment object to database"),
          footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
          )))
      )
      
    }
    
  })
  
  # Upload enrichment
  x <- reactiveVal(1)
  observeEvent(input$upload_enrichment, {
    
    if (is.null(input$enrichment_module_name)) {
      id <- showNotification("Saving module object to database", duration = NULL, closeButton = FALSE, type = "warning")
      enrichment <- upload_enrichment()
      
      try(MODifieRDB::enrichment_object_to_db(enrichment_object = enrichment[[1]],
                                          module_name = input$module_name, 
                                          enrichment_method = input$enrichment_type, 
                                          enrichment_name = names(enrichment),
                                          con = con))
      on.exit(removeModal())
    } else {
    id <- showNotification("Saving module object to database", duration = NULL, closeButton = FALSE, type = "warning")
    enrichment <- upload_enrichment()
    
    try(MODifieRDB::enrichment_object_to_db(enrichment_object = enrichment[[1]],
                                        module_name = input$module_name, 
                                        enrichment_method = input$enrichment_type, 
                                        enrichment_name = input$enrichment_module_name,
                                        con = con))
    on.exit(removeModal())
    }
    
    on.exit(removeNotification(id), add = TRUE)
    # Refresh
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    output$enrichment_overview <- DT::renderDataTable({enrichment_objects},
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      colnames = c("Enrichment name", "Module name", "Enrichment method"),
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
  
  
  enrichment_name <- reactive({
    input$enrichment_module_name
  })
  
  # Check name
  observe({
    if (any(MODifieRDB::get_available_enrichment_objects(con)$enrichment_name == enrichment_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_enrichment_overview_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-enrichment_overview_ui_1-upload_enrichment').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_enrichment_overview_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-enrichment_overview_ui_1-upload_enrichment').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  }) 
  
  # Render DT
  observeEvent(main_page_v2_module$enrich, {
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    output$enrichment_overview <- DT::renderDataTable({enrichment_objects},
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      colnames = c("Enrichment name", "Module name", "Enrichment method"),
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
      modules <- lapply(current_enrichment_objects(), MODifieRDB::enrichment_object_from_db, con = con)
      names(modules) <- current_enrichment_objects()
      return(modules)
    } else {
      module <- list(MODifieRDB::enrichment_object_from_db(enrichment_objects$enrichment_name[selected], con))
      names(module) <- enrichment_objects$enrichment_name[selected]
      return(module)
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
    output$enrichment_overview <- DT::renderDataTable({enrichment_objects},
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      colnames = c("Enrichment name", "Module name", "Enrichment method"),
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
    output$enrichment_overview <- DT::renderDataTable({enrichment_objects},
                                                      rownames = FALSE,
                                                      selection = list(selected = c(1)),
                                                      colnames = c("Enrichment name", "Module name", "Enrichment method"),
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
          top = 2,
          title = app_servr$enrichment_name,
          easyClose = TRUE,
          size = "l",
          fluidPage(
            tabsetPanel(id = ns("tabs"),
                        type = "tabs",
                        tabPanel(title = "Results",
                                 tags$br(),
                                 prettySwitch(ns("include_disease_genes"), 
                                              label = "Include disease genes in table",
                                              value = FALSE,
                                              popup = "The disease genes will be shown as a string and may be hard to read from. You can either download the table as .xlsx/.csv to manipulate it or visit the Visualization tab to inspect the disease genes"),
                                 uiOutput(ns("separator")),
                                 DT::dataTableOutput(ns("results"))),
                        tabPanel(title = "Settings",
                                 DT::dataTableOutput(ns("settings")))),
            rep_br(2)),
          footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
          )
        ))
      )
    })
    
    observeEvent(input$include_disease_genes, {
      
      if (input$include_disease_genes == TRUE) {
        output$separator <- renderUI({
          tagList(
            prettySwitch(ns("separator_used"), label = "Separate geneID by ' / ' or comma separated", value = FALSE)
          ) 
        })
        
        observeEvent(input$separator_used, {
          if (input$separator_used == TRUE) {
            enrichment_results <- enrichment_module@result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count", "geneID")]
            geneID <- enrichment_results$geneID %>%  gsub("/", ",", .)
            enrichment_results <- data.frame(Disease_ID = row.names(enrichment_results),
                                             enrichment_results[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count")], 
                                             geneID = geneID)
            
            output$results <- DT::renderDataTable({enrichment_results},
                                                  rownames = FALSE,
                                                  colnames = c("Disease ID", "Description", "Gene Ration", "Bg Ration", "P-value", "P-adjusted", "q-value", "Count", "Genes"),
                                                  filter = "top", 
                                                  class = 'compact cell-border hover',
                                                  style = "default",
                                                  extensions = 'Buttons',
                                                  options = list(
                                                    pageLength = 25,
                                                    paging = TRUE,
                                                    searching = TRUE,
                                                    scrollX = TRUE,
                                                    scrollY = TRUE,
                                                    #fixedColumns = FALSE,
                                                    autoWidth = FALSE,
                                                    ordering = TRUE,
                                                    dom = "lfrtipB",
                                                    buttons = c('copy', 'csv', 'excel'),
                                                    lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All"))))
          } else {
            enrichment_results <- enrichment_module@result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count", "geneID")]
            enrichment_results <- data.frame(Disease_ID = row.names(enrichment_results), enrichment_results)
            
            output$results <- DT::renderDataTable({enrichment_results},
                                                  rownames = FALSE,
                                                  colnames = c("Disease ID", "Description", "Gene Ration", "Bg Ration", "P-value", "P-adjusted", "q-value", "Count", "Genes"),
                                                  filter = "top", 
                                                  class = 'compact cell-border hover',
                                                  style = "default",
                                                  extensions = 'Buttons',
                                                  options = list(
                                                    pageLength = 25,
                                                    paging = TRUE,
                                                    searching = TRUE,
                                                    scrollX = TRUE,
                                                    scrollY = TRUE,
                                                    #fixedColumns = FALSE,
                                                    autoWidth = FALSE,
                                                    ordering = TRUE,
                                                    dom = "lfrtipB",
                                                    buttons = c('copy', 'csv', 'excel'),
                                                    lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All"))))
          }
        })
        
      } else { 
      shinyjs::hide("separator_used")
        enrichment_results <- enrichment_module@result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count")]
        enrichment_results <- data.frame(Disease_ID = row.names(enrichment_results), enrichment_results)
        
        output$results <- DT::renderDataTable({enrichment_results},
                                              rownames = FALSE,
                                              colnames = c("Disease ID", "Description", "Gene Ration", "Bg Ration", "P-value", "P-adjusted", "q-value", "Count"),
                                              filter = "top", 
                                              class = 'compact cell-border hover',
                                              style = "default",
                                              extensions = 'Buttons',
                                              options = list(
                                                pageLength = 25,
                                                paging = TRUE,
                                                searching = TRUE,
                                                scrollX = TRUE,
                                                scrollY = TRUE,
                                                #fixedColumns = FALSE,
                                                autoWidth = FALSE,
                                                ordering = TRUE,
                                                dom = "lfrtipB",
                                                buttons = c('copy', 'csv', 'excel'),
                                                lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All"))))
      }
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

