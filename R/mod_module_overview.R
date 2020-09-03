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
    shinyjs::useShinyjs(),
    tags$div(style= "margin-left: 10px; margin-right: 10px",
            tags$h1(style= "color: #2b3e50; ", "Module Objects"),
            actionLink(inputId = "information_btn_module", label = "Learn More"),
            tags$br(),
            tags$br(),
    DT::dataTableOutput(ns("module_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4", style = "color:black",
             fileInput(ns("module_object"), label = "Upload a module object", accept =  ".rds")),
             tags$div(uiOutput(ns("module_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-8", style = "text-align:right", id ="buttons_module_overview",
                      downloadButton(ns("download_module_cytoscape"), label = "dumby", style = "visibility: hidden;"),
                      actionButton(ns("post_process"), label = "Post-process"),
                      actionButton(ns("download_cytoscape_trigger"), label = "Cytoscape", icon = icon("download")), #This triggers the downloadButton download_module_cytoscape
                      downloadButton(ns("download_module"), "Download"),
                      actionButton(ns("delete"), tags$i(class="fa fa-trash-o", `aria-hidden`="true")),
                      htmlOutput(ns("close_loading_modal")) # Close modal with JS 
                      )),
    uiOutput(ns("inspected_results")),
    uiOutput(ns("disable")),
    uiOutput(ns("DT_tooltip")),
    uiOutput(ns("modal_ppi_network")),
    
  ))
}

#' module_overview Server Function
#'
#' @noRd 
mod_module_overview_server <- function(input, output, session, con, Columns_ui_1, app_servr){
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
                                                  selection = list(selected = c(1)),
                                                  callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("module_name", data[0]);
                                                              Shiny.setInputValue("module_dbclick", dbclick);
                                                             });'))
  })
  
  module_objects <- MODifieRDB::get_available_module_objects(con)
  # Render DT
  output$module_overview <- DT::renderDataTable(module_objects,
                                                rownames = FALSE,
                                                selection = list(selected = c(1)),
                                                callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("module_name", data[0]);
                                                              Shiny.setInputValue("module_dbclick", dbclick);
                                                             });
                                                             Shiny.setInputValue("DT_tooltip1", "DT_tooltip1");
                                                                 '))
  
  
  # Refresh DT
  observeEvent(Columns_ui_1$module_name, {
    module_objects <- MODifieRDB::get_available_module_objects(con)
    output$module_overview <- DT::renderDataTable(module_objects,
                                                  rownames = FALSE,
                                                  selection = list(selected = c(1)),
                                                  callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("module_name", data[0]);
                                                              Shiny.setInputValue("module_dbclick", dbclick);
                                                             });'))
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
  
  subset_module_genes <- reactiveVal()
  
  #Prepare data for cytoscape download.
  
  observeEvent(input$download_cytoscape_trigger, {

    module_object <- MODifieRDB::MODifieR_module_from_db(module_objects$module_name[input$module_overview_rows_selected], con = con)

    if (module_objects$module_type[input$module_overview_rows_selected] %in% c("Mcode", "Correlation_clique", "DIAMOnD", "module_discoverer")) {
      shinyjs::runjs("loading_modal_open(); stopWatch();")
      ppi_name <- as.character(module_object$settings$ppi_network)
      ppi_network <- MODifieRDB::ppi_network_from_db(ppi_name = ppi_name, con = con)
      module_genes <- module_object$module_genes
  
      
      subset_module_genes(dplyr::filter(ppi_network, ppi_network[,1] %in% module_genes & ppi_network[,2] %in% module_genes))
      
      shinyjs::runjs("document.getElementById('main_page_v2_ui_1-module_overview_ui_1-download_module_cytoscape').click();")

    } else if (module_objects$module_type[input$module_overview_rows_selected] == "Clique_Sum_permutation") {
      
      shinyjs::runjs("loading_modal_open(); stopWatch();")
      ppi_name <- MODifieRDB::match_db_loc_to_ppi(module_object$settings$db, con = con)
      ppi_network <- MODifieRDB::ppi_network_from_db(ppi_name = ppi_name, con = con)
      module_genes <- module_object$module_genes
      
      subset_module_genes(dplyr::filter(ppi_network, ppi_network[,1] %in% module_genes & ppi_network[,2] %in% module_genes))
      
      shinyjs::runjs("document.getElementById('main_page_v2_ui_1-module_overview_ui_1-download_module_cytoscape').click();")
      
    } else {
      ppi_networks <- MODifieRDB::get_available_networks(con)

      output$modal_ppi_network <- renderUI({
        tagList(
          showModal(modalDialog(
            title = "Select a PPI Network",
            easyClose = TRUE,
            size = "l",
            tags$p("This MODifieR object doesn't contain any PPI Network, please select one from the database"),
            selectInput(ns("selected_ppi_network"),
                        label = "Select a PPI network",
                        choices = ppi_networks),
            actionButton(ns("download_cytoscape_object"), label = "Download cytoscape object"),
            footer = tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
          ))
        )
      })

      observeEvent(input$download_cytoscape_object, {

        removeModal()
        shinyjs::runjs("loading_modal_open(); stopWatch();")
        
        ppi_network <- MODifieRDB::ppi_network_from_db(input$selected_ppi_network, con = con)
        module_genes <- module_object$module_genes
        
        subset_module_genes(dplyr::filter(ppi_network, ppi_network[,1] %in% module_genes & ppi_network[,2] %in% module_genes))
        
        shinyjs::runjs("document.getElementById('main_page_v2_ui_1-module_overview_ui_1-download_module_cytoscape').click();")

        })

    }

  })
  
  #Download cytoscape object
  output$download_module_cytoscape <- downloadHandler(

    filename = function() {
      paste0(current_modules(), "_module_genes_interaction_", Sys.Date(), ".zip")
    },

    content = function(file) {
      shinyjs::runjs("loading_modal_close(); reset();")
      module_object <- MODifieRDB::MODifieR_module_from_db(module_objects$module_name[input$module_overview_rows_selected], con = con)
      file_subset_edgeR <- retrieve_input_data(module_object, con = con)
      file_subset_edgeR <- cbind("genes" = rownames(file_subset_edgeR), file_subset_edgeR)
      file1 <- paste0(current_modules(),".csv")
      file2 <- paste0(current_modules(),"_edgeR_deg_table.csv")
      
      file_network <- write.table(subset_module_genes(), file=file1, quote=FALSE, sep='\t', row.names = F)
      file_subset_edgeR <- write.table(file_subset_edgeR, file=file2, quote=FALSE, sep='\t', row.names = F)
      
      zip(file,c(file1, file2))
    }
  )
  
  #Download module object
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
                                                  selection = list(selected = c(1)),
                                                  rownames = FALSE,
                                                  callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("module_name", data[0]);
                                                              Shiny.setInputValue("module_dbclick", dbclick);
                                                             });'))
    
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
                                                  selection = list(selected = c(1)),
                                                  rownames = FALSE,
                                                  callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("module_name", data[0]);
                                                              Shiny.setInputValue("module_dbclick", dbclick);
                                                             });'))
    # Send refresh
    module_overview_module$delete <- input$delete
  })
  
  #Listening the the module_type that is being selected in the DT.
  #This observeEvent use module_objects due to observers for new input data above.
  observeEvent(module_objects$module_type[input$module_overview_rows_selected],{
    selected_module_type <- module_objects$module_type[input$module_overview_rows_selected]
    if (length(selected_module_type) == 1) {
      shinyjs::enable("download_cytoscape_trigger")
      req(module_objects$module_type[input$module_overview_rows_selected] %in% c("Correlation_clique", "DIAMOnD", "DiffCoEx", "Mcode", "MODA", "WGCNA"))
      shinyjs::enable("post_process")
    } else {
      shinyjs::disable("download_cytoscape_trigger")
      shinyjs::disable("post_process")
    }
    
  })
  
  # Inspect current module
  
  inspected_result_list <- reactiveValues()
  module_objects_inspected <- MODifieRDB::get_available_module_objects(con) #This value is needed in order to retrieve the actual contet that is being displayed in the DT
  post_process_button <- NULL
  inspect_button <- NULL
  
  # Observer when DT is loaded
  observeEvent(app_servr$DT_tooltip1, {
    output$DT_tooltip <- renderUI({
      tags$script('
                  $("#main_page_v2_ui_1-module_overview_ui_1-module_overview").find("tr").eq(1).attr("id", "DT_tooltip1");
                  Tipped.create("#DT_tooltip1",
                  "Double-click me to inspect the object!",
                  {shadow: false});
                  Tipped.show("#DT_tooltip1");
                  ')
    })
  })
  
  #Refresh
  observeEvent(inspected_result_list$list$server_output$post_process_module_object, { 
    ##### Important: This needs too be double arrowed in order for it too work! #####
    module_objects_inspected <<- MODifieRDB::get_available_module_objects(con) 
    
    output$module_overview <- DT::renderDataTable(module_objects_inspected,
                                                  selection = list(selected = c(1)),
                                                  rownames = FALSE,
                                                  callback = DT::JS('
                                                            table.on("dblclick.dt","tr", function() {
                                                              var data=table.row(this).data();
                                                              dbclick++;
                                                              Shiny.setInputValue("module_name", data[0]);
                                                              Shiny.setInputValue("module_dbclick", dbclick);
                                                             });'))
  })
  
  
  observeEvent(app_servr$module_dbclick, {
    post_process_button <- NULL
    inspect_button <- 1

    selected <- input$module_overview_rows_selected
    inspected_module <- MODifieRDB::MODifieR_module_from_db(app_servr$module_name, con = con)
    if (is.null(inspected_module$module_genes)) {
      showNotification("This module doesn't contain any genes", duration = 10, closeButton = TRUE, type = "warning")
    } else {
    selected_module_name$name <- app_servr$module_name
    inspected_result_list$list <- inspect_module(inspected_module, selected_module_name, inspect_button, post_process_button, ns, con)
    output$inspected_results <- renderUI({
      inspected_result_list$list$ui_output
    })
  }
  })
  
#Post processing of current module
  

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
