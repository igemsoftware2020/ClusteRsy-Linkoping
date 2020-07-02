#' ppi_networks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ppi_networks_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("ppi_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-8", style = "color:black",
             fileInput(ns("ppi_network"), label = "Upload a PPI network"),
             uiOutput(ns("ppi_name_chooser"))),
             tags$br(),
             tags$div(`class`="col-sm-4", style = "text-align:right",
                      actionButton(ns("refresh"), HTML("<i class='fa fa-refresh' aria-hidden='true'></i> Refresh")),
                      ))
  )
}

#' ppi_networks Server Function
#'
#' @noRd 
mod_ppi_networks_server <- function(input, output, session, con){
  ns <- session$ns
  
  # Reactive function for file input
  upload_ppi <- reactive({
    req(input$ppi_network)
    infile <- (input$ppi_network$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    read.table(file = infile, header = T)
  })
  
  output$ppi_name_chooser <- renderUI({
    ppi <- upload_ppi() # reactive
    tagList( 
      textInput(ns("ppi_name"), "PPI network name"),
      actionButton(ns("upload_ppi"), "Add PPI to database")
    )
  })
  
  ppi_networks <- as.data.frame(MODifieRDB::get_available_networks(con))
  colnames(ppi_networks) <- "PPI networks"
  
  # Render DT
  output$ppi_overview <- DT::renderDataTable(ppi_networks,
                                             selection = list(selected = c(1)))
  
  # Refresh DT
  observeEvent(input$refresh, {
    ppi_networks <- as.data.frame(MODifieRDB::get_available_networks(con)) 
    colnames(ppi_networks) <- "PPI networks"
    output$ppi_overview <- DT::renderDataTable(ppi_networks,
                                               selection = list(selected = c(1)))
  })
  
  observeEvent(input$upload_ppi, {
    id <- showNotification("Saving PPI to database", duration = NULL, closeButton = FALSE)
    ppi <- upload_ppi()
    ppi_name <- input$ppi_name
    on.exit(removeNotification(id), add = TRUE)
    
    MODifieRDB::ppi_network_to_db(ppi_network = ppi, ppi_name = ppi_name, con = con)
    
  })
    
  if (is.data.frame(ppi_networks) && nrow(ppi_networks)==0) {
    MODifieRDB::ppi_network_to_db(ppi_network = MODifieR::ppi_network,
                                  ppi_name = "Default", 
                                  con = con)
  }
    else if (any(ppi_networks == "Default")) {
      return()
    }
  }

## To be copied in the UI
# mod_ppi_networks_ui("ppi_networks_ui_1")

## To be copied in the server
# callModule(mod_ppi_networks_server, "ppi_networks_ui_1")