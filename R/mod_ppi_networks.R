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
    tags$div(style= "margin-left: 10px; margin-right: 10px",
             tags$h1(style= "color: #2b3e50", "PPI Networks"),
             actionLink(inputId = "information_btn_ppi", label = "Learn More"),
             tags$br(),
             tags$br(),
    DT::dataTableOutput(ns("ppi_overview")),
    tags$div(`class`="row",
             tags$div(`class`="col-sm-8", style = "color:black",
             fileInput(ns("ppi_network"), label = "Upload a PPI network"),
             uiOutput(ns("ppi_name_chooser"))))
  ))
}

#' ppi_networks Server Function
#'
#' @noRd 
mod_ppi_networks_server <- function(input, output, session, con){
  ns <- session$ns
  ppi_networks_module <- reactiveValues()
  
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
      textInput(ns("ppi_name"), "PPI network name", placeholder = "PPI name"),
      actionButton(ns("upload_ppi"), "Add PPI to database")
    )
  })
  
  # Upload PPI
  x <- reactiveVal(1)
  observeEvent(input$upload_ppi, {
    id <- showNotification("Saving PPI to database", duration = NULL, closeButton = FALSE)
    ppi <- upload_ppi()
    ppi_name <- input$ppi_name
    on.exit(removeNotification(id), add = TRUE)
    
    MODifieRDB::ppi_network_to_db(ppi_network = ppi, ppi_name = ppi_name, con = con)
    
    # Refresh
    ppi_networks <- as.data.frame(MODifieRDB::get_available_networks(con)) 
    colnames(ppi_networks) <- "PPI networks"
    output$ppi_overview <- DT::renderDataTable(ppi_networks,
                                               rownames = FALSE,
                                               selection = list(selected = c(1)))
    # Send refresh to Description1_ui_1
    x(x() + 1)
    ppi_networks_module$upload_ppi <- x()
  })
  
  ppi_networks <- as.data.frame(MODifieRDB::get_available_networks(con))
  
  
  # Create Deafault network
  if (any(ppi_networks == "Default_string_700")) {
    ppi_networks <- as.data.frame(MODifieRDB::get_available_networks(con))
    colnames(ppi_networks) <- "PPI networks"
    output$ppi_overview <- DT::renderDataTable(ppi_networks,
                                               rownames = FALSE,
                                               selection = list(selected = c(1)))
  } else {
    PPI_network <- read.delim("./inst/app/www/PPI_network.txt")
    MODifieRDB::ppi_network_to_db(PPI_network,
                                  ppi_name = "Default_string_700",
                                  con = con)
    ppi_networks <- as.data.frame(MODifieRDB::get_available_networks(con))
    colnames(ppi_networks) <- "PPI networks"
    output$ppi_overview <- DT::renderDataTable(ppi_networks,
                                               rownames = FALSE,
                                               selection = list(selected = c(1)))
  }
  
  
  # Create Deafault Clique SLQ
  # if (nrow(MODifieRDB::get_available_db_networks(con))==0 ) {
  #   clique_db <- MODifieRDB::build_clique_db_db(ppi_name = "Default",
  #                                               db_folder =  "./data_example" ,
  #                                               db_name = "igem",
  #                                               con = con)
  # }
  
  return(ppi_networks_module)
}

## To be copied in the UI
# mod_ppi_networks_ui("ppi_networks_ui_1")

## To be copied in the server
# callModule(mod_ppi_networks_server, "ppi_networks_ui_1")