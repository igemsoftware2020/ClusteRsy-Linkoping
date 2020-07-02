#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visual_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("enrichment_overview")),
    mod_enrichKEGG_ui(ns("enrichKEGG_ui_1")),
    actionButton(ns("refresh"), label = "refresh"),
    tags$div(`class`="jumbotron",
             tags$h1("Hello,world!",class="display-3"),
             tags$hr(class="my-4"),
             tags$p("I made it!"),
             tags$b(),
             tags$p(class="lead",
                   tags$a("Learn more", class="btn btn-primary btn-lg", href="https://bootswatch.com/flatly/")
                   )
             )
    )
}


#' create_input Server Function
#'
#' @noRd 
mod_visual_server <- function(input, output, session, con){
  ns <- session$ns
  callModule(mod_enrichKEGG_server, "enrichKEGG_ui_1",con = con)
  
  observeEvent(input$refresh, {
    if(RSQLite::dbExistsTable(con, "enrichment_register")) {
      
      enrichment_object <- MODifieRDB::get_available_enrichment_objects(con)
      
      output$enrichment_overview <- DT::renderDataTable({
        DT::datatable(enrichment_object)
      })
    } else {
      print("hej")
    }
  })  
  
  
}

## To be copied in the UI
# mod_visual_ui("visual_ui_1")

## To be copied in the server
# callModule(mod_visual_server, "visual_ui_1")

