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
    uiOutput(ns("results_ui")),
    DT::dataTableOutput(ns("enrichment_overview")),
    actionButton(ns("refresh"), label = "Refresh"),
    actionButton(ns("analyze"), label = "Analyze"),
    downloadButton(ns("download_enrichment"), label = "Download")
  )
}
    
#' enrichment_overview Server Function
#'
#' @noRd 
mod_enrichment_overview_server <- function(input, output, session, con){
  ns <- session$ns
  
   selected <- reactiveValues()
   
   observeEvent(input$refresh, {
     if(RSQLite::dbExistsTable(con, "enrichment_register")) {
       
       enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
       
       output$enrichment_overview <- DT::renderDataTable({
         styling <- DT:::DT2BSClass(c('compact', 'hover'))
         DT::datatable(enrichment_objects, 
                       class = styling,
                       selection = "single")
       })
       
     } else {
       return(NULL)
     }
   })

      output$download_enrichment <- downloadHandler(
        filename = function() {
          paste0("enrichment_set_", Sys.Date(), ".rds", sep="")
        },
        content = function(file) {
          saveRDS(selected, file)
        }
      )

      callModule(mod_enrichment_results_server, "enrichment_results_ui_1", selected, con = con)
}

    
## To be copied in the UI
# mod_enrichment_overview_ui("enrichment_overview_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1")
 
