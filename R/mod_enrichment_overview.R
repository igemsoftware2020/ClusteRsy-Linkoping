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
    DT::dataTableOutput(ns("enrichment_overview")),
    actionButton(ns("refresh"), label = "Refresh"),
    downloadButton(ns("download_enrichment"), label = "Download")
  )
}
    
#' enrichment_overview Server Function
#'
#' @noRd 
mod_enrichment_overview_server <- function(input, output, session, con){
  ns <- session$ns
   
   observeEvent(input$refresh, {
     if(RSQLite::dbExistsTable(con, "enrichment_register")) {
       
       enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)[c("module_name", "enrichment_method")]
       
       output$enrichment_overview <- DT::renderDataTable(enrichment_objects, 
                                                         class = "compact, hover",
                                                         selection = "multiple")
       }
   })
   
   # Choose multiple options
   current_enrichment_objects <- function() {
     selected <- input$enrichment_overview_overview_rows_selected
     MODifieRDB::enrichment_object_from_db(selected, con = con)
   }
   
   retrieve_enrichment_object <- function(){
     selected <- input$enrichment_overview_rows_selected
     if (length(selected) > 1){
       lapply(current_enrichment_objects(), MODifieRDB::enrichment_object_from_db, con = con)
     } else {
       MODifieRDB::MODifieR_module_from_db(MODifieRDB::get_available_enrichment_objects(selected ,con))
     }
   }

   # Download function
      output$download_enrichment <- downloadHandler(
        filename = function() {
          paste0("enrichment_set_", Sys.Date(), ".rds", sep="")
        },
        content = function(file) {
          saveRDS(retrieve_enrichment_object, file)
        }
      )

}

    
## To be copied in the UI
# mod_enrichment_overview_ui("enrichment_overview_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1")
 
