#' enrichment_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichment_results_ui <- function(id){
  ns <- NS(id)
  tagList(
          DT::dataTableOutput(ns("enrichment_results")),
         
  )
}
    
#' enrichment_results Server Function
#'
#' @noRd 
mod_enrichment_results_server <- function(input, output, session, selected, con){
  ns <- session$ns
    
  object <- reactive({
    MODifieRDB::enrichment_object_from_db(selected$selected_object,con)@result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count")]
  })    
    
    output$enrichment_results <- DT::renderDataTable({object()},
                                                     options = list(
                                                       rownames = FALSE,
                                                       filter = "top", 
                                                       class = 'compact cell-border hover',
                                                       style = "default",
                                                       extensions = 'Buttons',
                                                       paging = TRUE,
                                                       searching = TRUE,
                                                       scrollX = TRUE,
                                                       scrollY = TRUE,
                                                       #fixedColumns = FALSE,
                                                       autoWidth = FALSE,
                                                       ordering = TRUE,
                                                       dom = 'Bfrtip',
                                                       buttons = c('copy', 'csv', 'excel')))
}
    
## To be copied in the UI
# mod_enrichment_results_ui("enrichment_results_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_server, "enrichment_results_ui_1")
 
