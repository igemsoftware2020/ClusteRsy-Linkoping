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
         mod_enrichment_overview_ui(ns("enrichment_overview_ui_1"))
  )
}
    
#' enrichment_results Server Function
#'
#' @noRd 
mod_enrichment_results_server <- function(input, output, session, con){
  ns <- session$ns
  
  
  enrichment_overview_ui_1 <- callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1", con = con)
  observeEvent(enrichment_overview_ui_1$enrichment_object, {
    
    enrichment_result <<- enrichment_overview_ui_1$enrichment_object@result
    enrichment_result_table <<- enrichment_result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count")]
    
    
    output$enrichment_results <- DT::renderDataTable({
      styling <- DT:::DT2BSClass(c('compact', 'cell-border', 'hover'))
      DT::datatable(enrichment_result_table, 
                    filter = "top", 
                    class = styling,
                    
                    extensions = 'Buttons',
                    
                    options = list(
                      paging = TRUE,
                      searching = TRUE,
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel')
                    )
                    )
    })
  })
  
  
  
}
    
## To be copied in the UI
# mod_enrichment_results_ui("enrichment_results_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_server, "enrichment_results_ui_1")
 
