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

    output$enrichment_results <- DT::renderDataTable({
      
      
      enrichment_object <- MODifieRDB::enrichment_object_from_db(con,
                                                                 selected$selected_object
      )
      
      enrichment_results <- enrichment_object@result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count")]
      styling <- DT:::DT2BSClass(c('compact', 'cell-border', 'hover'))
      DT::datatable(enrichment_results, 
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

  
}
    
## To be copied in the UI
# mod_enrichment_results_ui("enrichment_results_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_server, "enrichment_results_ui_1")
 
