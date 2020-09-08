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
          DT::dataTableOutput(ns("enrichment_results")) %>% shinycssloaders::withSpinner(color="#ffbd40", 
                                                                        type = 4,
                                                                        size = 0.8),
          
          uiOutput(ns("inspected_disease")),
          uiOutput(ns("disable"))
  )
}
    
#' enrichment_results Server Function
#'
#' @noRd 
mod_enrichment_results_server <- function(input, output, session, enrichment_results_para_ui_1, selected, con){
  ns <- session$ns
  
  enrichment_results_module <- reactiveValues()
    
  object <- reactive({
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    MODifieRDB::enrichment_object_from_db(enrichment_objects$enrichment_name[selected$selected_object], con)@result[c("Description", "GeneRatio", "BgRatio", "pvalue", "p.adjust", "qvalue", "Count")]
  })
    
  
  output$enrichment_results <- try(DT::renderDataTable({object()},
                                                   rownames = FALSE,
                                                   selection = list(selected = c(1)),
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
                                                     lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")))))
  
  observe({
    if (is.null(input$enrichment_results_rows_selected) || length(input$enrichment_results_rows_selected) > 1) {
      enrichment_results_module$check <- T
    } else {
      enrichment_results_module$check <- F
    }
  })

  observeEvent(enrichment_results_para_ui_1$inspect_disease, {
    selected <- selected$selected_object
    enrichment_objects <- MODifieRDB::get_available_enrichment_objects(con)
    
    enrichment_object <- MODifieRDB::enrichment_object_from_db(enrichment_objects$enrichment_name[selected],con)
    selected_enrichment_object <- input$enrichment_results_rows_selected

    output$inspected_disease <- renderUI({
        tagList(
          showModal(modalDialog(
            top = 8,
            title = paste(paste(enrichment_object@result$Description[selected_enrichment_object], "Genes")),
            easyClose = TRUE,
            size = "l",
            fluidPage(
              tags$p(paste("Disease ID:", enrichment_object@result$ID[selected_enrichment_object])),
              DT::dataTableOutput(ns("disease_genes")),
            ),
            rep_br(2),
            footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
            )

          ))
        )
    })

    inspected_genes <- data.frame(strsplit(enrichment_object@result$geneID[selected_enrichment_object], "/"))
    colnames(inspected_genes) <- "Genes"

    output$disease_genes <- DT::renderDataTable({inspected_genes},
                                            rownames = FALSE,
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
                                              autoWidth = FALSE,
                                              ordering = TRUE,
                                              dom = "lfrtipB",
                                              buttons = c('copy', 'csv', 'excel'),
                                              lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")) ))

  })
  
  return(enrichment_results_module)
}
    
## To be copied in the UI
# mod_enrichment_results_ui("enrichment_results_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_server, "enrichment_results_ui_1")
 
