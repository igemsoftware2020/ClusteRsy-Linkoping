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
    shinyjs::useShinyjs(),
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
                                                   selection = list(mode = 'single', selected = c(1)),
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
                                                     lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All"))))
                                   )
  
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
    
    input_data_name <- MODifieRDB::get_input_name_by_enrichment_row(enrichment_objects$enrichment_name[selected], con)
    input_data <- MODifieRDB::MODifieR_input_from_db(input_data_name, con)

    output$inspected_disease <- renderUI({
        tagList(
          showModal(modalDialog(
            top = 8,
            title = paste(paste(enrichment_object@result$Description[selected_enrichment_object], "Genes")),
            easyClose = TRUE,
            size = "l",
            fluidPage(
              tags$p(paste("Disease ID:", enrichment_object@result$ID[selected_enrichment_object])),
              prettySwitch(ns("color_dt"), label = "Color the datatable", value = TRUE, popup = "This setting will color the logFC values with a colorgradient, red = high logFC and blue = low logFC. The P-values will also be colored with green = P-value < the P-value threshold slider"),
              uiOutput(ns("color_parameters")),
              DT::dataTableOutput(ns("disease_genes"), width = "auto"),
            ),
            rep_br(2),
            footer = tagList(tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
            )

          ))
        )
    })
  
  observeEvent(input$color_dt, {
    
    inspected_genes <- data.frame(strsplit(enrichment_object@result$geneID[selected_enrichment_object], "/"))
    colnames(inspected_genes) <- "Genes"
    subset_edgeR_disease_genes <- dplyr::filter(input_data$edgeR_deg_table, rownames(input_data$edgeR_deg_table) %in% inspected_genes$Genes)
    subset_edgeR_disease_genes <- data.frame(Genes = rownames(subset_edgeR_disease_genes), subset_edgeR_disease_genes)
    
    no_input_genes <- setdiff(inspected_genes$Genes, subset_edgeR_disease_genes$Genes)
    
    if (length(no_input_genes) != 0) {
      no_input_genes <- data.frame(Genes = no_input_genes, logFC = NA, logCPM = NA, F = NA, PValue = NA, FDR = NA)
      disease_genes <- rbind(subset_edgeR_disease_genes, no_input_genes)
    } else {
      disease_genes <- rbind(subset_edgeR_disease_genes, no_input_genes)
    }
    
    
    if (input$color_dt == TRUE) {
      
      output$color_parameters <- renderUI({
        tagList(
          sliderInput(ns("pvalue_color"), "Select the threshold for the P-value color", min = 0.0001, max = 0.07, value = 0.05)
        )
      })
    
      observeEvent(input$pvalue_color, {
        
        cuts <- subset_edgeR_disease_genes$logFC[order(subset_edgeR_disease_genes$logFC)]
        color_func <- colorRampPalette(c("#4475b4","#73acd0","#abd8e9", "#fffeeb", "#fead61", "#f36d42", "#d73026"))
        dat <- DT::datatable(disease_genes,
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
                               lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")))) %>% 
          formatStyle("logFC", backgroundColor = styleInterval(cuts, color_func(length(cuts) +1))) %>% 
          formatStyle("PValue", background = styleInterval(input$pvalue_color, c("#8cdb9a", "#fc6d6d"))) %>%
          formatRound(columns = c("logFC", "logCPM", "F", "FDR"), digits = 5) %>% 
          formatRound(columns = "PValue", digits = 8)
        
        output$disease_genes <- DT::renderDataTable({dat})
        
      })  
      
    
                          
    } else {
      
      shinyjs::hide("pvalue_color")
      
      dat <- DT::datatable(disease_genes,
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
                             lengthMenu = list(c(10,25,50,100, -1), c(10,25,50,100, "All")))) %>% 
        formatRound(columns = c("logFC", "logCPM", "F", "FDR"), digits = 5) %>% 
        formatRound(columns = "PValue", digits = 8)
      
      output$disease_genes <- DT::renderDataTable({dat})
       
    }                      
                                                
  })
  
  })
  
  
  return(enrichment_results_module)
}
    
## To be copied in the UI
# mod_enrichment_results_ui("enrichment_results_ui_1")
    
## To be copied in the server
# callModule(mod_enrichment_results_server, "enrichment_results_ui_1")
 
