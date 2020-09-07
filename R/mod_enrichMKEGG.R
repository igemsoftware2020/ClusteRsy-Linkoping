#' enrichMKEGG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_enrichMKEGG_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_enrich',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_enrich", class = "collapse",
             tags$div(
    selectInput(ns("keytype"), 
                label = "Select key type",
                choices = c("kegg",
                            "ncbi-proteinid",
                            "uniprot"),
                popup = "The gene-id type of the input"),
    sliderInput(ns("deg_cutoff"), label = "P-value cutoff", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value"),
    sliderInput(ns("qvalue_cutoff"), label = "Q-value cutoff", min = 0, max = 1, value = 0.2, popup = "Rejecting the null hypothesis for any result with an equal or smaller value. Q-values are false discovery rate (FDR) adjusted p-values"),
    selectInput(ns("padj_method"), 
                label = "Select Adjusted p-value method",
                choices = c("hochberg",
                            "hommel",
                            "bonferroni",
                            "BH",
                            "BY",
                            "fdr",
                            "none"),popup = "Correction methods used to control p-values and q-values",
                multiple = FALSE,
                selectize = TRUE
                ),
    uiOutput(ns("background_genes")),
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set annotated", min = 1, max = 100, value = 10, popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximum size of each gene set annotated", min = 100, max = 1000, value = 500, popup = "Maximum size of each gene set used for analyzing"),
             )),
    
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich", onclick="loading_modal_open(); stopWatch();"),
              htmlOutput(ns("close_loading_modal")),  # Close modal with JS 
              htmlOutput((ns("adv_settings")))
    )
  )
}
    
#' enrichMKEGG Server Function
#'
#' @noRd 
mod_enrichMKEGG_server <- function(input, output, session, con, Description1_ui_1, module_overview_ui_1){
  ns <- session$ns
  
  enrichMKEGG_module <- reactiveValues()
  x <- reactiveVal(1) # Reactive value to record if the input button is pressed
  
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    tagList(
      selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis."),
      textInput(ns("enrichment_name"), "Module object name", popup = "Object that is produced by the enrichment methods.", placeholder = "Enrichment name"),
      uiOutput(ns("error"))
    )
  })
  
  observeEvent(c(Description1_ui_1$module_name, module_overview_ui_1$value$delete, module_overview_ui_1$value$upload), {
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    updateSelectInput(session, "module_object", choices = module_objects)
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    output$error <- renderUI({})
    output$adv_settings <- renderUI({})
    
    module_genes <- try(get_module_genes(input$module_object, con = con))
    background_genes <- try(get_background_genes(input$module_object, con = con))
    
    enrichment_object <- try(clusterProfiler::enrichMKEGG(gene = module_genes,
                                                         organism = 'hsa', #Homo sapiens set as default.
                                                         keyType = input$keytype,
                                                         pvalueCutoff = input$deg_cutoff,
                                                         pAdjustMethod = input$padj_method,
                                                         universe = background_genes,
                                                         minGSSize = input$mingssize,
                                                         maxGSSize = input$maxgssize,
                                                         qvalueCutoff = input$qvalue_cutoff
                                                         
    )
    )
    if (any(c(class(enrichment_object), class(background_genes), class(module_genes)) == "try-error")){
      output$adv_settings <- renderUI({
        tags$script("if ($('.collapsible.btn.btn-primary.btn-block').eq(1).attr('aria-expanded') === 'false') {
                            $('.collapsible.btn.btn-primary.btn-block').eq(1).click();
                    }")
      })
      output$error <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), enrichment_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      x(x() + 1)
      enrichMKEGG_module$enrich <- c(x(), "enrichMKEGG")  # Reactive value to record if the input button is pressed
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(enrichment_object,
                                          module_name = module_name, 
                                          enrichment_method = "enrichMKEGG", 
                                          enrichment_name = input$enrichment_name,
                                          con = con)
      updateTextInput(session, "enrichment_name", value = character(0))
    }
    # Close loading modal
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(enrichMKEGG_module)
}
    
## To be copied in the UI
# mod_enrichMKEGG_ui("enrichMKEGG_ui_1")
    
## To be copied in the server
# callModule(mod_enrichMKEGG_server, "enrichMKEGG_ui_1")
 
