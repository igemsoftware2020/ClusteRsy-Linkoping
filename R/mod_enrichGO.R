#' GO UI Function 
#' 
#' @description A shiny Module.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
#' 
#' @importFrom shiny NS tagList
mod_enrichGO_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("module_input")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_enrich',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_enrich", class = "collapse",
             tags$div(
    selectInput(
      ns("keytype"),
      label = "Select the type of gene input",
      choices = c(AnnotationDbi::keytypes(org.Hs.eg.db::org.Hs.eg.db)),
      popup = "Select the type of the input data"
      ),
    selectInput(
      ns("ont"),
      label = "Select subontologies",
      choices = c("BP", "MF", "CC", "ALL"),
      popup = "Either biological process (BP), cellular component (CC), molecular function (MF) or all."
      ),
    sliderInput(
      ns("pvaluecutoff"),
      label = "Select p-value cutoff",
      min = 0, 
      max = 1, 
      value = 0.05,
      popup = "Rejecting the null hypothesis for any result with an equal or smaller value"
      ), 
    sliderInput(
      ns("qvaluecutoff"), 
      label = "Select q-value cutoff", 
      min = 0, 
      max = 1, 
      value = 0.2,
      popup = "Rejecting the null hypothesis for any result with an equal or smaller value. Q-values are false discovery rate (FDR) adjusted p-values"
    ), 
    selectInput(
      ns("padjustmethod"), 
      label = "Select method", 
      choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none "),
      popup = "Correction methods used to control p-values and q-values"
      ),
   
    sliderInput(
      ns("mingssize"), 
      label = "Select minimum size of each gene set", 
      min = 1, 
      max = 100, 
      value = 10,
      popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(
      ns("maxgssize"),
      label = "Select maximum size of each gene set", 
      min = 100, 
      max = 1000,
      value = 500,
      popup = "Maximum size of each gene set used for analyzing"
      ), 
    prettySwitch(
      ns("readable"),
      label = "Readable",
      value = FALSE, 
      status = "warning",
      popup = "whether mapping gene ID to gene Name"
      ), 
    prettySwitch(
      ns("pool"), 
      label = "Pool",
      value = FALSE, 
      status = "warning",
      popup = "Need to be switch on if all subontologies are chosen"
      ),
             )),
    
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich", onclick="loading_modal_open(); stopWatch();"),
              htmlOutput(ns("close_loading_modal")),  # Close modal with JS 
              htmlOutput((ns("adv_settings")))
    )
  )
}

#' GO Server Function 
#' 
#' @noRd
mod_enrichGO_server <- function(input, output, session, con, Description1_ui_1, module_overview_ui_1){
  ns <- session$ns
  
  enrichGO_module <- reactiveValues()
  x <- reactiveVal(1) # Reactive value to record if the input button is pressed
  
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    tagList(
      selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for enrichment analysis."),
      uiOutput(ns("error"))
    )
  })
  
  observeEvent(c(Description1_ui_1$module_name, module_overview_ui_1$delete$delete), {
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    updateSelectInput(session, "module_object", choices = module_objects)
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    output$error <- renderUI({})
    
    module_genes <- try(get_module_genes(input$module_object, con = con))
    background_genes <- try(get_background_genes(input$module_object, con = con))

    enrichment_object <- try(clusterProfiler::enrichGO(gene = module_genes,
                                                       OrgDb = 'org.Hs.eg.db', 
                                                       keyType = input$keytype, 
                                                       ont = input$ont, 
                                                       pvalueCutoff = input$pvaluecutoff, 
                                                       pAdjustMethod = input$padjustmethod, 
                                                       universe = background_genes, 
                                                       qvalueCutoff = input$pvaluecutoff, 
                                                       minGSSize = input$mingssize, 
                                                       maxGSSize = input$maxgssize, 
                                                       readable = input$readable,
                                                       pool = input$pool
    ))
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
      enrichGO_module$enrich <- c(x(), "enrichGO")  # Reactive value to record if the input button is pressed 
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(enrichment_object,
                                          module_name = module_name, 
                                          enrichment_method = "enrichGO", 
                                          con = con)
    }
    # Close loading modal
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(enrichGO_module)
}

## To be copies in the UI 
# mod_enrichGO_ui("enrichGO_ui_1")

## To be copied in the server
# callModule(mod_enrichGO_server, "enrichGO_ui_1")


    
    
    
    