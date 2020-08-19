#' gseNCG UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_gseNCG_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_enrich',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_enrich", class = "collapse",
             tags$div(
    sliderInput(ns("pvaluecutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05, popup = "Rejecting the null hypothesis for any result with an equal or smaller value"),
    selectInput(ns("padjustmethod"), "Select an adjustment method",
                choices = c("holm",
                            "hochberg",
                            "hommel",
                            "bonferroni",
                            "BH",
                            "BY",
                            "fdr",
                            "none"),
                popup = "Correction methods used to control p-values and q-values",
                multiple = FALSE,
                selectize = TRUE),
    sliderInput(ns("exponent"), label = "Exponent", value = 5, max = 100, min = 0, popup = "Weight of each step"),
    sliderInput(ns("mingssize"), label = "Minimum size of each gene set", value = 5, max = 100, min = 0, popup = "Minimum size of each gene set used for analyzing"),
    sliderInput(ns("maxgssize"), label = "Maximum size of each gene set", value = 500, max = 5000, min = 0, popup = "Maximum size of each gene setused for analyzing"),
    sliderInput(ns("nperm"), label = "Permutation number", min = 1, max = 1000, value = 500, popup = "Number of permutations that should be performed"),
    prettySwitch(ns("include_seed"), label = "Include seed", value = FALSE, status = "warning", popup = "Get reproducible results"),
    selectInput(ns("by"), label = "Select algorithm",
                choices = c("fgsea",
                            "DOSE"),
                popup = "Algorithm used for the gene set enrichment analysis"),
             )),
    
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich", onclick="loading_modal_open(); stopWatch();"),
              htmlOutput(ns("close_loading_modal")),  # Close modal with JS 
              htmlOutput((ns("adv_settings")))
    )
  )
}
    
#' gseNCG Server Function
#'
#' @noRd 
mod_gseNCG_server <- function(input, output, session, con, Description1_ui_1, module_overview_ui_1){
  ns <- session$ns
  
  gseNCG_module <- reactiveValues()
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
    output$adv_settings <- renderUI({})
    
    gene_list <- try(get_sorted_module_genes(input$module_object, con = con))
   
    gse_object <- try(DOSE::gseNCG(
                                  geneList = gene_list,
                                  exponent = input$exponent,
                                  nPerm = input$nperm,
                                  pvalueCutoff = input$pvaluecutoff,
                                  pAdjustMethod = input$padjustmethod,
                                  minGSSize = input$mingssize,
                                  maxGSSize = input$maxgssize,
                                  by = input$by,
                                  seed = input$include_seed,
                                  verbose = FALSE  
      
    )
    )
    if (any(c(class(gse_object), class(gene_list)) == "try-error")){
      output$adv_settings <- renderUI({
        tags$script("if ($('.collapsible.btn.btn-primary.btn-block').eq(1).attr('aria-expanded') === 'false') {
                            $('.collapsible.btn.btn-primary.btn-block').eq(1).click();
                    }")
      })
      output$error <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), gse_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      x(x() + 1)
      gseNCG_module$enrich <- c(x(), "gseNCG")  # Reactive value to record if the input button is pressed
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(gse_object,
                                          module_name = module_name, 
                                          enrichment_method = gse_object@setType, 
                                          con = con)
    }
    # Close loading modal
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(gseNCG_module)
}
    
## To be copied in the UI
# mod_gseNCG_ui("gseNCG_ui_1")
    
## To be copied in the server
# callModule(mod_gseNCG_server, "gseNCG_ui_1")
 
