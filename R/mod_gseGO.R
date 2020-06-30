#' GO UI Function 
#' 
#' @description A shiny Module.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
#' 
#' @importFrom shiny NS tagList
mod_gseGO_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("module_input")),
    selectInput(
      ns("keytype"), 
      label = "Select the type of gene input", 
      choices = c(keytypes(org.Hs.eg.db::org.Hs.eg.db)),
      popup = "Select the type of the input data"
    ),
    selectInput(
      ns("ont"),
      label = "Select subontologies", 
      choices= c("BP", "MF", "CC", "ALL"),
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
    selectInput(
      ns("padjustmethod"), 
      label = "Select adjustment method", 
      choices = c ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none "),
      popup = "Correction methods used to control p-values and q-values"
    ),
    sliderInput(
      ns("exponent"), 
      label = "Select weight of each step", 
      min = 0,
      max = 5, 
      value = 1,
      popup = "Weight of each step"
    ),
    sliderInput(
      ns("mingssize"),
      label = "Select minimum size of each gene set",
      min = 1,
      max = 100, 
      value = 10,
      popup = "Minimum size of each gene set used for analyzing"
    ),
    sliderInput(
      ns("maxgssize"), 
      label = "Select maximum size of each gene set", 
      min = 100, 
      max = 1000, 
      value = 500,
      popup = "Maximum size of each gene set used for analyzing"
    ), 
    sliderInput(
      ns("nperm"), 
      label = "Permutation numbers", 
      min = 1, 
      max = 5000, 
      value = 1000,
      popup = "Number of permutations that should be performed"
    ), 
    prettySwitch(
      ns("seed"), 
      label = "Logical",
      value = FALSE,
      status = "warning",
      popup = "Get reproducible results"
    ), 
    selectInput(
      ns("by"), 
      label = "Select", 
      choices = c("fgsea", "DOSE"),
      popup = "Algorithm used for the gene set enrichment analysis"
    ), 
    
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Enrich") 
    )
  )
}

#' GO Server Function 
#' 
#' @noRd
mod_gseGO_server <- function(input, output, session, con){
  ns <- session$ns
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, 
                popup = "The module used for gene set enrichment analysis.")
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating gene set enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    gene_list <- get_sorted_module_genes(input$module_object, con = con)
    
    gse_object <- try(clusterProfiler::gseGO(geneList = gene_list,
                                                    ont = input$ont, 
                                                    OrgDb = 'org.Hs.eg.db',
                                                    keyType = input$keytype, 
                                                    exponent = input$exponent, 
                                                    nPerm = input$nperm, 
                                                    minGSSize = input$mingssize, 
                                                    maxGSSize = input$maxgssize, 
                                                    pvalueCutoff = input$pvaluecutoff, 
                                                    pAdjustMethod = input$padjustmethod, 
                                                    verbose = FALSE, 
                                                    seed = input$seed, 
                                                    by = input$by
    ))
    if (class(gse_object) == "try-error"){
      output$error_p_value <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), gse_object)
      })
    }
  })
}

## To be copies in the UI 
# mod_gseGO_ui("gseGO_ui_1")

## To be copied in the server
# callModule(mod_gseGO_server, "gseGO_ui_1")


