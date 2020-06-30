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
      ns("ont"),
      label = "Select subontologies", 
      choices= c("BP", "MF", "CC", "ALL")
    ),
    selectInput(
      ns("keyType"), 
      label = "Select keyType of gene", 
      choices = c(keytypes(org.Hs.eg.db::org.Hs.eg.db))
    ),
    sliderInput(
      ns("exponent"), 
      label = "Select weight of each step", 
      min = 0,
      max = 5, 
      value = 1
    ),
    sliderInput(
      ns("nPerm"), 
      label = "Permutation numbers", 
      min = 1, 
      max = 5000, 
      value = 1000
    ), 
    sliderInput(
      ns("minGSSize"),
      label = "Select minimal size of each geneSet",
      min = 1,
      max = 100, 
      value = 10
    ),
    sliderInput(
      ns("maxGSSize"), 
      label = "Select maximal size of each geneSet", 
      min = 100, 
      max = 1000, 
      value = 500
    ), 
    sliderInput(
      ns("pvalueCutoff"), 
      label = "Pvalue cutoff", 
      min = 0, 
      max = 1, 
      value = 0.05
    ), 
    selectInput(
      ns("pAdjustMethod"), 
      label = "Select adjustment method", 
      choices = c ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none ")
    ),
    prettySwitch(
      ns("verbose"), 
      label = "Print messege or not", 
      value = FALSE, 
      status = "warning"
    ), 
    prettySwitch(
      ns("seed"), 
      label = "Logical",
      value = FALSE,
      status = "warning"
    ), 
    selectInput(
      ns("by"), 
      label = "Select", 
      choices = c("fgsea", "DOSE")
    ), 
    
    #tags$div(style = "text-align:center",
             actionButton(ns("load_input"), "Create gene set enrichment analysis object")
    #)
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
                                                    keyType = input$keyType, 
                                                    exponent = input$exponent, 
                                                    nPerm = input$nPerm, 
                                                    minGSSize = input$minGSSize, 
                                                    maxGSSize = input$maxGSSize, 
                                                    pvalueCutoff = input$pvalueCutoff, 
                                                    pAdjustMethod = input$pAdjustMethod, 
                                                    verbose = input$verbose, 
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


