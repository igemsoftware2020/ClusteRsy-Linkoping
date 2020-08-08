#' DIAMoND_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DIAMoND_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("module_genes_table")),
  DT::dataTableOutput(ns("seed_genes_table")),
  DT::dataTableOutput(ns("ignored_genes_table")),
  DT::dataTableOutput(ns("added_genes_table")),
  DT::dataTableOutput(ns("settings_table"))
  )
}
    
#' DIAMoND_post_processing Server Function
#'
#' @noRd 
mod_DIAMoND_post_processing_server <- function(input, output, session, inspected_module, con){
  ns <- session$ns
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  seed_genes <- as.matrix(inspected_module$seed_genes)
  colnames(seed_genes) <- list("Seed genes")
  
  ignored_genes <- as.matrix(inspected_module$ignored_genes)
  colnames(ignored_genes) <- list("Ignored genes")
  
  added_genes <- inspected_module$added_genes
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes})
  
  output$seed_genes_table <- DT::renderDataTable({seed_genes})
  
  output$ignored_genes_table <- DT::renderDataTable({ignored_genes})
  
  output$added_genes_table <- DT::renderDataTable({added_genes})
  
  output$settings_table <- DT::renderDataTable({settings})
 
}
    
## To be copied in the UI
# mod_DIAMoND_post_processing_ui("DIAMoND_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_DIAMoND_post_processing_server, "DIAMoND_post_processing_ui_1")
 
