#' MODA_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MODA_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("module_genes_table")),
  DT::dataTableOutput(ns("group1_modules_table")),
  DT::dataTableOutput(ns("group2_modules_table")),
  DT::dataTableOutput(ns("jaccard_table_table")),
  DT::dataTableOutput(ns("settings_table"))
  )
}
    
#' MODA_post_processing Server Function
#'
#' @noRd 
mod_MODA_post_processing_server <- function(input, output, session, inspected_module, con){
  ns <- session$ns
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  group1_genes <- as.matrix(inspected_module$group1_modules)
  colnames(group1_genes) <- list("Group 1 genes")
  
  group2_genes <- as.matrix(inspected_module$group2_modules)
  colnames(group2_genes) <- list("Group 2 genes")
  
  jaccard_table <- inspected_module$jaccard_table
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes})
  
  output$group1_modules_table <- DT::renderDataTable({group1_genes})
  
  output$group2_modules_table <- DT::renderDataTable({group2_genes})
  
  output$jaccard_table_table <- DT::renderDataTable({jaccard_table})
  
  output$settings_table <- DT::renderDataTable({settings})
}
    
## To be copied in the UI
# mod_MODA_post_processing_ui("MODA_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_MODA_post_processing_server, "MODA_post_processing_ui_1")
 
