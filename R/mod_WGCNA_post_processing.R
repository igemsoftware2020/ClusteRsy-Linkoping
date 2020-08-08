#' WGCNA_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_WGCNA_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("module_genes_table")),
    DT::dataTableOutput(ns("info_table_table")),
    DT::dataTableOutput(ns("correlation_to_trait_table_table")),
    DT::dataTableOutput(ns("settings_table"))
  )
}
    
#' WGCNA_post_processing Server Function
#'
#' @noRd 
mod_WGCNA_post_processing_server <- function(input, output, session, inspected_module, con){
  ns <- session$ns
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  info_table <- inspected_module$info_table
  
  correlation_to_train_table <- inspected_module$correlation_to_trait_table
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes})
  
  output$info_table_table <- DT::renderDataTable({info_table})
  
  output$correlation_to_trait_table_table <- DT::renderDataTable({correlation_to_train_table})
  
  output$settings_table <- DT::renderDataTable({settings})
  
  
}
    
## To be copied in the UI
# mod_WGCNA_post_processing_ui("WGCNA_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_WGCNA_post_processing_server, "WGCNA_post_processing_ui_1")
 
