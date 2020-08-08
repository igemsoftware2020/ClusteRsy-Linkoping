#' Correlation_clique_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Correlation_clique_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("module_genes_table")),
    DT::dataTableOutput(ns("frequency_table")),
    DT::dataTableOutput(ns("settings_table"))
  )
}
    
#' Correlation_clique_post_processing Server Function
#'
#' @noRd 
mod_Correlation_clique_post_processing_server <- function(input, output, session, inspected_module, con){
  ns <- session$ns
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  frequency_table <- as.matrix(inspected_module$frequency_table)
  colnames(frequency_table) <- list("Frequency")
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes})
  
  output$frequency_table <- DT::renderDataTable({frequency_table})
  
  output$settings_table <- DT::renderDataTable({settings})
 
}
    
## To be copied in the UI
# mod_Correlation_clique_post_processing_ui("Correlation_clique_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Correlation_clique_post_processing_server, "Correlation_clique_post_processing_ui_1")
 
