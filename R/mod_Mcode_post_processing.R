#' Mcode_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Mcode_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("module_genes_table")),
  DT::dataTableOutput(ns("modules_table")),
  DT::dataTableOutput(ns("module_score_table")),
  DT::dataTableOutput(ns("settings_table"))
  )
}
    
#' Mcode_post_processing Server Function
#'
#' @noRd 
mod_Mcode_post_processing_server <- function(input, output, session, inspected_module, con){
  ns <- session$ns
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  modules <- as.matrix(inspected_module$modules)
  colnames(modules) <- list("Modules")
  
  module_score <- as.matrix(inspected_module$module_scores)
  colnames(module_score) <- list("Module Score")
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes})
  
  output$modules_table <- DT::renderDataTable({modules})
  
  output$module_score_table <- DT::renderDataTable({module_score})
  
  output$settings_table <- DT::renderDataTable({settings})
 
}
    
## To be copied in the UI
# mod_Mcode_post_processing_ui("Mcode_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_Mcode_post_processing_server, "Mcode_post_processing_ui_1")
 
