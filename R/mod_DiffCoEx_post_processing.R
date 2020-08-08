#' DiffCoEx_post_processing UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DiffCoEx_post_processing_ui <- function(id){
  ns <- NS(id)
  tagList(
  DT::dataTableOutput(ns("module_genes_table")),
  DT::dataTableOutput(ns("module_p_values_table")),
  DT::dataTableOutput(ns("color_vector_table")),
  DT::dataTableOutput(ns("settings_table"))
  )
}
    
#' DiffCoEx_post_processing Server Function
#'
#' @noRd 
mod_DiffCoEx_post_processing_server <- function(input, output, session, inspected_module, con){
  ns <- session$ns
  
  module_genes <- as.matrix(inspected_module$module_genes)
  colnames(module_genes) <- list("Module genes")
  
  module_p_values <- inspected_module$module_p_values
  
  color_vector <- as.matrix(inspected_module$color_vector)
  colnames(color_vector) <- list("Gene")
  
  settings <- as.matrix(inspected_module$settings)
  colnames(settings) <- list("Settings used")
  
  output$module_genes_table <- DT::renderDataTable({module_genes})
  
  output$module_p_values_table <- DT::renderDataTable({module_p_values})
 
  output$color_vector_table <- DT::renderDataTable({color_vector})
  
  output$settings_table <- DT::renderDataTable({settings})
}
    
## To be copied in the UI
# mod_DiffCoEx_post_processing_ui("DiffCoEx_post_processing_ui_1")
    
## To be copied in the server
# callModule(mod_DiffCoEx_post_processing_server, "DiffCoEx_post_processing_ui_1")
 
