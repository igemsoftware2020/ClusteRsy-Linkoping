#' DiffCoEx UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DiffCoEx_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    textInput(ns("module_name"), "Module object name"),
    radioButtons(ns("clustermethod"), "Select a cluster method:", 
                 choices = c("ward",
                             "single",
                             "complete",
                             "average",
                             "mcquitty",
                             "median",
                             "centroid"),
                selected = "ward",
                width = NULL),
    selectInput(ns("cor_method"), "Select an correlation coefficent",
                choices = c("pearson",
                            "kendall",
                            "spearman"),
                selected = "pearson",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL),
    selectInput(ns("cuttree_method"), "Select a method to use",
                choices = c("hybrid",
                            "tree"),
                multiple = FALSE,
                selectize = TRUE,
                width = NULL),
   
    htmlOutput(ns("para")),   
    numericInput(ns("beta"), label = "Soft thresholding power (%)", value = 99, max = 100, min = 0),
    sliderInput(ns("minClusterSize"), label = "Minimum cluster size", min = 0, max = 100, value = 5),
    sliderInput(ns("cut_height"), label = "Maximum joining heights", min = 0, max = 1, value = 0.1),
    sliderInput(ns("pval_cutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05),
    actionButton(ns("load_input"), "Infer DiffCoEx module")
    )

}
    
#' DiffCoEx Server Function
#'
#' @noRd 
mod_DiffCoEx_server <- function(input, output, session, con){
  ns <- session$ns
 
  observeEvent(input$cuttree_method, {
    if (input$cuttree_method == "hybrid") {
    output$para <- renderUI({
      tagList(
      radioButtons(ns("deepSplit"), label = "Sensitivity to cluster splitting", choices = c(1, 2, 3, 4), selected = 2,
                     inline = T),
      prettySwitch(ns("pamRespectsDendro"), label = "PAM respects dendrogram", value = FALSE, status = "warning"))})}
    else {
    output$para <- renderUI({
      tagList(
      prettySwitch(ns("deepSplit"), label = "Sensitivity to cluster splitting", value = FALSE, status = "warning"))})}
  })
    
    
   output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
  })

  observeEvent(input$load_input, {
    module_object <- MODifieRDB::DiffCoEx(input_name = input$input_object, 
                                          ppi_name = input$ppi_object,
                                          cluster_method = input$cluster_method,
                                          cor_method = input$cor_method,
                                          cuttree_method = input$cuttree_method,
                                          minClusterSize = input$minClusterSize,
                                          deepSplit = input$deepSplit,
                                          pamRespectsDendro = input$pamRespectsDendro,
                                          cut_height = input$cut_height,
                                          pval_cutoff = input$pval_cutoff,
                                          beta = input$beta,
                                          module_name = input$module_name,
                                          con = con)
    
    
  })
}
    
## To be copied in the UI
# mod_DiffCoEx_ui("DiffCoEx_ui_1")
    
## To be copied in the server
# callModule(mod_DiffCoEx_server, "DiffCoEx_ui_1")
 
