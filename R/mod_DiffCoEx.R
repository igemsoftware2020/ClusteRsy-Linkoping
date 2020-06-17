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
<<<<<<< HEAD
    selectInput(ns("cluster_method"), "Select an agglomeration method", 
=======
    actionButton(ns("load_input"), "Infer DiffCoEx module"),
    radioButtons(clustermethod, "Select a cluster method:", 
>>>>>>> 97d09f791b2578dc0d775698bbf5543b5f0aae0f
                 c("ward",
                   "single",
                   "complete",
                   "average",
                   "mcquitty",
                   "median",
                   "centroid"),
                selected = "ward",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL),
    selectInput(ns("cor_method"), "Select an correlation coefficent",
                c("pearson",
                  "kendall",
                  "spearman"),
                selected = "pearson",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL),
    selectInput(ns("cuttree_method"), "Select a method to use",
                c("hybrid",
                  "tree"),
                multiple = FALSE,
                selectize = TRUE,
                width = NULL),
   
     htmlOutput(ns("method")),   
    
    sliderInput(ns("minClusterSize"), label = "minimum cluster size", min = 0, max = 100, value = 5),
    sliderInput(ns("cut_height"), label = "maximum joining heights", min = 0, max = 1, value = 0.1),
    sliderInput(ns("pval_cutoff"), label = "p-value cut-off", min = 0, max = 1, value = 0.05),
    actionButton(ns("load_input"), "Infer DiffCoEx module")
    )
<<<<<<< HEAD
  
=======
>>>>>>> 97d09f791b2578dc0d775698bbf5543b5f0aae0f
}
    
#' DiffCoEx Server Function
#'
#' @noRd 
mod_DiffCoEx_server <- function(input, output, session){
  ns <- session$ns
 
  observeEvent(input$cuttree_method, {
               if (input$cuttree_method == "hybrid") {output$method <- 
    renderUI({
      tagList(
      sliderInput(ns("deepSplit"), label = "sensitivity to cluster splitting", min = 1, max = 4, value = 0.1),
      numericInput(ns("beta"), label = "soft thresholding power (%)", value = 99, max = 100, min = 0),
      prettySwitch(ns("pamRespectsDendro"), label = "PAM respects dendrogram", value = FALSE, status = "warning"))})}
    else {output$method <- 
      renderUI({
      tagList(
      prettySwitch(ns("deepSplit"), label = "sensitivity to cluster splitting", value = FALSE, status = "warning"),
      numericInput(ns("beta"), label = "soft thresholding power (%)", value = 0.99, max = 100, min = 0))})}
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
 
