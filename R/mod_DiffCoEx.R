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
    tags$div(id = "error_name_DiffCoEx_js",
    textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods", placeholder = "Module name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    radioButtons(ns("cluster_method"), "Select a cluster method:", 
                 choices = c("ward",
                             "single",
                             "complete",
                             "average",
                             "mcquitty",
                             "median",
                             "centroid"),
                selected = "ward",
                width = NULL,
                popup = "The agglomeration method to be used"),
    selectInput(ns("cor_method"), "Select an correlation coefficent",
                choices = c("pearson",
                            "kendall",
                            "spearman"),
                selected = "pearson",
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                popup = "Decide which correlation (or covariance) coefficient to be computed"),
    selectInput(ns("cuttree_method"), "Select a method to use",
                choices = c("hybrid",
                            "tree"),
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                popup = "Which method will be used to cut the dendrogram"),
   
    htmlOutput(ns("para")),   
    numericInput(ns("beta"), label = "Soft thresholding power (%)", value = 99, max = 100, min = 0, popup = "A soft threshold defined by the user"),
    sliderInput(ns("minClusterSize"), label = "Minimum cluster size", min = 0, max = 100, value = 5, popup = "Minimum cluster size"),
    sliderInput(ns("cut_height"), label = "Maximum joining heights", min = 0, max = 1, value = 0.1, popup = "Maximum height of joins in the dendrogram that will be considered"),
    sliderInput(ns("pval_cutoff"), label = "P-value cut-off", min = 0, max = 1, value = 0.05, popup = "P-value cutoff for significant co-expression modules"),
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer DiffCoEx module", onclick="loading_modal_open(); stopWatch()"),
    htmlOutput(ns("close_loading_modal"))  # Close modal with JS
    )
  )
}
    
#' DiffCoEx Server Function
#'
#' @noRd 
mod_DiffCoEx_server <- function(input, output, session, con, upload_ui_1, input_overview_ui_1){
  ns <- session$ns
  
  DiffCoEx_module <- reactiveValues()
 
  observeEvent(input$cuttree_method, {
    if (input$cuttree_method == "hybrid") {
    output$para <- renderUI({
      tagList(
      radioButtons(ns("deepSplit"), label = "Sensitivity to cluster splitting", choices = c(1, 2, 3, 4), selected = 2,
                     inline = T),
      shinyWidgets::prettySwitch(ns("pamRespectsDendro"), label = "PAM respects dendrogram", value = FALSE, status = "warning"))})}
    else {
    output$para <- renderUI({
      tagList(
      shinyWidgets::prettySwitch(ns("deepSplit"), label = "Sensitivity to cluster splitting", value = FALSE, status = "warning"))})}
  })
    
    
   output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects, popup = "The input used for analyzation")
  })
   
   observeEvent(c(upload_ui_1$input_name, input_overview_ui_1$delete$delete), {
     input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
     updateSelectInput(session, "input_object", choices = input_objects)
   })

   module_name <- reactive({
     input$module_name
   })
   
   observe({
     if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
       output$error_name_js <- renderUI({
         tags$script(HTML("element = document.getElementById('error_name_DiffCoEx_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-DiffCoEx_ui_1-load_input').disabled = true;"))
       })
       output$error_name_descrip <- renderUI({
         tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
                style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
       })
     } else {
       output$error_name_js <- renderUI({
         tags$script(HTML("document.getElementById('error_name_DiffCoEx_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-DiffCoEx_ui_1-load_input').disabled = false;"))
       })
       output$error_name_descrip <- NULL
     }
   }) 
   
   
  observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module_object <- try(MODifieRDB::diffcoex_db(input_name = input$input_object,
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
    )
    if (class(module_object) == "try-error"){
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), module_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      DiffCoEx_module$module_name <- module_name()
      updateTextInput(session, "module_name", value = character(0))
    }
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(DiffCoEx_module)
}
    
## To be copied in the UI
# mod_DiffCoEx_ui("DiffCoEx_ui_1")
    
## To be copied in the server
# callModule(mod_DiffCoEx_server, "DiffCoEx_ui_1")
 
