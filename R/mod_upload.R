#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("expression_matrix"), label = "Upload an expression matrix"),
    uiOutput(ns("sample_chooser"))
  )
}

#' upload Server Function
#'
#' @noRd 
mod_upload_server <- function(input, output, session, input_object){
  ns <- session$ns
  
  MODifieR_module <- reactiveValues()

  registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data))
      NULL
    else
      list(left=as.character(data$left), right=as.character(data$right))
  }, force = TRUE)

  upload_expression <- reactive({
    req((input$expression_matrix))
    infile <- (input$expression_matrix$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    
    read.table(file = infile, header = T)
  })
 
  output$sample_chooser <- renderUI({
    expression_matrix <- upload_expression()
    tagList( textInput(ns("group1"), "Group 1 label"),
             textInput(ns("group2"), "Group 2 label"),
             chooserInput(ns("sample_groups"), "Available frobs", "Selected frobs", 
                          colnames(expression_matrix), c(), size = 10, multiple = TRUE),
             verbatimTextOutput(ns("current_groups")),
             prettySwitch(ns("adjusted_pvalue"), label = "Pvalue", value = TRUE, status = "warning"),
             prettySwitch(ns("quantile_normalization"), label = "Quantile", value = FALSE, status = "warning"),
             tags$div(style = "text-align: right",
             actionButton(ns("create_input"), "Create input object")
             )
    )
  })
  
  group1_label_r <- reactive({
    input$group1
  })
  
  group2_label_r <- reactive({
    input$group2
  })
  
  output$current_groups <- renderPrint({
    groups <- input$sample_groups
    names(groups) <- c(group1_label_r(), group2_label_r())
    groups
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(upload_expression()))
  })
  
  observeEvent(input$create_input, {
    id <- showNotification("Creating input object", duration = NULL, closeButton = FALSE)
    count_matrix <- as.matrix(upload_expression())
    group1_indici <- match(input$sample_groups[[1]], colnames(count_matrix))
    group2_indici <- match(input$sample_groups[[2]], colnames(count_matrix))
    group1_label <- group1_label_r()
    group2_label <- group2_label_r()
    use_adjusted <- input$adjusted_pvalue
    normalize_quantiles <- input$quantile_normalization
    
    on.exit(removeNotification(id), add = TRUE)
    
    input_object <- MODifieR::create_input_rnaseq(count_matrix = count_matrix, 
                                                  group1_indici = group1_indici, 
                                                  group2_indici = group2_indici, 
                                                  group1_label = group1_label, 
                                                  group2_label = group2_label, 
                                                  use_adjusted = use_adjusted, 
                                                  normalize_quantiles = normalize_quantiles)
    
    input_name <- input$input_name
    
    MODifieRDB::MODifieR_object_to_db(MODifieR_object = input_object,
                                      object_name = input_name,
                                      con = con)
    
    
    MODifieR_module$module <- input_object
  })
  
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  return(MODifieR_module)

}

## To be copied in the UI
# mod_upload_ui("upload_ui_1")

## To be copied in the server
# callModule(mod_upload_server, "upload_ui_1")

