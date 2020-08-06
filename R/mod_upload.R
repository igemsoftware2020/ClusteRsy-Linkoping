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
      tags$div(id = "hide_matrix",
               `data-intro`="Begin by uploadnig an expression matrix with your data.",
               `data-step`=1,
      fileInput(ns("expression_matrix"), 
                label = "Upload an expression matrix",
                accept = c("text/csv", 
                           "text/plain", 
                           "application/vnd.ms-excel", 
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                           "text/tab-separated-values", ".rds"),
                popup = tags$p("Read more about how to upload your data in our",
                                shinyLink(to = "user_guide", label = "User Guide")) #This doesn't work when in the tooltip, need to figure out why.. 
                )
      ),
      uiOutput(ns("sample_chooser")),
      tags$div(id = "hide_rds",
      tags$div(fileInput(ns("input_object_rds"), label = "Upload an input object", accept = ".rds"), `data-hint`="Here you can upload an alreday formated input object! You can find these under the database tab.", `data-hintPosition`="top-right")
      ),
      uiOutput(ns("input_name_chooser")),
      htmlOutput(ns("error_name_js")),
    )
  }


#' upload Server Function
#'
#' @noRd 
mod_upload_server <- function(input, output, session, con){
  ns <- session$ns
  options(shiny.maxRequestSize = 50*1024^2)
  upload_module <- reactiveValues()


  registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
    if (is.null(data))
      NULL
    else
      list(left=as.character(data$left), right=as.character(data$right))
  }, force = TRUE)
  
  # File handler
  upload_expression <- reactive({
    req((input$expression_matrix))
    infile <- (input$expression_matrix$datapath)
    if (is.null(infile)){
      return(NULL)
    }
    if(input$expression_matrix$type == "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"){
      readxl::read_excel(infile)
    }
    else {
      read.table(file = infile, header = T) 
    }
  })
  
  
  
  output$sample_chooser <- renderUI({
    expression_matrix <- upload_expression()
    tagList( 
      tags$script(HTML("document.getElementById('hide_rds').style.display = 'none';")),
      tags$div(id = "error_name_js",
      textInput(ns("input_name"), "Input object name", placeholder = "Input name")),
      htmlOutput(ns("error_name_descrip")),
      textInput(ns("group1"), "Group 1 label", placeholder = "Group 1 label"),
      textInput(ns("group2"), "Group 2 label", placeholder = "Group 2 label"),
      chooserInput(ns("sample_groups"), "Available frobs", "Selected frobs",
      colnames(expression_matrix), c(), multiple = TRUE),
      tags$br(),
      uiOutput(ns("error_empty_group")),
      shinyWidgets::prettySwitch(ns("adjusted_pvalue"), label = "Pvalue", value = TRUE, status = "warning"),
      shinyWidgets::prettySwitch(ns("quantile_normalization"), label = "Quantile", value = FALSE, status = "warning"),
      tags$div(style = "text-align:center",
      actionButton(ns("create_input"), "Create input object", onclick="loading_modal_open(); stopWatch();"),
      htmlOutput(ns("close_loading_modal")) # Close modal with JS
      )
    )
  })
  
  input_name <- reactive({
    input$input_name
  })
  
  observe({
    if (any(MODifieRDB::get_available_input_objects(con)$input_name == input_name())){
    output$error_name_js <- renderUI({
      tags$script(HTML("element = document.getElementById('error_name_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-upload_ui_1-create_input').disabled = true;"))
      })
    output$error_name_descrip <- renderUI({
      tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
             style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
    })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-upload_ui_1-create_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
  }
    })
  
  group1_label_r <- reactive({
    input$group1
  })
  
  group2_label_r <- reactive({
    input$group2
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(upload_expression()))
  })
  
  observeEvent(input$create_input, {
    
    input_name <- input_name()
    upload_module$input_name <- input_name #This creates reactive value and is sent to the Columns module
    
    id <- showNotification("Creating input object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)

    count_matrix <- as.matrix(upload_expression())
    group1_indici <- match(input$sample_groups[[1]], colnames(count_matrix))
    group2_indici <- match(input$sample_groups[[2]], colnames(count_matrix))
    group1_label <- group1_label_r()
    group2_label <- group2_label_r()
    use_adjusted <- input$adjusted_pvalue
    normalize_quantiles <- input$quantile_normalization
    
    output$error_empty_group <- NULL
    
    input_object <- try(MODifieR::create_input_rnaseq(count_matrix = count_matrix, 
                                              group1_indici = group1_indici, 
                                              group2_indici = group2_indici, 
                                              group1_label = group1_label, 
                                              group2_label = group2_label, 
                                              use_adjusted = use_adjusted, 
                                              normalize_quantiles = normalize_quantiles)
                        )
  
    if(class(input_object) == "try-error"){
        output$error_empty_group <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), input_object,
                 style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
        })
    } else {
      updateTextInput(session, "input_name", value = character(0))
      updateTextInput(session, "group1", value = character(0))
      updateTextInput(session, "group2", value = character(0))
      MODifieRDB::MODifieR_object_to_db(MODifieR_object = input_object,
                                        object_name = input_name,
                                        con = con)
    }
    output$close_loading_modal <- renderUI({
     tags$script("loading_modal_close(); reset();")
    })
  })
  
  # File input for .rds files 
  # Reactive function for fileinput
  upload_input_reactive <- reactive({
    req(input$input_object_rds)
    infile <- (input$input_object_rds$datapath)
    if (is.null(infile)){
      
      return(NULL)
    }
    readRDS(file = infile)
  })
  
  # File input
  output$input_name_chooser <- renderUI({
    input <- upload_input_reactive() #reactive pop up
    tagList( 
      tags$script(HTML("document.getElementById('hide_matrix').style.display = 'none';")),
      textInput(ns("input_name_rds"), "Input object name", placeholder = "Input name"),
      actionButton(ns("upload_input_rds"), "Add input object to database"),
    )
  })
  
  input_name_rds <- reactive({
    input$input_name_rds
  })
  
  upload_input_rds <- reactive({
    input$upload_input_rds
  }
  )
  
  observeEvent(input$upload_input_rds, {
    upload_input_rds <- upload_input_rds()
    upload_module$upload_input_rds <- upload_input_rds #This creates reactive value and is sent to the Columns module
  })
  
  # Upload input object
  observeEvent(input$upload_input_rds, {
    id <- showNotification("Saving input object to database", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    input <- upload_input_reactive()
    input_name_rds <- input_name_rds()
    
    MODifieRDB::MODifieR_object_to_db(MODifieR_object = input,
                                      object_name = input_name_rds,
                                      con = con)
  })
  
  #---
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  return(upload_module)

}

## To be copied in the UI
# mod_upload_ui("upload_ui_1")

## To be copied in the server
# callModule(mod_upload_server, "upload_ui_1")