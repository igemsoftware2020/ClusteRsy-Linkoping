#' WGCNA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_WGCNA_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    tags$div(id = "error_name_WGCNA_js",
    textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods", placeholder = "Module name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_mod', "href"='#advanced_mod',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_mod", class = "collapse",
             tags$div(
    radioButtons(
      ns("group_of_interest"), 
      label= "Select group of interest", 
      choiceNames=c("Group 1", "Group 2"), 
      choiceValues = c(1, 2), 
      popup = "A numeric value indicating if group 1 or group 2 contains the condition of interest."),
    
    sliderInput(
      ns("minModuleSize"), 
      label= "Minimum module size", 
      min=1, 
      max=100, 
      value=30, 
      popup = "The minimum model size for module detection."),
    
    sliderInput(
      ns("deepSplit"), 
      label= "Integer value between 0 and 4", 
      min=0, 
      max=4, 
      value=2, 
      popup = "An integer value between 0 and 4 which provides a simplified control over the module detection sensitivity for the module splitting. 0 is the least sensitive and 4 is the most sensitive."),
    
    prettySwitch(
      ns("pamRespectsDendro"), 
      label= "Select pamStage", 
      value=FALSE, status = "warning", 
      popup = "This is only used if pamStage is TRUE. If so then the PAM stage will respect the dendrogram."),
    
    sliderInput(
      ns("mergeCutHeight"), 
      label = "Dendrogram cut height for module merging", 
      min=0, 
      max=1, 
      value=0.1, 
      popup = "Module merging dendrogram cut height."), 
    
    radioButtons(
      ns("numericLabels"), 
      label="Sholud the returned modules be labeled by color or numbers?", 
      choiceNames=c("Color", "Numbers"), 
      choiceValues = c(F,T), 
      popup = "If the returned modules should be labeled by numbers or by colors."),
    
    sliderInput(
      ns("pval_cutoff"), 
      label="P-value cutoff for significant co-expression modules", 
      min=0, 
      max=1, 
      value=0.05,
      popup = "The significant co-expression modules p-value cutoff."),
    
    radioButtons(
      ns("corType"), 
      label="Specify correlation", 
      choiceNames = c("Pearson", "Bicor"), 
      choiceValues = c("pearson", "bicor"), 
      popup = "Specifying the correlation to be used. “Pearson'' indicates Pearson midcorrelation and “Bicor'' indicates bidweight midcorrelation."),
    
    sliderInput(
      ns("maxBlockSize"), 
      label="Max block size", 
      min=0, 
      max=10000, 
      value=5000, 
      popup = "The maximum block size for module detection."),
    
    selectInput(
      ns("TOMType"), 
      label="Select TOMType", 
      c("None", "Unsigned", "Signed", "Signedtoup Nowick", "Unsigned 2", "Signed 2", "Signed Nowick 2"), 
      popup = "The type of consensus topological overlap matrices."),
    
    prettySwitch(
      ns("saveTOMs"), 
      label= "Consensus topological overlap matrices saved and returned", 
      value = FALSE, 
      status = "warning", 
      popup = "Save and return the consensus topological overlap matrices for each block?"), 
    
    sliderInput(
      ns("maxPOutliers"), 
      label="Maimum percentile outlisers", 
      min=0, 
      max=1, 
      value = 0.1, 
      popup = "This is only used when the corType is Bicor. Specifies maximum percentile of the separate outliers data on either side of the median."),
             )),
    
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer WGCNA trait-based module", onclick="loading_modal_open(); stopWatch()"),
    htmlOutput(ns("close_loading_modal"))  # Close modal with JS
    )
  )
}
    
#' WGCNA Server Function
#'
#' @noRd 
mod_WGCNA_server <- function(input, output, session, con, upload_ui_1, input_overview_ui_1){
  ns <- session$ns
  
  WGCNA_module <- reactiveValues()
  
  # This function is used to make TOMType input valid
  decapitalize <- function(str){
    lo <- tolower(substring(str, 1, 1))
    return(paste(lo, substring(str, 2), sep = ""))
  }
  
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
        tags$script(HTML("element = document.getElementById('error_name_WGCNA_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-WGCNA_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_WGCNA_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-WGCNA_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  }) 
  
  observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    module_object <- try(MODifieRDB::wgcna_db(input_name = input$input_object, 
                                          group_of_interest = input$group_of_interest,
                                          minModuleSize = input$minModuleSize,
                                          deepSplit = input$deepSplit,
                                          pamRespectsDendro = input$pamRespectsDendro,
                                          mergeCutHeight = input$mergeCutHeight, 
                                          numericLabels = eval(parse(text = input$numericLabels)),
                                          pval_cutoff = input$pval_cutoff,
                                          corType = input$corType,
                                          maxBlockSize = input$maxBlockSize,
                                          TOMType = decapitalize(input$TOMType), # Used decapitalize function
                                          saveTOMs = input$saveTOMs,
                                          maxPOutliers = input$maxPOutliers, 
                                          module_name = input$module_name,
                                          con = con)
    )
    if (class(module_object) == "try-error"){
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), module_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      WGCNA_module$module_name <- module_name()
      updateTextInput(session, "module_name", value = character(0))
    }
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(WGCNA_module)
}
    
## To be copied in the UI
# mod_WGCNA_ui("WGCNA_ui_1")
    
## To be copied in the server
# callModule(mod_WGCNA_server, "WGCNA_ui_1")
 
