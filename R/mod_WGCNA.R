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
    uiOutput(ns("ppi_choice")),
    tags$div(id = "error_name_WGCNA_js",
    textInput(ns("module_name"), "Module object name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    radioButtons(ns("group_of_interest"), label= "Select group of interest", choiceNames=c("Group 1", "Group 2"), choiceValues = c(1, 2)),
    sliderInput(ns("minModuleSize"), label= "Minimum module size", min=1, max=100, value=30),
    sliderInput(ns("deepSplit"), label= "Integer value between 0 and 4", min=0, max=4, value=2),
    shinyWidgets::prettySwitch(ns("pamRespectsDendro"), label= "Select pamStage", value=TRUE, status = "warning"),
    sliderInput(ns("mergeCutHeight"), label = "Dendrogram cut height for module merging", min=0, max=1, value=0.1), 
    radioButtons(ns("numericLabels"), label="Sholud the returned modules be labeled by color or numbers?", choiceNames=c("Color", "Numbers"), choiceValues = c(F,T)),
    sliderInput(ns("pval_cutoff"), label="P-value cutoff for significant co-expression modules", min=0, max=1, value=0.05),
    radioButtons(ns("corType"), label="Specify correlation", choiceNames = c("Pearson", "Bicor"), choiceValues = c("pearson", "bicor")),
    sliderInput(ns("maxBlockSize"), label="Max block size", min=0, max=10000, value=5000),
    selectInput(ns("TOMType"), label="Select TOMType", c("None", "Unsigned", "Signed", "Signedtoup Nowick", "Unsigned 2", "Signed 2", "Signed Nowick 2")),
    shinyWidgets::prettySwitch(ns("saveTOMs"), label= "Consensus topological overlap matrices saved and returned", value = TRUE, status = "warning"), 
    sliderInput(ns("maxPOutliers"), label="Maimum percentile outlisers", min=0, max=1, value = 0.1),
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer WGCNA trait-based module")
    )
  )
}
    
#' WGCNA Server Function
#'
#' @noRd 
mod_WGCNA_server <- function(input, output, session, con){
  ns <- session$ns
 
  decapitalize <- function(str){
    lo <- tolower(substring(str, 1, 1))
    return(paste(lo, substring(str, 2), sep = ""))
  }
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects)
  })
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks)
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
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!")
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
    module_object <- MODifieRDB::wgcna_db(input_name = input$input_object, 
                                          group_of_interest = input$group_of_interest,
                                          minModuleSize = input$minModuleSize,
                                          deepSplit = input$deepSplit,
                                          pamRespectsDendro = input$pamRespectsDendro,
                                          mergeCutHeight = input$mergeCutHeight, 
                                          numericLabels = eval(parse(text = input$numericLabels)),
                                          pval_cutoff = input$pval_cutoff,
                                          corType = input$corType,
                                          maxBlockSize = input$maxBlockSize,
                                          TOMType = decapitalize(input$TOMType), 
                                          saveTOMs = input$saveTOMs,
                                          maxPOutliers = input$maxPOutliers, 
                                          module_name = input$module_name,
                                          con = con)
  })
  
  
  
}
    
## To be copied in the UI
# mod_WGCNA_ui("WGCNA_ui_1")
    
## To be copied in the server
# callModule(mod_WGCNA_server, "WGCNA_ui_1")
 
