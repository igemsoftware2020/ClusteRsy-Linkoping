#' GO UI Function 
#' 
#' @description A shiny Module.
#' 
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd
#' 
#' @importFrom shiny NS tagList
mod_groupGO_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("module_input")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_enrich',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$br(),
    tags$div(id = "advanced_enrich", class = "collapse",
             tags$div(
    selectInput(
      ns("keytype"), 
      label = "Select the type of gene input", 
      choices = c(AnnotationDbi::keytypes(org.Hs.eg.db::org.Hs.eg.db)),
      popup = "Select the type of the input data"
    ),
    selectInput(
      ns("ont"),
      label = "Select subontologies", 
      choices= c("BP", "MF", "CC"),
      popup = "Either biological process (BP), cellular component (CC) or molecular function (MF)."
    ),
    sliderInput(
      ns("level"),
      label = "Select specific GO Level", 
      min = 1, 
      max = 5, 
      value = 2, 
      popup = "Select the level that should return the GO profile"
    ),
    prettySwitch(
      ns("readable"), 
      label = "Readable",
      value = FALSE, 
      status = "warning",
      popup = "Whether the gene IDs will mapping to gene symbols"
    ),
             )),
    
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Group", onclick="loading_modal_open(); stopWatch();"),
              htmlOutput(ns("close_loading_modal")),  # Close modal with JS 
              htmlOutput((ns("adv_settings")))
    )
  )
}

#' GO Server Function 
#' 
#' @noRd
mod_groupGO_server <- function(input, output, session, con, Description1_ui_1, module_overview_ui_1){
  ns <- session$ns
  
  groupGO_module <- reactiveValues()
  x <- reactiveVal(1) # Reactive value to record if the input button is pressed
  
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    tagList(
      selectInput(ns("module_object"), label = "Module object", choices = module_objects, popup = "The module used for gene set enrichment analysis."),
      tags$div(id = "error_name_groupGO_js",
              textInput(ns("enrichment_name"), "Module object name", popup = "Object that is produced by the enrichment methods.", placeholder = "Enrichment name")),
      uiOutput(ns("error_name_descrip")),
      uiOutput(ns("error_name_js")),
      uiOutput(ns("error"))
    )
  })
  
  observeEvent(c(Description1_ui_1$module_name, module_overview_ui_1$value$delete, module_overview_ui_1$value$upload), {
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    updateSelectInput(session, "module_object", choices = module_objects)
  })
  
  enrichment_name <- reactive({
    input$enrichment_name
  })
  
  # Check name
  observe({
    if (any(MODifieRDB::get_available_enrichment_objects(con)$enrichment_name == enrichment_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_groupGO_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-disease_analysis_ui_1-groupGO_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_groupGO_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-disease_analysis_ui_1-groupGO_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    output$error <- renderUI({})
    output$adv_settings <- renderUI({})
    
    module_genes <- try(get_module_genes(input$module_object, con = con))
    
    group_object <- try(clusterProfiler::groupGO(gene = module_genes,
                                                 OrgDb = 'org.Hs.eg.db',
                                                 keyType = input$keytype, 
                                                 ont = input$ont, 
                                                 level = input$level, 
                                                 readable = input$readable
  ))
    
    if (any(c(class(group_object), class(module_genes)) == "try-error")){
      output$adv_settings <- renderUI({
        tags$script("if ($('.collapsible.btn.btn-primary.btn-block').eq(1).attr('aria-expanded') === 'false') {
                            $('.collapsible.btn.btn-primary.btn-block').eq(1).click();
                    }")
      })
      output$error <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), group_object,
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      x(x() + 1)
      groupGO_module$enrich <- c(x(), "groupGO")  # Reactive value to record if the input button is pressed
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(group_object,
                                          module_name = module_name, 
                                          enrichment_method = "groupGO", 
                                          
                                          con = con)
      updateTextInput(session, "enrichment_name", value = character(0))
    }
    # Close loading modal
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(groupGO_module)
}

## To be copies in the UI 
# mod_groupGO_ui("groupGO_ui_1")

## To be copied in the server
# callModule(mod_groupGO_server, "groupGO_ui_1")
                                              
                                                       
                                                       
                                                       
    
    
    