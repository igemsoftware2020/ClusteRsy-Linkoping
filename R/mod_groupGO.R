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
    uiOutput(
      ns("module_input")),
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
    
    tags$div( style = "text-align:center",
              actionButton(ns("load_input"), label = "Group", onclick="loading_modal_open(); stopWatch();"),
              htmlOutput(ns("close_loading_modal"))  # Close modal with JS 
    )
  )
}

#' GO Server Function 
#' 
#' @noRd
mod_groupGO_server <- function(input, output, session, con, Description1_ui_1){
  ns <- session$ns
  
  groupGO_module <- reactiveValues()
  x <- reactiveVal(1) # Reactive value to record if the input button is pressed
  
  
  output$module_input <- renderUI({
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    selectInput(ns("module_object"), label = "Module object", choices = module_objects, 
                popup = "The module used for gene set enrichment analysis.")
  })
  
  observeEvent(Description1_ui_1$module_name, {
    module_objects <- unlist(MODifieRDB::get_available_module_objects(con)$module_name)
    updateSelectInput(session, "module_object", choices = module_objects)
  })
  
  observeEvent(input$load_input, {
    id <- showNotification("Creating enrichment analysis object", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    
    module_genes <- get_module_genes(input$module_object, con = con)
    background_genes <- get_background_genes(input$module_object, con = con)
    
    group_object <- try(clusterProfiler::groupGO(gene = module_genes,
                                                 OrgDb = 'org.Hs.eg.db',
                                                 keyType = input$keytype, 
                                                 ont = input$ont, 
                                                 level = input$level, 
                                                 readable = input$readable
  ))
    
    if (class(group_object) == "try-error"){
      output$error <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), group_object)
      })
    } else {
      groupGO_module$enrich <- c(x(), "groupGO")  # Reactive value to record if the input button is pressed
      module_name <- input$module_object
      MODifieRDB::enrichment_object_to_db(group_object,
                                          module_name = module_name, 
                                          enrichment_method = "groupGO", 
                                          con = con)
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
                                              
                                                       
                                                       
                                                       
    
    
    