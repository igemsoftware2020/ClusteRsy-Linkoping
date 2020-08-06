#' main_page_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_main_page_v2_ui <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(id = ns("navbar"), title = actionLink(ns("title"), tags$i(class = "fa fa-home", `aria-hidden`="true"), type = "heading"),
               position = "fixed-top",
               collapsible = TRUE, selected = " ",
               tabPanel("Tools", value = "tools", mod_Columns_ui(ns("Columns_ui_1"))),
               tabPanel("Visualization", mod_visual_ui(ns("visual_ui_1"))),
                 tabPanel("Database",
                            tabsetPanel(
                            tabPanel("Input objects", value = "input_objects", mod_input_overview_ui(ns("input_overview_ui_1"))),
                            tabPanel("Module objects", value = "module_objects", mod_module_overview_ui(ns("module_overview_ui_1"))),
                            tabPanel("Enrichment objects", value = "enrichment_objects", mod_enrichment_overview_ui(ns("enrichment_overview_ui_1"))),
                            tabPanel("PPI networks", value = "ppi_networks", mod_ppi_networks_ui(ns("ppi_networks_ui_1")))
                 )),
               tabPanel("User guide", value = "user_guide", mod_user_guide_ui(ns("user_guide_ui_1"))),
               tabPanel(" ", mod_welcoming_page_ui(ns("welcoming_page_ui_1")))
                #Hide this with Javascirpt
    ),
    tags$script(HTML("element = document.getElementsByTagName('a')[5]; element.style.display = 'none'")), #Change the number according to the tabPanel order
    htmlOutput(ns("loaded"))
  )
}

#' main_page_v2 Server Function
#'
#' @noRd 
mod_main_page_v2_server <- function(input, output, session, con, app_servr){
  ns <- session$ns
  
  main_page_v2_module <- reactiveValues()
  module_overview_ui_1 <- reactiveValues()
  input_overview_ui_1 <- reactiveValues()
  ppi_networks_ui_1 <- reactiveValues()
  
  Columns_ui_1 <- callModule(mod_Columns_server, "Columns_ui_1", con = con, module_overview_ui_1, input_overview_ui_1, ppi_networks_ui_1)
  module_overview_ui_1$delete <- callModule(mod_module_overview_server, "module_overview_ui_1", con = con, Columns_ui_1)
  input_overview_ui_1$delete <- callModule(mod_input_overview_server, "input_overview_ui_1", con = con, Columns_ui_1)
  ppi_networks_ui_1$upload_ppi <- callModule(mod_ppi_networks_server, "ppi_networks_ui_1", con = con)
  callModule(mod_user_guide_server, "user_guide_ui_1")
  
  observeEvent(input$title, {
    updateNavbarPage(session, "navbar", " ")
  })
  
  #Front page button calls
  observeEvent(app_servr$user_guide_btn, {
    updateNavbarPage(session, "navbar", "user_guide")
  })
  
  observeEvent(app_servr$blob_button, {
    updateNavbarPage(session, "navbar", "tools")
  })
  
  observeEvent(app_servr$tutorial_btn, {
    updateNavbarPage(session, "navbar", "tools")
  })
  
 
  
  observeEvent(app_servr$loaded, {
    output$loaded <- renderUI({
      tags$script("$('body').toggleClass('loaded');")
    })
  })
  
  # Reactive values to record the buttons from all enrich methods
  enrichDGN = reactiveVal("0")
  enrichDO = reactiveVal("0")
  enrichGO = reactiveVal("0")
  enrichKEGG = reactiveVal("0")
  enrichMKEGG = reactiveVal("0")
  enrichNCG = reactiveVal("0")
  groupGO = reactiveVal("0")
  gseDGN = reactiveVal("0")
  gseNCG = reactiveVal("0")
  gseDO = reactiveVal("0")
  gseGO = reactiveVal("0")
  gseMKEGG = reactiveVal("0")
  gseKEGG = reactiveVal("0")

  observeEvent(Columns_ui_1$enrich, {
    main_page_v2_module$enrich <- Columns_ui_1$enrich 
    if (Columns_ui_1$enrich[2] == "enrichDGN"){
      if (Columns_ui_1$enrich[1] != enrichDGN()){
        enrichDGN(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "enrichDO"){
      if (Columns_ui_1$enrich[1] != enrichDO()){
        enrichDO(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "enrichGO"){
      if (Columns_ui_1$enrich[1] != enrichGO()){
        enrichGO(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "enrichKEGG"){
      if (Columns_ui_1$enrich[1] != enrichKEGG()){
        enrichKEGG(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "enrichMKEGG"){
      if (Columns_ui_1$enrich[1] != enrichMKEGG()){
        enrichMKEGG(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    
    if (Columns_ui_1$enrich[2] == "enrichNCG"){
      if (Columns_ui_1$enrich[1] != enrichNCG()){
        enrichNCG(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "groupGO"){
      if (Columns_ui_1$enrich[1] != groupGO()){
        groupGO(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "gseDGN"){
      if (Columns_ui_1$enrich[1] != gseDGN()){
        gseDGN(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "gseNCG"){
      if (Columns_ui_1$enrich[1] != gseNCG()){
        gseNCG(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "gseDO"){
      if (Columns_ui_1$enrich[1] != gseDO()){
        gseDO(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "gseGO"){
      if (Columns_ui_1$enrich[1] != gseGO()){
        gseGO(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "gseMKEGG"){
      if (Columns_ui_1$enrich[1] != gseMKEGG()){
        gseMKEGG(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
    if (Columns_ui_1$enrich[2] == "gseKEGG"){
      if (Columns_ui_1$enrich[1] != gseKEGG()){
        gseKEGG(Columns_ui_1$enrich[1])
        updateNavbarPage(session, "navbar", "Visualization")
      }
    }
  })
  
  callModule(mod_welcoming_page_server, "welcoming_page_ui_1")
  callModule(mod_visual_server, "visual_ui_1", con = con, main_page_v2_module)
  callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1", con = con, main_page_v2_module)
  callModule(mod_ppi_networks_server, "ppi_networks_ui_1", con = con)
}

## To be copied in the UI
# mod_main_page_v2_ui("main_page_v2_ui_1")

## To be copied in the server
# callModule(mod_main_page_v2_server, "main_page_v2_ui_1")
