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
               tabPanel("Tools", mod_Columns_ui(ns("Columns_ui_1"))),
               tabPanel("Visualization", mod_visual_ui(ns("visual_ui_1"))),
                tabPanel("Database",
                          tabsetPanel(id = ns("tabs"),
                                      type = "tabs",
                                      tabPanel("Input objects", mod_input_overview_ui(ns("input_overview_ui_1"))),
                                      tabPanel("Module objects", mod_module_overview_ui(ns("module_overview_ui_1"))),
                                      tabPanel("Enrichment objects", mod_enrichment_overview_ui(ns("enrichment_overview_ui_1"))),
                                      tabPanel("PPI networks", mod_ppi_networks_ui(ns("ppi_networks_ui_1"))))),
               tabPanel("User guide", mod_user_guide_ui(ns("user_guide_ui_1"))),
               tabPanel(" ", mod_welcoming_page_ui(ns("welcoming_page_ui_1")))
                #Hide this with Javascirpt
    ),
    tags$script(HTML("element = document.getElementsByTagName('a')[5]; element.style.display = 'none'")), #Change the number according to the tabPanel order
    htmlOutput(ns("loaded")),
    htmlOutput(ns("hidehints")),
    htmlOutput(ns("hidehints1"))
  )
}

#' main_page_v2 Server Function
#'
#' @noRd 
mod_main_page_v2_server <- function(input, output, session, con, app_servr){
  ns <- session$ns
  
  main_page_v2_module <- reactiveValues()
  enrichment_overview_ui_1 <- reactiveValues()
  module_overview_ui_1 <- reactiveValues()
  input_overview_ui_1 <- reactiveValues()
  ppi_networks_ui_1 <- reactiveValues()
  
  Columns_ui_1 <- callModule(mod_Columns_server, "Columns_ui_1", con = con, module_overview_ui_1, input_overview_ui_1, ppi_networks_ui_1)
  enrichment_overview_ui_1$value <- callModule(mod_enrichment_overview_server, "enrichment_overview_ui_1", con = con, main_page_v2_module, app_servr)
  module_overview_ui_1$value <- callModule(mod_module_overview_server, "module_overview_ui_1", con = con, Columns_ui_1, app_servr)
  input_overview_ui_1$value <- callModule(mod_input_overview_server, "input_overview_ui_1", con = con, Columns_ui_1, app_servr)
  ppi_networks_ui_1$upload_ppi <- callModule(mod_ppi_networks_server, "ppi_networks_ui_1", con = con)
  callModule(mod_user_guide_server, "user_guide_ui_1")
  
  observeEvent(input$title, {
    updateNavbarPage(session, "navbar", " ")
  })
  
  #Front page button calls
  observeEvent(app_servr$user_guide_btn, {
    updateNavbarPage(session, "navbar", "User guide")
  })
  
  observeEvent(app_servr$tool_button, {
    updateNavbarPage(session, "navbar", "Tools")
  })
  
  observeEvent(app_servr$tutorial_btn, {
    updateNavbarPage(session, "navbar", "Tools")
    showModal(modalDialog(
      top = "30",
      easyClose = TRUE,
      title = "Welcome to MODifieRWeb!",
      "This tutorial will show you how to use this webtool. You can find and manage the obects you create when using the tool under the Database tab and if you need more help you can find more detailed instructions in the User guide.",
      style = "color:black; text-align:center;",
      footer=tagList(
        tags$button("Close", class="btn btn-default", `data-dismiss`="modal"),
        tutorial_start_button <- actionButton(inputId = "tutorial_start_btn", label = "Start the tutorial!", style = "background-color: #2c3e50;border-color:#2c3e50;")
      )
    ))
  })
  
  
  observeEvent(app_servr$tutorial_start_btn, {
    removeModal()
  })
  observeEvent(app_servr$tutorial_start_btn, {
    rintrojs::introjs(session)
  })
  
  # Hide hints
  observeEvent(input$navbar, {
    if (input$navbar == "Tools"){
      rintrojs::hintjs(session)
      }
    else{
      output$hidehints <- renderUI({
        tags$script("introJs().hideHints();")
      })
    }
  })
  
  observeEvent(input$navbar, {
    if (input$navbar == "Database"){
      output$hidehints1 <- renderUI({})
      }
    else{
      output$hidehints1 <- renderUI({
        tags$script("Tipped.hide('#DT_tooltip');
                    Tipped.hide('#DT_tooltip1');
                    Tipped.hide('#DT_tooltip2')")
      })
    }
  })
  
  observeEvent(input$tabs, {
    output$hidehints1 <- renderUI({
      if (input$tabs == "Input objects") {
        tags$script("Tipped.hide('#DT_tooltip1');
                    Tipped.hide('#DT_tooltip2');")
      }
      if (input$tabs == "Module objects") {
        tags$script("Tipped.hide('#DT_tooltip');
                    Tipped.hide('#DT_tooltip2');")
      }
      if (input$tabs == "Enrichment objects") {
        tags$script("Tipped.hide('#DT_tooltip');
                    Tipped.hide('#DT_tooltip1');")
      }
      else {
        tags$script("Tipped.hide('#DT_tooltip');
                    Tipped.hide('#DT_tooltip1');
                    Tipped.hide('#DT_tooltip2');")
      }
    })
  })
  
  ##############################################
  
  observeEvent(app_servr$loaded, {
    output$loaded <- renderUI({
      tags$script("$('body').toggleClass('loaded');")
    })
  })
  
  # Information popups in data-tab
  observeEvent(app_servr$information_btn_ppi, {
    showModal(modalDialog(
      top = "30",
      easyClose = TRUE,
      title = "PPI Networks",
      "In the PPI Networks tab all the previously uploaded 
      PPI Networks can be found. The user can either use the default PPI Network 
      or upload a custom made for a specific analyse.",
      style = "color:black; text-align:center;",
      footer=tagList(
        tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
      )
    ))
  })
  observeEvent(app_servr$information_btn_input, {
    showModal(modalDialog(
      top = "30",
      easyClose = TRUE,
      title = "Input Objects",
      "In the Input Oblects tab all the previously made 
       Inputs can be found. The Inputs in the table below can be brought back in 
       column 3 in the Tools-tab for further analysis.",
      style = "color:black; text-align:center;",
      footer=tagList(
        tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
      )
    ))
  })
  observeEvent(app_servr$information_btn_module, {
    showModal(modalDialog(
      top = "30",
      easyClose = TRUE,
      title = "Module Objects",
      tags$p(
      "In the Module Objects tab all the previously made Modules 
       are saved and stored. You can inspect the module objects by double clicking the selected module."),
      tags$br(),
      tags$p(
      "You can download one or multiple objects. Note that if you like to upload an module object you have to store it as a list and naming the list and save it as a .Rds file before you can upload it to the database."),
      style = "color:black; text-align:center;",
      footer=tagList(
        tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
      )
    ))
  })
  observeEvent(app_servr$information_btn_enrichment, {
    showModal(modalDialog(
      top = "30",
      easyClose = TRUE,
      title = "Enrichment Objects",
      tags$p(
      "In the Enrichment Objects tab all the previously made 
       enrichments can be found. You can inspect the results by double clicking the selected enrichment object. 
      If you want to inspect a certain disease from one of the enrichment objects, please visit the Visualization tab and go the results table."),
      tags$br(),
      tags$p("You can download multiple or single enrichment objects but as of now we only offer single upload."),
      tags$br(),
      tags$p("If you like to upload an enrichment object created elsewhere please store it as a list and save it as a .Rds file before you can upload it to the database"),
      style = "color:black; text-align:center;",
      footer=tagList(
        tags$button("Close", class="btn btn-default", `data-dismiss`="modal")
      )
    ))
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
  callModule(mod_visual_server, "visual_ui_1", con = con, main_page_v2_module, enrichment_overview_ui_1)
  callModule(mod_ppi_networks_server, "ppi_networks_ui_1", con = con)
}

## To be copied in the UI
# mod_main_page_v2_ui("main_page_v2_ui_1")

## To be copied in the server
# callModule(mod_main_page_v2_server, "main_page_v2_ui_1")
