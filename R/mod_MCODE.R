#' MCODE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MCODE_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("input_choice")),
    uiOutput(ns("ppi_choice")),
    tags$div(id = "error_name_MCODE_js",
    textInput(ns("module_name"), "Module object name", popup = "Object that is produced by the disease module inference methods.", placeholder = "Module name")),
    uiOutput(ns("error_name_descrip")),
    uiOutput(ns("error_name_js")),
    
    tags$a(class="collapsible", "Advanced settings", class = "btn btn-primary btn-block", "data-toggle" = 'collapse', "data-target" = '#advanced_mod', "href"='#advanced_mod',"aria-expanded" = 'false', tags$div(class= "expand_caret caret")),
    tags$div(id = "advanced_mod", class = "collapse",
             tags$div(
    radioButtons(
        ns("hierarchy"),
        label = "Hierarchy",
        choices= c(1, 2, 3),
        selected = 1,
        inline = T,
        popup = "Indicates how many hierarchies are included in the network."
    ),
    sliderInput(
      ns("vwp"),
      label = "Vertex weight percentage",
      min = 0.0,
      max = 1.0,
      value = 0.5,
      step = 0.01,
      round = T,
      ticks = T,
      popup = "Threshold for the inclusion of vertices, as a percentage of the vertex with the maximum weight."
    ),
    sliderInput(
      ns("fdt"),
      label = "Clust density cutoff",
      min = 0.0,
      max = 1.0,
      value = 0.5,
      step = 0.01,
      round = T,
      ticks = T,
      popup = "Threshold for cluster density cutoff."
    ),
    sliderInput(
      ns("deg_cutoff"),
      label = "P-value cutoff",
      min = 0.0,
      max = 1.0,
      value = 0.05,
      step = 0.01,
      round = T,
      ticks = T,
      popup = "P-value cutoff for differentially expressed genes."
    ),
    uiOutput(ns("error_p_value")),
    sliderInput(
      ns("module_cutoff"),
      label = "Minimal score for a module to be returned",
      min = 0.0,
      max = 1.0,
      value = 0.5,
      step = 0.01,
      round = T,
      ticks = T,
      popup = "Threshold for modules to be returned."
    ),
    prettySwitch(
      ns("haircut"),
      label = "Haircut",
      value = FALSE,
      status = "warning",
      popup = "Remove singly-connected nodes from clusters."
    ),
    prettySwitch(
      ns("fluff"),
      label = "Fluff",
      value = FALSE,
      status = "warning",
      popup = "Expand cluster shell outwards by one neighbour shell."
    ),
    prettySwitch(
      ns("loops"),
      label = "Loops",
      value = FALSE,
      status = "warning",
      popup = "Include self-loops."
    ),
             )),
    
    tags$div(style = "text-align:center",
    actionButton(ns("load_input"), "Infer MCODE module", onclick="loading_modal_open(); stopWatch();"),
    htmlOutput(ns("close_loading_modal"))  # Close modal with JS
    )
  )
}
    
#' MCODE Server Function
#'
#' @noRd 
mod_MCODE_server <- function(input, output, session, con, upload_ui_1, input_overview_ui_1, ppi_networks_ui_1){
  ns <- session$ns
  
  MCODE_module <- reactiveValues()
  
  output$input_choice <- renderUI({
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    selectInput(ns("input_object"), label = "Input object", choices = input_objects,popup = "The input used for analyzation.")
  })
  
  observeEvent(c(upload_ui_1$input_name, input_overview_ui_1$delete$delete), {
    input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
    updateSelectInput(session, "input_object", choices = input_objects)
  })
  
  output$ppi_choice <- renderUI({
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    selectInput(ns("ppi_object"), label = "PPI network", choices = ppi_networks, popup = "Protein-Protein interaction network to overlay the differentially expressed genes on.")
  })
  
  observeEvent(ppi_networks_ui_1$upload_ppi$upload_ppi, {
    ppi_networks <- unlist(MODifieRDB::get_available_networks(con))
    updateSelectInput(session, "ppi_object", choices = ppi_networks)
  })
  
  module_name <- reactive({
    input$module_name
  })
  
  observe({
    if (any(MODifieRDB::get_available_module_objects(con)$module_name == module_name())){
      output$error_name_js <- renderUI({
        tags$script(HTML("element = document.getElementById('error_name_MCODE_js');
                       element.classList.add('has-error');
                       document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MCODE_ui_1-load_input').disabled = true;"))
      })
      output$error_name_descrip <- renderUI({
        tags$p(class = "text-danger", tags$b("Error:"), "This name has been taken. Please try again!",
               style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
      })
    } else {
      output$error_name_js <- renderUI({
        tags$script(HTML("document.getElementById('error_name_MCODE_js').classList.remove('has-error');
                         document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description1_ui_1-MCODE_ui_1-load_input').disabled = false;"))
      })
      output$error_name_descrip <- NULL
    }
  })
            
  
  observeEvent(input$load_input, {
    id <- showNotification("Infering method", duration = NULL, closeButton = FALSE, type = "warning")
    on.exit(removeNotification(id), add = TRUE)
    output$error_p_value <- NULL 
    output$error_name_descrip <- NULL
    module_object <- try(MODifieRDB::mcode_db(input_name = input$input_object, 
                                          ppi_name = input$ppi_object, 
                                          hierarchy = as.numeric(input$hierarchy),
                                          vwp = input$vwp,
                                          haircut = input$haircut,
                                          fdt = input$fdt,
                                          loops = input$loops,
                                          module_cutoff = input$module_cutoff,
                                          deg_cutoff = input$deg_cutoff,
                                          module_name = input$module_name,
                                          con = con)
                         )
    if (class(module_object) == "try-error"){
      if (grepl("Name", module_object)) {
        output$error_name_descrip <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), module_object,
                 style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
        })
      } else {
        output$error_p_value <- renderUI({
          tags$p(class = "text-danger", tags$b("Error:"), module_object,
                 style = "-webkit-animation: fadein 0.5s; -moz-animation: fadein 0.5s; -ms-animation: fadein 0.5s;-o-animation: fadein 0.5s; animation: fadein 0.5s;")
        })
      }
    } else {
      MCODE_module$module_name <- module_name()
      updateTextInput(session, "module_name", value = character(0))
    }
    # Close loading modal
    output$close_loading_modal <- renderUI({
      tags$script("loading_modal_close(); reset();")
    })
  })
  return(MCODE_module)
}
    
## To be copied in the UI
# mod_MCODE_ui("MCODE_ui_1")
    
## To be copied in the server
# callModule(mod_MCODE_server, "MCODE_ui_1")
 
