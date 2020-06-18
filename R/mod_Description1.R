#' Description UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Description1_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = "Inference_method",
             selectInput(ns("Inference_method"), label = "Inference method", choices = c("Clique Sum", "WGCNA", "DIAMoND", "DiffCoEx", "MCODE", "MODA", "Module Discoverer", "Correlation Clique"),
                         tooltip = F)
    ),
    htmlOutput(ns("description")),
    tags$br(),
    htmlOutput(ns("method"))
  )
}

#' Description Server Function
#'
#' @noRd 
mod_Description1_server <- function(input, output, session, con){
  ns <- session$ns
  
  description <- reactiveValues()
  
  upload_description <- reactive({
    req((input$Inference_method))
    method <- input$Inference_method
    if (is.null(method)){
      return(NULL)
    }
    
    if (method %in% c("Clique Sum", "WGCNA", "DIAMoND", "DiffCoEx", "MCODE", "MODA", "Module Discoverer", "Correlation Clique")){
      description <- tagList(tags$button(paste("Click me to learn more about ", method),
                                 id = "method",
                                 class = "link",
                                 type = "button",
                                 `data-toggle` = "modal",
                                 `data-target` = "#descrip"),
                             tags$div(`class` = "modal fade", `id` = "descrip", `role` = "dialog", `tabindex` = "-1", `aria-hidden` = "true", `style` = "display:none;",
                                      tags$div(`class` = "modal-dialog",
                                               tags$div(`class` = "modal-content",
                                                        tags$div(`class` = "modal-header",
                                                                tags$button("×", type = "button", class = "close", `data-dismiss` = "modal", `aria-hidden` = "true"),
                                                                tags$h4(method ,class = "modal-title", style = "color: black")),
                                                        tags$div(`class` = "modal-body",
                                                                  tags$p(paste(method,"'s description"), style = "color: black")),
                                                        tags$div(`class` = "modal-footer",
                                                                  tags$button("Close", class = "btn btn-default", `data-dismiss` = "modal"),
                                                                  tags$a("Learn more", class= "btn btn-default", href="https://bootswatch.com/flatly/", style = "background-color: #2c3e50; border-color: #2c3e50"))))))
      return(description)
    }
  }
  )
  
  output$description <- renderUI({
    tags$div(style = "text-align: center;",
             upload_description()
    )
  })
  
  observeEvent(input$Inference_method, {
    method <- input$Inference_method
    
    if(method == "Clique Sum"){
      output$method <- renderUI({mod_CliqueSum_ui(ns("CliqueSum_ui_1"))
      callModule(mod_CliqueSum_server, "CliqueSum_ui_1")  
      })
    }
    
    if(method == "WGCNA"){
      output$method <- renderUI({mod_WGCNA_ui(ns("WGCNA_ui_1"))
      })
      callModule(mod_WGCNA_server, "WGCNA_ui_1", con = con)
    }
    
    if(method == "DIAMoND"){
      output$method <- renderUI({mod_DIAMoND_ui(ns("DIAMoND_ui_1"))
      })
      callModule(mod_DIAMoND_server, "DIAMoND_ui_1", con = con)
    }
    
    if(method == "DiffCoEx"){
      output$method <- renderUI({mod_DiffCoEx_ui(ns("DiffCoEx_ui_1"))
      })
      callModule(mod_DiffCoEx_server, "DiffCoEx_ui_1", con = con)
    }
    
    if(method == "MCODE"){
      output$method <- renderUI({mod_MCODE_ui(ns("MCODE_ui_1"))
      })
      callModule(mod_MCODE_server, "MCODE_ui_1", con = con)
    }
    
    if(method == "MODA"){
      output$method <- renderUI({mod_MODA_ui(ns("MODA_ui_1"))
      })
      callModule(mod_MODA_server, "MODA_ui_1", con = con)
    }
    
    if(method == "Module Discoverer"){
      output$method <- renderUI({mod_Modulediscoverer_ui(ns("Modulediscoverer_ui_1"))
      })
      callModule(mod_Modulediscoverer_server, "Modulediscoverer_ui_1", con = con)
    }
    
    if(method == "Correlation Clique"){
      output$method <- renderUI({mod_CClique_ui(ns("CClique_ui_1"))
      })
      callModule(mod_CClique_server, "CClique_ui_1", con = con)
    }
  }
  )
  
  
}



## To be copied in the UI
# mod_Description1_ui("Description1_ui_1")

## To be copied in the server
# callModule(mod_Description1_server, "Description1_ui_1")

