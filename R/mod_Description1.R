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
    tags$div(selectInput(ns("Inference_method"), label = "Inference method", popup = "Network-based approaches to create disease modules",
                         choices = c("Clique Sum", "Correlation Clique", "DIAMoND", "DiffCoEx", "MCODE", "MODA", "Module Discoverer", "WGCNA"))
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
    
    # Method's description
    descrip <- function(method){
      if (method == "Clique Sum"){
        description = "Method to determine if a clique is significantly enriched with differentially expressed genes."
        return(description)
      }
      if (method == "WGCNA"){
        description = "WGCNA trait-based is an implementation of WGCNA to correlate coexpression modules to disease."
        return(description)
      }
      if (method == "DIAMoND"){
        description = "A seed gene based algorithm to identify disease modules from differentially expressed genes."
        return(description)
      }
      if (method == "DiffCoEx"){
        description = "A method for identifying correlation pattern changes. DiffCoEx builds on WGCNA-framework for coexpression analysis."
        return(description)
      }
      if (method == "MCODE"){
        description = "An algorithm to identify disease modules from differentially expressed."
        return(description)
      }
      if (method == "MODA"){
        description = "An algorithm to understand gene expression patterns under different conditions."
        return(description)
      }
      if (method == "Module Discoverer"){
        description = "A clique based algorithm to produce disease modules from differentially expressed genes."
        return(description)
      }
      if (method == "Correlation Clique"){
        description = "A clique based method to find a disease module from correlated gene expression."
        return(description)
      }
    }
    
    # Method's link
    hyperlink <- function(method){
      if (method == "Clique Sum"){
        link = "https://doi.org/10.1186/gm534"
        return(link)
      }
      if (method == "WGCNA"){
        link = "https://doi.org/10.1186/1471-2105-9-559"
        return(link)
      }
      if (method == "DIAMoND"){
        link = "https://doi.org/10.1371/journal.pcbi.1004120"
        return(link)
      }
      if (method == "DiffCoEx"){
        link = "https://doi.org/10.1186/1471-2105-11-497"
        return(link)
      }
      if (method == "MCODE"){
        link = "https://doi.org/10.1186/1471-2105-4-2"
        return(link)
      }
      if (method == "MODA"){
        link = "https://doi.org/10.1101/053496"
        return(link)
      }
      if (method == "Module Discoverer"){
        link = "https://doi.org/10.1038/s41598-017-18370-2"
        return(link)
      }
      if (method == "Correlation Clique"){
        link = "https://www.google.com/"
        return(link)
      }
    }
    
    
    if (is.null(method)){
      return(NULL)
    }
    
    #Popup Modal
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
                                                                  tags$p(descrip(method = method), style = "color: black")),
                                                        tags$div(`class` = "modal-footer",
                                                                  tags$button("Close", class = "btn btn-default", `data-dismiss` = "modal"),
                                                                  tags$a("Learn more", class= "btn btn-default", href=hyperlink(method = method), style = "background-color: #2c3e50; border-color: #2c3e50"))))))
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
      })
      callModule(mod_CliqueSum_server, "CliqueSum_ui_1", con = con) 
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

