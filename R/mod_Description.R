#' Description UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Description_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(id = "Inference_method",
    selectInput(ns("Inference_method"), label = "Inference method", choices = c("Clique Sum", "WGCNA", "DIAMoND", "DiffCoEx", "MCODE", "MODA", "Module Discoverer", "Correlation Clique"))
    ),
    htmlOutput(ns("description")),
    tags$br(),
    tags$div(style = "text-align: right",
    actionButton(ns("load_method"), "Select Inference Method", onclick = "removeDescription()")
    ),
    htmlOutput(ns("method")),
    tags$script(HTML("function removeDescription(){
       var description = document.getElementById('method');
       description.remove();}
        
       document.getElementById('Inference_method').addEventListener('click', 
                              function(){document.getElementById('main_page_v2_ui_1-Columns_ui_1-Description_ui_1-method').innerHTML = '';})
                     
      function showDescription(){ 
                  var str = document.getElementById('method').innerHTML;
                  
                  if (str.indexOf('Clique Sum') >= 0){
                    var res = str.replace ('Click me to learn more about Clique Sum', 'Clique Sum\\'s description');
                  }
                  
                  if (str.indexOf('WGCNA') >= 0){
                    var res = str.replace ('Click me to learn more about WGCNA', 'WGCNA\\'s description');
                  }
                  
                  if (str.indexOf('DIAMoND') >= 0){
                    var res = str.replace ('Click me to learn more about DIAMoND', 'DIAMoND\\\'s description');
                  }
                  
                  if (str.indexOf('DiffCoEx') >= 0){
                    var res = str.replace ('Click me to learn more about DiffCoEx', 'DiffCoEx\\'s description');
                  }
                  
                  if (str.indexOf('MCODE') >= 0){
                    var res = str.replace ('Click me to learn more about MCODE', 'A wonderful serenity has taken possession of my entire soul, like these sweet mornings of spring which I enjoy with my whole heart. I am alone, and feel the charm of existence in this spot, which was created for the bliss of souls like mine. I am so happy, my dear friend, so absorbed in the exquisite sense of mere tranquil existence, that I neglect my talents. I should be incapable of drawing a single stroke at the present moment; and yet I feel that I never was a greater artist than now. When, while the lovely valley teems with vapour around me, and the meridian sun strikes the upper surface of the impenetrable foliage of my trees, and but a few stray gleams steal into the inner sanctuary, I throw myself down among the tall grass by the trickling stream; and, as I lie close to the earth, a thousand unknown plants are noticed by me: when I hear the buzz of the little world among the stalks, and grow familiar with the countless indescribable forms of the insects and flies, then I feel the presence of the Almighty, who formed us in his own image, and the breath of that universal love which bears and sustains us, as it floats around us in an eternity of bliss; and then, my friend, when darkness overspreads my eyes, and heaven and earth seem to dwell in my soul and absorb its power, like the form of a beloved mistress, then I often think with longing, Oh, would I could describe these conceptions, could impress upon paper all that is living so full and warm within me, that it might be the mirror of my soul, as my soul is the mirror of the infinite God! O my friend -- but it is too much for my strength -- I sink under the weight of the splendour of these visions!A wonderful serenity has taken possession of my entire soul, like these sweet mornings of spring which I enjoy with my whole heart. I am alone, and feel the charm of existence in this spot, which was created for the bliss of souls like mine. I am so happy, my dear friend, so absorbed in the exquisite sense of mere tranquil existence, that I neglect my talents. I should be incapable of drawing a single stroke at the present moment; and yet I feel that I never was a greater artist than now. When, while the lovely valley teems with vapour around me, and the meridian sun strikes the upper surface of the impenetrable foliage of my trees, and but a few stray gleams steal into the inner sanctuary, I throw myself down among the tall grass by the trickling stream; and, as I lie close to the earth, a thousand unknown plants are noticed by me: when I hear the buzz of the little world among the stalks, and grow familiar with the countless indescribable forms of the insects and flies, then I feel the presence of the Almighty, who formed us in his own image, and the breath of that universal love which bears and sustains us, as it floats around us in an eternity of bliss; and then, my friend, when darkness overspreads my eyes, and heaven and earth seem to dwell in my soul and absorb its power, like the form of a beloved mistress, then I often think with longing, Oh, would I could describe these conceptions, could impress upon paper all that is living so full and warm within me, that it might be the' );
                  }
                  
                  if (str.indexOf('MODA') >= 0){
                    var res = str.replace ('Click me to learn more about MODA', 'MODA\\'s description');
                  }
                  
                  if (str.indexOf('Module Discoverer') >= 0){
                    var res = str.replace ('Click me to learn more about Module Discoverer', 'Module Discoverer\\'s description');
                  }
                  
                  if (str.indexOf('Correlation Clique') >= 0){
                    var res = str.replace ('Click me to learn more about Correlation Clique', 'Correlation Clique\\'s description');
                  }
                  
                  
                  document.getElementById('method').innerHTML = res;}"
    ))
  )
}
    
#' Description Server Function
#'
#' @noRd 
mod_Description_server <- function(input, output, session){
  ns <- session$ns
  
  description <- reactiveValues()
  
  upload_description <- reactive({
    req((input$Inference_method))
    method <- input$Inference_method
    if (is.null(method)){
      return(NULL)
    }
    
    if (method == "Clique Sum"){
      description <- tags$button("Click me to learn more about Clique Sum",
                            id = "method",
                            class = 'link',
                            onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "WGCNA"){
      description <- tags$button("Click me to learn more about WGCNA",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "DIAMoND"){
      description <- tags$button("Click me to learn more about DIAMoND",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "DiffCoEx"){
      description <- tags$button("Click me to learn more about DiffCoEx",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "MCODE"){
      description <- tags$button("Click me to learn more about MCODE",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "MODA"){
      description <- tags$button("Click me to learn more about MODA",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "Module Discoverer"){
      description <- tags$button("Click me to learn more about Module Discoverer",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
    if (method == "Correlation Clique"){
      description <- tags$button("Click me to learn more about Correlation Clique",
                                 id = "method",
                                 class = 'link',
                                 onclick = 'showDescription()')
      return(description)
    }
    
  }
  )
 
  output$description <- renderUI({
    tags$div(style = "text-align: center;",
    upload_description()
    )
  })
  
  observeEvent(input$load_method, {
    method <- input$Inference_method
    
    if(method == "Clique Sum"){
    output$method <- renderUI({mod_Cliquesum_ui(ns("Cliquesum_ui_1"))
    })
    }
    
    if(method == "WGCNA"){
      output$method <- renderUI({mod_WGCNA_ui(ns("WGCNA_ui_1"))
      })
    }
    
    if(method == "DIAMoND"){
      output$method <- renderUI({mod_DIAMoND_ui(ns("DIAMoND_ui_1"))
      })
    }
    
    if(method == "DiffCoEx"){
      output$method <- renderUI({mod_DiffCoEx_ui(ns("DiffCoEx_ui_1"))
      })
    }
    
    if(method == "MCODE"){
      output$method <- renderUI({mod_MCODE_ui(ns("MCODE_ui_1"))
      })
    }
    
    if(method == "MODA"){
      output$method <- renderUI({mod_MODA_ui(ns("MODA_ui_1"))
      })
    }
    
    if(method == "Module Discoverer"){
      output$method <- renderUI({mod_Modulediscovere_ui(ns("Modulediscoverer_ui_1"))
      })
    }
    
    if(method == "Correlation Clique"){
      output$method <- renderUI({mod_Correlationclique_ui(ns("Correlationclique_ui_1"))
      })
    }
  }
  )
  
  callModule(mod_MCODE_server, "MCODE_ui_1")
  callModule(mod_MCODE_server, "Cliquesum_ui_1")
  callModule(mod_MCODE_server, "WGCNA_ui_1")
  callModule(mod_MCODE_server, "DIAMoND_ui_1")
  callModule(mod_MCODE_server, "DiffCoEx_ui_1")
  callModule(mod_MCODE_server, "MCODE_ui_1")
  callModule(mod_MCODE_server, "MODA_ui_1")
  callModule(mod_MCODE_server, "Modulediscoverer_ui_1")
  callModule(mod_MCODE_server, "MCODE_ui_1")
  callModule(mod_MCODE_server, "Correlationclique_ui_1")
}

  
    
## To be copied in the UI
# mod_Description_ui("Description_ui_1")
    
## To be copied in the server
# callModule(mod_Description_server, "Description_ui_1")
 
