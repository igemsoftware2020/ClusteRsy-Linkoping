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
    selectInput(ns("Inference_method"), label = "Inference method", choices = c("Clique Sum", "WGCNA", "DIAMoND", "DiffCoEx", "MCODE", "MODA", "Module Discoverer", "Correlation Clique")),
    ),
    htmlOutput(ns("description")),
    actionButton(ns("load_method"), "Select Inference Method", onclick = "removeDescription()"),
    htmlOutput(ns("method")),
    tags$script(HTML("function removeDescription(){
       var description = document.getElementById('method');
       description.remove();}
        
       document.getElementById('Inference_method').addEventListener('click', 
                              function(){document.getElementById('main_page_v2_ui_1-Description_ui_1-method').innerHTML = '';})"
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
      description <- tags$p("This is Clique Sum",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "WGCNA"){
      description <- tags$p("This is WGCNA",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "DIAMoND"){
      description <- tags$p("This is DIAMoND",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "DiffCoEx"){
      description <- tags$p("This is DiffCoEx",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "MCODE"){
      description <- tags$p("
Diam viverra sociis fusce in id. Maecenas tortor vulputate iaculis ligula massa fringilla tortor, non aliquet pharetra lobortis cras morbi malesuada. Congue natoque ut aliquam. Suscipit pretium rhoncus facilisi euismod suspendisse lectus ultrices lobortis sollicitudin suscipit viverra praesent est habitasse habitant adipiscing. Nostra A laoreet scelerisque cras senectus inceptos habitant, ornare augue luctus inceptos justo at iaculis, ac. Tempus vulputate risus. Facilisi orci. Malesuada tempus ac diam et conubia ultricies commodo litora tellus nascetur massa lectus. Senectus sociis eros natoque nibh interdum ultricies natoque aliquam Nam natoque, praesent cubilia vestibulum laoreet turpis aliquam non turpis tempus orci nam semper sem fames metus vulputate Habitasse vitae nunc Cubilia convallis volutpat ultricies amet hac, cras aptent penatibus diam mi fusce metus. In tempus praesent tellus hymenaeos sem taciti. Iaculis mollis pretium diam inceptos euismod gravida eu dolor. Ante Magna Praesent amet. Consectetuer praesent fringilla eleifend porttitor ultricies sociosqu fusce aliquam feugiat venenatis nibh Rhoncus ac vehicula nonummy platea duis feugiat litora nam. Integer hymenaeos dis lorem. Litora sociis nullam class maecenas. Curae; ut dolor diam. Nascetur sociis. Ac vivamus. Nunc porttitor convallis natoque placerat torquent orci ligula inceptos, eleifend. Habitasse interdum nonummy quam libero praesent pellentesque. Habitasse cras ligula magnis donec turpis ullamcorper aenean ipsum rhoncus felis purus. Auctor ridiculus egestas hymenaeos Ante ac. Vitae quis sapien convallis taciti nunc. Orci Et mattis sed sem tempus nam venenatis semper torquent, odio. Ultricies tellus diam pede mollis. Dui nascetur. Etiam dui per magna massa in tempor eu dolor pretium praesent quis in vestibulum, vel. Nam integer suscipit. Vehicula, imperdiet. Mauris, bibendum dictum mus, quisque orci, neque sollicitudin nibh cum accumsan sapien pulvinar facilisi ornare ipsum velit aliquam volutpat libero fusce nonummy. Rutrum. Porta nulla purus ullamcorper lorem venenatis nullam rutrum justo commodo habitant lacus sit amet laoreet dictum torquent mus curae; quisque. Lobortis habitant nullam.

Ante lacinia Scelerisque consectetuer gravida taciti hendrerit at viverra eu laoreet suscipit class mattis magnis auctor sapien iaculis gravida facilisis feugiat eget nunc placerat taciti. Purus volutpat nisl Morbi proin mattis ornare interdum et convallis gravida. Mauris montes sociis purus justo vulputate viverra convallis. Tristique eros. Porta leo orci. Tempor dis, primis nisi magnis duis. Turpis ultricies sodales aliquet facilisi. Vehicula magna etiam magna tortor tellus donec nec Fames commodo a nonummy class condimentum lacinia ultrices Feugiat sociis platea primis elit aliquet leo aliquet a scelerisque est, rhoncus condimentum sagittis tortor imperdiet cursus eget. Interdum. Tempor quisque lacus aliquet risus potenti sem cum scelerisque in ultricies sit dolor lacus sociis sit hymenaeos nisl pellentesque odio cras ante. Nulla in pellentesque tincidunt, risus tortor dictumst nonummy etiam dictum condimentum metus Litora vehicula. Consectetuer libero congue quis ad ante est habitant est. Orci dignissim laoreet vehicula. Ridiculus torquent dolor odio augue viverra, bibendum eu magnis laoreet cras aliquam Amet congue ultricies, interdum congue montes et molestie dolor diam gravida semper penatibus, venenatis fusce id hac velit suspendisse tristique lacus montes lacus consectetuer hymenaeos. Potenti aptent nunc mollis faucibus mus mattis Risus hac donec nec integer tempus lectus facilisi dignissim ac sociosqu orci nonummy sapien nunc etiam fusce ullamcorper a lobortis cursus euismod mattis dolor magna conubia dictum purus arcu. Ullamcorper pellentesque. Eros senectus odio fermentum justo fringilla mollis vivamus praesent porta urna hymenaeos nonummy, felis dapibus placerat primis magna cubilia nascetur elementum quis sodales ultrices a pulvinar mattis massa pretium dictum Ridiculus amet, magnis ut pellentesque. Suspendisse per turpis habitasse laoreet eu sociosqu fringilla curae; hendrerit varius sapien libero consectetuer sagittis scelerisque per phasellus hymenaeos facilisis diam id mi etiam venenatis etiam Lacus diam parturient imperdiet. Suscipit varius faucibus primis natoque felis neque elementum blandit luctus cubilia posuere pede, netus dui mi elit ridiculus feugiat tempor. Est nonummy vivamus placerat vel, dolor eleifend lectus blandit convallis fusce sagittis pharetra aptent felis vel, placerat commodo ornare ultricies, arcu nisi accumsan consequat risus dapibus, pulvinar varius placerat facilisi. Turpis ut mi leo torquent. Felis consequat pharetra. Libero. Dolor, montes proin. Egestas id penatibus suscipit pede orci natoque netus nostra lobortis, nec molestie dolor torquent purus, id tristique. Egestas adipiscing pretium convallis odio orci felis sit purus duis nulla.

Eros diam primis consectetuer sodales cubilia vitae tempus elementum vivamus. Varius mus tellus non platea volutpat torquent dolor Aenean interdum dictumst duis aptent felis, mus aliquet consectetuer eleifend a ligula diam proin, bibendum semper, pellentesque. Amet pulvinar. Mollis duis id tortor nunc urna eleifend a. Torquent sit. Commodo, aenean aptent tristique ut convallis montes ad nascetur faucibus vel adipiscing rhoncus ultricies ligula primis. Scelerisque velit vehicula. Pretium vehicula nunc inceptos. Cum ante rutrum semper torquent sed ac, mus vestibulum suscipit sodales volutpat, sociis pede. Enim vehicula primis. Quam. Senectus iaculis curabitur ante egestas eu vel. Neque condimentum semper pellentesque. Placerat. Purus sociis leo Cras nec ligula inceptos condimentum potenti. Integer curabitur lacinia hac. Porta, suscipit magna pulvinar ridiculus condimentum egestas diam varius sagittis ad purus felis porttitor dictumst ullamcorper convallis cubilia dolor nascetur lacus inceptos ultrices nostra nam. Massa felis diam magna, rutrum, parturient. Fames potenti ullamcorper nulla luctus nisl netus suscipit cum congue inceptos interdum lectus pulvinar venenatis dis. Mollis phasellus nibh mus taciti dolor imperdiet bibendum pharetra porttitor eleifend viverra. Sapien vehicula suspendisse vestibulum nunc ornare nec erat. Vivamus congue phasellus suspendisse. Ante donec, justo parturient litora, interdum in netus curabitur malesuada amet rhoncus justo erat penatibus mus porttitor urna lobortis dui sapien nulla sit nisi class ac nullam mus lobortis. Dignissim turpis euismod sapien condimentum. Et massa. Porta porta Consequat sed curae; mattis neque faucibus vivamus consequat tortor aliquam sodales, non porta accumsan cubilia tristique inceptos in molestie dictumst arcu lacus pharetra rhoncus lacinia ipsum ad felis arcu accumsan nullam primis. Vivamus aenean phasellus condimentum dapibus cursus pellentesque arcu maecenas facilisis aliquam sociosqu. Eros sagittis eleifend sapien vel. Mollis nisi nam facilisi venenatis consequat gravida sed nullam nullam pharetra sociosqu, elementum sed placerat nisl litora. Neque facilisi venenatis vestibulum ac sollicitudin iaculis placerat, platea faucibus neque hac potenti montes tempus dignissim hymenaeos pellentesque laoreet felis placerat penatibus nulla sociosqu porttitor mattis Litora. Phasellus interdum penatibus mus consectetuer Mi libero justo. Malesuada.
",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "MODA"){
      description <- tags$p("This is MODA",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "Module Discoverer"){
      description <- tags$p("This is Module Discoverer",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
    if (method == "Correlation Clique"){
      description <- tags$p("This is Correlation Clique",
                            id = "method",
                            style="word-break: break-word;")
      return(description)
    }
    
  }
  )
 
  output$description <- renderUI({
    upload_description()
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
 
