#' create_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_vis_enrichobj_ui <- function(id){
  ns <- NS(id)
  tagList(
    #The blue little box
    tags$div(`class`="row",
             tags$div(`class`="col-sm-4", style = "-webkit-animation: fadein 1s; -moz-animation: fadein 1s; -ms-animation: fadein 1s;-o-animation: fadein 1s; animation: fadein 1s;",
                      tags$form(class = "well",
                                `style`="background-color:#2c3e50;",
                                uiOutput(ns("input_choice")),
                                ),
                      ),
              ),
    htmlOutput(ns("fadein")),
  
    #htmlOutput() or uiOutput()
    tags$head(tags$script(src = "www/alert.js", type="text/javascript")),
    htmlOutput(ns("alert")),
    
    
    )
}

#' create_input Server Function
#'
#' @noRd 
mod_vis_enrichobj_server <- function(input, output, session, con){
  ns <- session$ns
  #get_available_input_objects(con)$input_name need to be changed to the enrichment object
  if (length(unlist((MODifieRDB::get_available_input_objects(con)$input_name)))==0){
     output$input_choice <- renderUI({
       
       
       tags$script(
         alert("No enrichment object has been detected! Please return to the Input tab")
       )

       
       #Another solution
        box <- tagList(tags$button(paste("No erichment object detected. Click me to learn more"),
                                           class = "link",
                                           type = "button",
                                           `data-toggle` = "modal",
                                           `data-target` = "#descrip"),
                              tags$div(`class` = "modal fade", `id` = "descrip", `role` = "dialog", `tabindex` = "-1", `aria-hidden` = "true", `style` = "display:none;",
                                       tags$div(`class` = "modal-dialog",
                                                tags$div(`class` = "modal-content",
                                                         tags$div(`class` = "modal-header",
                                                                  tags$button("×", type = "button", class = "close", `data-dismiss` = "modal", `aria-hidden` = "true"),
                                                                  tags$h4(class = "modal-title", style = "color: black")),
                                                         tags$div(`class` = "modal-body",
                                                                  tags$p("No enrichment object has been detected. Please go to the input data tab in order to create your enrichment object",style = "color: black")),
                                                         tags$div(`class` = "modal-footer",
                                                                  tags$button("Close", class = "btn btn-default", `data-dismiss` = "modal"),
                                                                  tags$a("Input data tab (now google)", class= "btn btn-default", href="https://www.google.com/", style = "background-color: #2c3e50; border-color: #2c3e50"))))))
       return(box)
       #End of another solution
     })
     }
  
  #get_available_input_objects(con)$input_name need to be changed to the enrichment object
  #creat_input need to be changed to the module for the plots
  
  if (length(unlist((MODifieRDB::get_available_input_objects(con)$input_name))) != 0){
    output$input_choice <- renderUI({
      input_objects <- unlist(MODifieRDB::get_available_input_objects(con)$input_name)
      tagList(
        selectInput(ns("input_object"), label = "Enriched object", choices = input_objects, popup = "Choose the enrichment method you want to visualize"),
        tags$div(style = "text-align:center",
                 actionButton(ns("create_input"), "Visualize object")),
      )
    })
  }

}

## To be copied in the UI
# mod_vis_enrichobj_ui("vis_enrichobj_ui_1")

## To be copied in the server
# callModule(mod_vis_enrichobj_server, "vis_enrichobj_ui_1")