#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      # Change background color
      shinyWidgets::setBackgroundColor(color = "#ecf0f1"),
      
      # Loading modal
      tags$div(id = "loading_modal", `class`="modal fade loading-modal", `data-backdrop`="static", `data-keyboard`="false", tabindex="-1",
               tags$div(id="modal-dialog", `class`="modal-dialog modal-sm",
                        tags$div(`class`="modal-content",
                                 tags$span(class="fa fa-spinner fa-spin fa-3x"),
                                 tags$div(id="stopwatch", "00:00")))),
      # Loading screen
      tags$div(id = "loader-wrapper",
               mod_loading_screen_ui("loading_screen_ui_1")),
      
      # Main module
      tags$div(style = "-webkit-animation: fadein 0.8s; -moz-animation: fadein 0.8s; -ms-animation: fadein 0.8s;-o-animation: fadein 0.8s; animation: fadein 0.8s;",
               mod_main_page_v2_ui("main_page_v2_ui_1")),
      tags$script(src = "www/tooltip.js")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MODifieRWeb'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

