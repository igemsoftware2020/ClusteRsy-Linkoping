#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    fluidPage(
      #h1("MODifieRWeb"),
      #mod_main_page_ui("main_page_ui_1"),
      theme = shinytheme("flatly"),
      mod_main_page_v2_ui("main_page_v2_ui_1"),
      # Change background color
      setBackgroundColor(
        color = c("#ecf0f1")
      )
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

