#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(db = NULL,...){ 
  if (is.null(db)){
  db <- paste(system.file("database", package = "ClusteRsy"), "/igem.db", sep="")
  }
  db <<- db
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server,
#      options = (list(
#        host = "192.168.50.55",
#        port = 80,
#        launch.browser  = F
#      )
#      )
    ), 
    golem_opts = list(...)
  )
}
