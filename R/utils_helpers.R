chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices,
                        style = "color: black; width: 255px;background-color: #ecf0f1; border-radius: 8px;")
        ),
        div(class="chooser-container chooser-center-container", style = "position: relative; bottom: 5px; left: 1px;",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices,
                        style = "color: black; width: 254px;background-color: #ecf0f1; border-radius: 8px;")
        )
    )
  )
}