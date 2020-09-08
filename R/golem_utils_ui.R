#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#' 
#' @return an HTML list
#' @noRd
#' 
#' @examples
#' list_to_li(c("a","b"))
#'
#' @importFrom htmltools tags tagAppendAttributes tagList
list_to_li <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$li
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$li
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom htmltools tags tagAppendAttributes tagList
list_to_p <- function(list, class = NULL){
  if (is.null(class)){
    tagList(
      lapply(
        list, 
        tags$p
      )
    )
  } else {
    res <- lapply(
      list, 
      tags$p
    )
    res <- lapply(
      res, 
      function(x) { 
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
  
}

#' @importFrom htmltools tags tagAppendAttributes tagList
named_to_li <- function(list, class = NULL){
  if(is.null(class)){
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list, 
      names(list), 
      SIMPLIFY = FALSE
    )
    tagList(res)
  } else {
    res <- mapply(
      function(x, y){
        tags$li(
          HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list), 
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res, 
      function(x) {
        tagAppendAttributes(
          x, 
          class = class
        )
      }
    )
    tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#' 
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) {
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[ attrs[i] ]] <- NULL
  }
  tag
}

#' Hide or display a tag
#' 
#' @param tag the tag
#' 
#' @return a tag
#' @noRd
#' 
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
#' 
#' @importFrom htmltools tagList
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) && 
    !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;", 
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

#' @importFrom htmltools tagList
display <- function(tag) {
  if (
    !is.null(tag$attribs$style) && 
    grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*", 
      "", 
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#' 
#' @param id the id of the element to hide
#' 
#' @noRd
#' 
#' @importFrom htmltools tags
jq_hide <- function(id) {
  tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#' 
#' @examples
#' with_red_star("Enter your name here")
#' 
#' @importFrom htmltools tags HTML
with_red_star <- function(text) {
  htmltools::tags$span(
    HTML(
      paste0(
        text,
        htmltools::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#' 
#' @examples
#' rep_br(5)
#' 
#' @importFrom htmltools HTML
rep_br <- function(times = 1) {
  HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
#' 
#' @importFrom htmltools tags
enurl <- function(url, text){
  tags$a(href = url, text)
}

#' Columns wrappers
#' 
#' These are convenient wrappers around 
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#' 
#' @noRd
#' 
#' @importFrom shiny column
col_12 <- function(...){
  column(12, ...)
}

#' @importFrom shiny column
col_10 <- function(...){
  column(10, ...)
}

#' @importFrom shiny column
col_8 <- function(...){
  column(8, ...)
}

#' @importFrom shiny column
col_6 <- function(...){
  column(6, ...)
}


#' @importFrom shiny column
col_4 <- function(...){
  column(4, ...)
}


#' @importFrom shiny column
col_3 <- function(...){
  column(3, ...)
}


#' @importFrom shiny column
col_2 <- function(...){
  column(2, ...)
}


#' @importFrom shiny column
col_1 <- function(...){
  column(1, ...)
}

#' Modified shiny textInput func
textInput <- function(inputId, label, value = "", width = NULL,
                      placeholder = NULL, tooltip = T, header = "?", popup = "Help tips", pos = "right") {
  
  value <- restoreInput(id = inputId, default = value)
  
  div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      shiny:::shinyInputLabel(inputId, tagList(label,
                                               if (tooltip){
                                                 tags$span(
                                                   tags$button(
                                                     style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                                     class = "badge badge-pill badge-warning",
                                                     type = "button",
                                                     `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                                     `data-content` = popup,
                                                     header
                                                   ),
                                                   tags$script(src="www/tooltip.js"))
                                               })),
      tags$input(id = inputId, type="text", class="form-control", value=value,
                 placeholder = placeholder)
  )
}

#' Modified shiny numericInput func
numericInput <- function(inputId, label, value, min = NA, max = NA, step = NA,
                         width = NULL,  tooltip = T, header = "?", popup = "Help tips", pos = "right") {
  
  value <- restoreInput(id = inputId, default = value)
  
  # build input tag
  inputTag <- tags$input(id = inputId, type = "number", class="form-control",
                         value = shiny:::formatNoSci(value))
  if (!is.na(min))
    inputTag$attribs$min = min
  if (!is.na(max))
    inputTag$attribs$max = max
  if (!is.na(step))
    inputTag$attribs$step = step
  
  div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      shiny:::shinyInputLabel(inputId, tagList(label,
                                               if (tooltip){
                                                 tags$span(
                                                   tags$button(
                                                     style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                                     class = "badge badge-pill badge-warning",
                                                     type = "button",
                                                     `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                                     `data-content` = popup,
                                                     header
                                                   ),
                                                   tags$script(src="www/tooltip.js"))
                                               })),
      inputTag
  )
}

#' Modified shiny fileInput func
fileInput <- function(inputId, label, multiple = FALSE, accept = NULL,
                      width = NULL, buttonLabel = "Browse...", placeholder = "No file selected",
                      tooltip = T, header = "?", popup = "Help tips", pos = "right") {
  
  restoredValue <- restoreInput(id = inputId, default = NULL)
  
  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }
  
  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }
  
  inputTag <- tags$input(
    id = inputId,
    name = inputId,
    type = "file",
    style = "display: none;",
    `data-restore` = restoredValue
  )
  
  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse=',')
  
  
  div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      shiny:::shinyInputLabel(inputId, tagList(label,
                                               if (tooltip){
                                                 tags$span(
                                                   tags$button(
                                                     style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                                     class = "badge badge-pill badge-warning",
                                                     type = "button",
                                                     `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                                     `data-content` = popup,
                                                     header
                                                   ))
                                               })),
      
      div(class = "input-group",
          tags$label(class = "input-group-btn",
                     span(class = "btn btn-default btn-file",
                          buttonLabel,
                          inputTag
                     )
          ),
          tags$input(type = "text", class = "form-control",
                     placeholder = placeholder, readonly = "readonly"
          )
      ),
      
      tags$div(
        id=paste(inputId, "_progress", sep=""),
        class="progress progress-striped active shiny-file-input-progress",
        tags$div(class="progress-bar")
      )
  )
}

#' Modified shiny radioButtons func
radioButtons <- function(inputId, label, choices = NULL, selected = NULL,
                         inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL,
                         tooltip = T, header = "?", popup = "Help tips", pos = "right") {
  
  args <- shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
  
  selected <- restoreInput(id = inputId, default = selected)
  
  # default value if it's not specified
  selected <- if (is.null(selected)) args$choiceValues[[1]] else as.character(selected)
  
  if (length(selected) > 1) stop("The 'selected' argument must be of length 1")
  
  options <- shiny:::generateOptions(inputId, selected, inline,
                                     'radio', args$choiceNames, args$choiceValues)
  
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  if (inline) divClass <- paste(divClass, "shiny-input-container-inline")
  
  tags$div(id = inputId,
           style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
           class = divClass,
           shiny:::shinyInputLabel(inputId, tagList(label,
                                                    if (tooltip){
                                                      tags$span(
                                                        tags$button(
                                                          style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                                          class = "badge badge-pill badge-warning",
                                                          type = "button",
                                                          `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                                          `data-content` = popup,
                                                          header
                                                        ),
                                                        tags$script(src="www/tooltip.js"))
                                                    })),
           options
  )
}

#' Modified shiny sliderInput func
sliderInput <- function(inputId, label, min, max, value, step = NULL,
                        round = FALSE, format = NULL, locale = NULL,
                        ticks = TRUE, animate = FALSE, width = NULL, sep = ",",
                        pre = NULL, post = NULL, timeFormat = NULL,
                        timezone = NULL, dragRange = TRUE,
                        tooltip = T, header = "?", popup = "Help tips", pos = "right")
{
  if (!missing(format)) {
    shiny:::shinyDeprecated(msg = "The `format` argument to sliderInput is deprecated. Use `sep`, `pre`, and `post` instead.",
                            version = "0.10.2.2")
  }
  if (!missing(locale)) {
    shiny:::shinyDeprecated(msg = "The `locale` argument to sliderInput is deprecated. Use `sep`, `pre`, and `post` instead.",
                            version = "0.10.2.2")
  }
  
  dataType <- shiny:::getSliderType(min, max, value)
  
  if (is.null(timeFormat)) {
    timeFormat <- switch(dataType, date = "%F", datetime = "%F %T", number = NULL)
  }
  
  # Restore bookmarked values here, after doing the type checking, because the
  # restored value will be a character vector instead of Date or POSIXct, and we can do
  # the conversion to correct type next.
  value <- restoreInput(id = inputId, default = value)
  
  if (is.character(value)) {
    # If we got here, the value was restored from a URL-encoded bookmark.
    if (dataType == "date") {
      value <- as.Date(value, format = "%Y-%m-%d")
    } else if (dataType == "datetime") {
      # Date-times will have a format like "2018-02-28T03:46:26Z"
      value <- as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
  }
  
  step <- shiny:::findStepSize(min, max, step)
  
  if (dataType %in% c("date", "datetime")) {
    # For Dates, this conversion uses midnight on that date in UTC
    to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
    
    # Convert values to milliseconds since epoch (this is the value JS uses)
    # Find step size in ms
    step  <- to_ms(max) - to_ms(max - step)
    min   <- to_ms(min)
    max   <- to_ms(max)
    value <- to_ms(value)
  }
  
  range <- max - min
  
  # Try to get a sane number of tick marks
  if (ticks) {
    n_steps <- range / step
    
    # Make sure there are <= 10 steps.
    # n_ticks can be a noninteger, which is good when the range is not an
    # integer multiple of the step size, e.g., min=1, max=10, step=4
    scale_factor <- ceiling(n_steps / 10)
    n_ticks <- n_steps / scale_factor
    
  } else {
    n_ticks <- NULL
  }
  
  sliderProps <- shiny:::dropNulls(list(
    class = "js-range-slider",
    id = inputId,
    `data-type` = if (length(value) > 1) "double",
    `data-min` = shiny:::formatNoSci(min),
    `data-max` = shiny:::formatNoSci(max),
    `data-from` = shiny:::formatNoSci(value[1]),
    `data-to` = if (length(value) > 1) shiny:::formatNoSci(value[2]),
    `data-step` = shiny:::formatNoSci(step),
    `data-grid` = ticks,
    `data-grid-num` = n_ticks,
    `data-grid-snap` = FALSE,
    `data-prettify-separator` = sep,
    `data-prettify-enabled` = (sep != ""),
    `data-prefix` = pre,
    `data-postfix` = post,
    `data-keyboard` = TRUE,
    # This value is only relevant for range sliders; for non-range sliders it
    # causes problems since ion.RangeSlider 2.1.2 (issue #1605).
    `data-drag-interval` = if (length(value) > 1) dragRange,
    # The following are ignored by the ion.rangeSlider, but are used by Shiny.
    `data-data-type` = dataType,
    `data-time-format` = timeFormat,
    `data-timezone` = timezone
  ))
  
  # Replace any TRUE and FALSE with "true" and "false"
  sliderProps <- lapply(sliderProps, function(x) {
    if (identical(x, TRUE)) "true"
    else if (identical(x, FALSE)) "false"
    else x
  })
  
  sliderTag <- div(class = "form-group shiny-input-container",
                   style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
                   shiny:::shinyInputLabel(inputId, tagList(label,
                                                            if (tooltip){
                                                              tags$span(
                                                                tags$button(
                                                                  style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                                                  class = "badge badge-pill badge-warning",
                                                                  type = "button",
                                                                  `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                                                  `data-content` = popup,
                                                                  header
                                                                ),
                                                                tags$script(src="www/tooltip.js"))
                                                            })),
                   do.call(tags$input, sliderProps)
  )
  
  # Add animation buttons
  if (identical(animate, TRUE))
    animate <- animationOptions()
  
  if (!is.null(animate) && !identical(animate, FALSE)) {
    if (is.null(animate$playButton))
      animate$playButton <- icon('play', lib = 'glyphicon')
    if (is.null(animate$pauseButton))
      animate$pauseButton <- icon('pause', lib = 'glyphicon')
    
    sliderTag <- tagAppendChild(
      sliderTag,
      tags$div(class='slider-animate-container',
               tags$a(href='#',
                      class='slider-animate-button',
                      'data-target-id'=inputId,
                      'data-interval'=animate$interval,
                      'data-loop'=animate$loop,
                      span(class = 'play', animate$playButton),
                      span(class = 'pause', animate$pauseButton)
               )
      )
    )
  }
  
  dep <- list(
    htmltools::htmlDependency("ionrangeslider", "2.1.6", c(href="shared/ionrangeslider"),
                              script = "js/ion.rangeSlider.min.js",
                              # ion.rangeSlider also needs normalize.css, which is already included in
                              # Bootstrap.
                              stylesheet = c("css/ion.rangeSlider.css",
                                             "css/ion.rangeSlider.skinShiny.css")
    ),
    htmltools::htmlDependency("strftime", "0.9.2", c(href="shared/strftime"),
                              script = "strftime-min.js"
    )
  )
  
  htmltools::attachDependencies(sliderTag, dep)
}

#' Modified shiny sliderInput func
selectInput <- function(inputId, label, choices, selected = NULL,
                        multiple = FALSE, selectize = TRUE, width = NULL,
                        size = NULL,  tooltip = T, header = "?", popup = "Help tips", pos = "right") {
  
  selected <- restoreInput(id = inputId, default = selected)
  
  # resolve names
  choices <- shiny:::choicesWithNames(choices)
  
  # default value if it's not specified
  if (is.null(selected)) {
    if (!multiple) selected <- shiny:::firstChoice(choices)
  } else selected <- as.character(selected)
  
  if (!is.null(size) && selectize) {
    stop("'size' argument is incompatible with 'selectize=TRUE'.")
  }
  
  # create select tag and add options
  selectTag <- tags$select(
    id = inputId,
    class = if (!selectize) "form-control",
    size = size,
    shiny:::selectOptions(choices, selected)
  )
  if (multiple)
    selectTag$attribs$multiple <- "multiple"
  
  # return label and select tag
  res <- div(
    class = "form-group shiny-input-container",
    
    style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
    shiny:::shinyInputLabel(inputId, tagList(label,
                            if (tooltip){
                              tags$span(
                                tags$button(
                                  style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                  class = "badge badge-pill badge-warning",
                                  type = "button",
                                  `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                  `data-content` = popup,
                                  header
                                ),
                                tags$script(src="www/tooltip.js"))
                            })),
    
    div(selectTag)
  )
  
  if (!selectize) return(res)
  
  selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% choices))
}

selectizeIt <- function(inputId, select, options, nonempty = FALSE) {
  res <- shiny:::checkAsIs(options)
  
  selectizeDep <- htmltools::htmlDependency(
    "selectize", "0.11.2", c(href = "shared/selectize"),
    stylesheet = "css/selectize.bootstrap3.css",
    head = format(tagList(
      HTML('<!--[if lt IE 9]>'),
      tags$script(src = 'shared/selectize/js/es5-shim.min.js'),
      HTML('<![endif]-->'),
      tags$script(src = 'shared/selectize/js/selectize.min.js')
    ))
  )
  
  if ('drag_drop' %in% options$plugins) {
    selectizeDep <- list(selectizeDep, htmltools::htmlDependency(
      'jqueryui', '1.12.1', c(href = 'shared/jqueryui'),
      script = 'jquery-ui.min.js'
    ))
  }
  
  
  # Insert script on same level as <select> tag
  select$children[[2]] <- tagAppendChild(
    select$children[[2]],
    tags$script(
      type = 'application/json',
      `data-for` = inputId, `data-nonempty` = if (nonempty) '',
      `data-eval` = if (length(res$eval)) HTML(toJSON(res$eval)),
      if (length(res$options)) HTML(toJSON(res$options)) else '{}'
    )
  )
  
  htmltools::attachDependencies(select, selectizeDep)
}


#' Modified shinyWidgets prettySwitch func
prettySwitch <- function(inputId, label, value = FALSE, status = "default",
                         slim = FALSE, fill = FALSE, bigger = FALSE,
                         inline = FALSE, width = NULL,  tooltip = T, header = "?",
                         popup = "Help tips", pos = "right") {
  value <- shiny::restoreInput(id = inputId, default = value)
  status <- match.arg(status, c("default", "primary", "success",
                                "info", "danger", "warning"))
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value)
    inputTag$attribs$checked <- "checked"
  if (fill & slim)
    message("slim = TRUE & fill = TRUE don't work well together.")
  switchTag <- tags$div(
    class = "form-group shiny-input-container",
    style = if (!is.null(width))  paste0("width: ",
                                         htmltools::validateCssUnit(width), ";"),
    class = if (inline) "shiny-input-container-inline",
    style = if (inline) "display: inline-block; margin-right: 10px;",
    tags$div(
      class="pretty p-default p-switch", inputTag,
      class=if(bigger) "p-bigger",
      class=if(fill) "p-fill", class=if(slim) "p-slim",
      tags$div(
        class="state",
        class=if(status != "default") paste0("p-", status),
        tags$label(tags$span(label,
                             if (tooltip){
                               tags$span(
                                 tags$button(
                                   style ="border:none; left:2px; padding: 1px 4px; font-size:11px; background-color:#798D8F; position:relative; outline:none;",
                                   class = "badge badge-pill badge-warning",
                                   type = "button",
                                   `data-tipped-options` = paste("position:'", pos, "'", sep = ""),
                                   `data-content` = popup,
                                   header
                                 ),
                                 tags$script(src="www/tooltip.js"))
                             })),
        
      )
    )
  )
  shinyWidgets:::attachShinyWidgetsDep(switchTag, "pretty")
}

#' Modified absolutPanel from shiny
absolutePanel <- function(...,
                          top = NULL, left = NULL, right = NULL, bottom = NULL,
                          width = NULL, height = NULL,
                          draggable = FALSE, fixed = FALSE,
                          cursor = c('auto', 'move', 'default', 'inherit')) {
  cssProps <- list(
    top = top,
    left = left,
    right = right,
    bottom = bottom,
    width = width,
    height = height
  )
  cssProps <- cssProps[!sapply(cssProps, is.null)]
  cssProps <- sapply(cssProps, validateCssUnit)
  cssProps[['position']] <- ifelse(fixed, 'fixed', 'absolute')
  cssProps[['cursor']] <- match.arg(cursor)
  if (identical(cssProps[['cursor']], 'auto'))
    cssProps[['cursor']] <- ifelse(draggable, 'move', 'inherit')
  
  style <- paste(paste(names(cssProps), cssProps, sep = ':', collapse = ';'), ';', sep='')
  divTag <- tags$div(style=style, tags$p(id="collasp", "-"), ...)
  if (isTRUE(draggable)) {
    divTag <- tagAppendAttributes(divTag, class='draggable')
    return(tagList(
      divTag,
      tags$script('$(".draggable").draggable();'),
      tags$script("$('#collasp').click(function(){
        $('#collasp-panel').slideToggle();
      });")
    ))
  } else {
    return(divTag)
  }
}

#' Mopdified modalDialog function
modalDialog <- function(..., title = NULL, footer = modalButton("Dismiss"),
                        size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE, top = NULL) {
  
  size <- match.arg(size)
  
  cls <- if (fade) "modal fade" else "modal"
  div(id = "shiny-modal", class = cls, tabindex = "-1",
      `data-backdrop` = if (!easyClose) "static",
      `data-keyboard` = if (!easyClose) "false",
      
      div(
        class = "modal-dialog",
        style = if (is.null(top)) NULL else paste("top:", top, "%", sep = ""),
        class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg"),
        div(class = "modal-content",
            if (!is.null(title)) div(class = "modal-header",
                                     tags$h4(class = "modal-title", title)
            ),
            div(class = "modal-body", ...),
            if (!is.null(footer)) div(class = "modal-footer", footer)
        )
      ),
      tags$script("$('#shiny-modal').modal().focus();")
  )
}

# UNCOMMENT AND USE 
# 
# usethis::use_package("markdown")
# usethis::use_package("rmarkdown")
#   
# To use this part of the UI
#   
#' #' Include Content From a File
#' #' 
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #' 
#' #' @rdname includeRMarkdown
#' #' @export
#' #' 
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom htmltools HTML
#' includeRMarkdown <- function(path){
#'   
#'   md <- tempfile(fileext = '.md')
#'   
#'   on.exit(unlink(md),add = TRUE)
#'   
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'   
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'   
#'   Encoding(html) <- "UTF-8"
#'   
#'   return(HTML(html))
#' }
