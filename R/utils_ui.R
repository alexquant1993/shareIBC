# Utils for all tabs of the app
#' Replace values if it is not Truthy (shiny definition)
#' If \code{x} is not Truthy (NULL or NA values) then it
#' is replaced by \code{replace}.
#' @param x string
#' @param replace string that replaces x if it is not truthy
#' @noRd
replaceTruthy <- function(x, replace = "\u2014") {
  if (!shiny::isTruthy(x)) {
    out <- replace
  } else {
    out <- x
  }
  out
}

#' This functions creates an question mark that shows a pop-up window
#' @noRd
helpPopup <- function(content) {
  shiny::a(href = "#",
    class = "tooltip-init",
    `data-tooltip` = content,
    shiny::icon("question-circle")
  )
}

#' Inline Block
#' @param icon default NULL. Use Framework7 icons
#' @noRd
inlineBlock <- function(..., icon = NULL){
  shiny::tags$div(
    style = "display: inline-block; width:100%;",
    shinyMobile::f7Icon(icon),
    ...
  )
}

#' Special input functions to place them in a form
#' @param ... Form elements
#' @noRd
formList <- function(...) {
  list_class <- "list no-hairlines-md"
  shiny::tags$div(class = list_class,
                  shiny::tags$ul(...)
  )
}

#' Form Text element
#' @param inputId Text input id.
#' @param label Text input label.
#' @param value Text input value.
#' @param placeholder Text input placeholder.
#' @noRd
formText <- function(inputId, label, value = "", placeholder = NULL){
  itemCl <- "item-content item-input"
  itemLabelCl <- "item-title"
  inputTag <-
    shiny::tags$input(id = inputId, value = value, 
                      type = "text", placeholder = placeholder)
  shiny::tags$li(
    class = itemCl,
    shiny::tags$div(
      class = "item-media",
      # f7Icon("app_fill")
      shiny::tags$i(
        class = "icon demo-list-icon"
      )
    ),
    shiny::tags$div(
      class = "item-inner",
      shiny::tags$div(
        class = itemLabelCl, 
        label
      ),
      shiny::tags$div(
        class = "item-input-wrap", 
        inputTag, shiny::span(class = "input-clear-button")
      )
    )
  )
}

#' Form TextArea element
#' @param inputId Text input id.
#' @param label Text input label.
#' @param value Text input value.
#' @param placeholder Text input placeholder.
#' @param resize Whether to box can be resized. Default to FALSE
#' @noRd
formTextArea <- function (inputId,
                          label,
                          value = "",
                          placeholder = NULL,
                          resize = FALSE) {
  areaCl <- if (resize) {"resizable"} else {NULL}
  shiny::tags$li(
    class = "item-content item-input",
    shiny::tags$div(
      class = "item-media",
      # f7Icon("app_fill")
      shiny::tags$i(
        class = "icon demo-list-icon"
      )
    ),
    shiny::tags$div(
      class = "item-inner",
      shiny::tags$div(
        class = "item-title", 
        label
      ),
      shiny::tags$div(
        class = "item-input-wrap", 
        shiny::tags$textarea(
          id = inputId, value, placeholder = placeholder, class = areaCl
        ),
        shiny::span(class = "input-clear-button")
      )
    )
  )
}



#' Form Select element
#' @param inputId Select input id
#' @param label Select input label
#' @param choices Select input choices
#' @param selected Select input default selected value
#' @noRd
formSelect <- function(inputId, label, choices, selected = NULL){
  options <- createSelectOptions(choices, selected)
  selectTag <-
    shiny::tags$li(
      class = "item-content item-input",
      shiny::tags$div(
        class = "item-media",
        shiny::tags$i(
          class = "icon demo-list-icon"
        )
      ),
      shiny::tags$div(
        class = "item-inner",
        shiny::tags$div(
          class = "item-title", 
          label
        ),
        shiny::tags$div(
          class = "item-input-wrap input-dropdown-wrap", 
          shiny::tags$select(
            class = "input-select",
            id = inputId, 
            placeholer = "Please choose...",
            options
          )
        )
      )
    )
  shiny::tagList(InputsDeps(), selectTag)
}

#' Form CheckBox element
#' @param inputId Select input id
#' @param label Select input label
#' @param value Select input value. Default FALSE.
#' @noRd
formCheckBox <- function (inputId, label, value = FALSE) {
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- shiny::tags$input(id = inputId, type = "checkbox")
  if (!is.null(value) && value) 
    inputTag$attribs$checked <- "checked"
  shiny::tags$label(
    class = "item-checkbox item-content item-input", 
    inputTag,
    shiny::tags$i(class = "icon icon-checkbox"), 
    shiny::tags$div(
      class = "item-inner",
      shiny::tags$div(
        class = "item-title no-white-space", 
        label
      )
    )
  )
}




# Functions not exported by shinyMobile
#' Auxiliar function not exported by shinyMobile
#' @noRd
choicesWithNames <- function (choices) {
  listify <- function(obj) {
    makeNamed <- function(x) {
      if (is.null(names(x))) 
        names(x) <- character(length(x))
      x
    }
    res <- lapply(obj, function(val) {
      if (is.list(val)) 
        listify(val)
      else if (length(val) == 1 && is.null(names(val))) 
        val
      else makeNamed(as.list(val))
    })
    makeNamed(res)
  }
  choices <- listify(choices)
  if (length(choices) == 0) 
    return(choices)
  choices <- mapply(choices, names(choices), FUN = function(choice, 
                                                            name) {
    if (!is.list(choice)) 
      return(choice)
    if (name == "") 
      stop("All sub-lists in \"choices\" must be named.")
    choicesWithNames(choice)
  }, SIMPLIFY = FALSE)
  missing <- names(choices) == ""
  names(choices)[missing] <- as.character(choices)[missing]
  choices
}

#' Auxiliar function not exported by shinyMobile
#' @noRd
createSelectOptions <- function (choices, selected) {
  choices <- choicesWithNames(choices)
  options <- lapply(X = seq_along(choices), function(i) {
    shiny::tags$option(value = choices[[i]], names(choices)[i], 
                       selected = if (!is.null(selected)) {
                         if (choices[[i]] %in% selected) 
                           NA
                         else NULL
                       })
  })
  return(options)
}

#' F7InputsDeps function
#' @noRd
InputsDeps <- function () {
  htmltools::htmlDependency(name = "framework7-bindings", 
                            version = as.character(packageVersion("shinyMobile")), 
                            src = c(file = "framework7-5.5.0", href = "framework7-5.5.0"), 
                            package = "shinyMobile", script = "framework7.bindings.min.js")
}