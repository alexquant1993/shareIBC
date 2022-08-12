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
           shiny::icon("circle-question")
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


#' Form CheckBoxGroup element
#' @param inputId Checkbox group input.
#' @param label Checkbox group label.
#' @param choices Checkbox group choices.
#' @param selected Checkbox group selected value.

#' @noRd
formCheckBoxGroup <- function (inputId, label, choices = NULL, selected = NULL) {
  selectedPositions <- if (!is.null(selected)) {
    match(selected, choices)
  } else {NULL}
  choicesTag <-
    lapply(X = seq_along(choices), function(i) {
      shiny::tags$li(
        shiny::tags$label(
          class = "item-checkbox item-content", 
          shiny::tags$input(
            type = "checkbox",
            name = inputId, 
            value = choices[[i]],
            class = "checkbox_group"
          ),
          shiny::tags$i(class = "icon icon-checkbox"), 
          shiny::tags$div(
            class = "item-inner",
            shiny::tags$div(
              class = "item-title", 
              names(choices)[i]
            )
          )
        )
      )
    })
  if (!is.null(selected)) {
    for (i in seq_along(selectedPositions)) {
      choicesTag[[selectedPositions[i]]]$children[[1]]$children[[1]]$attribs$checked <- "checked"
    }
  }
  shiny::tagList(
    shiny::tags$div(
      class = "item-title",
      label
    ), 
    shiny::tags$div(
      class = "list shiny-input-checkboxgroup", 
      id = inputId,
      shiny::tags$ul(choicesTag)
    )
  )
}
