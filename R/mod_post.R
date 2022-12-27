#' post UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_post_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("bttn_post"),
      label = span(f7Icon("plus_app_fill"), "Post"),
      class = "button button-fill button-small",
      style = "width: 45%;"
    ),
    actionButton(
      inputId = ns("bttn_subs"),
      label = span(f7Icon("bell_fill"), "Subscribe"),
      class = "button button-fill button-small",
      style = "width: 55%;"
    ),
    f7Popup(
      id = ns("popup_post"),
      title = "Select the type of post",
      f7Block(
        strong(f7BlockHeader("Share with your IBC church community")),
        "God delights in His benevolent nature. He is kind, gracious, generous,
        patient, and loving. He is unrelenting in his care for us, and we are
        called to do the same with others. Thank you for sharing!"
      ),
      f7List(
        f7ListItem(
          mod_post_tabs_ui(
            ns("jobs"),
            "Share job opportunities",
            "briefcase_fill"
          )
        ),
        f7ListItem(
          mod_post_tabs_ui(
            ns("services"),
            "Offer your professional services",
            "person_2_square_stack"
          )
        ),
        f7ListItem(
          mod_post_tabs_ui(
            ns("upcycle"),
            "Upcycle and donate",
            "gift_fill"
          )
        ),
        f7ListItem(
          mod_post_tabs_ui(
            ns("mix"),
            "Miscellaneous",
            "burst_fill"
          )
        )
      )
    ),
    f7Popup(
      id = ns("popup_subs"),
      title = "Subscription settings",
      mod_subscription_ui(ns("subscription"))
    )
  )
}

#' share_post Server Functions
#'
#' @noRd 
mod_post_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # Toggle post pop up
    observeEvent(input$bttn_post, {
      updateF7Popup(id = "popup_post")
    })
    
    # Post popup tabs server
    mod_post_tabs_server("jobs")
    mod_post_tabs_server("services")
    mod_post_tabs_server("upcycle")
    mod_post_tabs_server("mix")
    
    # Toggle subscribe pop up
    observeEvent(input$bttn_subs, {
      updateF7Popup(id = "popup_subs")
    })
    
    # Subscription server logic
    mod_subscription_server("subscription")
    
  })
}

## To be copied in the UI
# mod_post_ui("post_ui")

## To be copied in the server
# mod_post_server("post_ui")
