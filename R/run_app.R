#' Run the Shiny Application
#'
#' @param env type of environment to run the application. It can take the
#' following values: "default" and "production"
#' @param ... arguments to pass to golem_opts
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    env = c("default", "production"),
    options = list(), 
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = ApiConnections(env),
      options = options, 
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}
