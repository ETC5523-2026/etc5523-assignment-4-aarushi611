#' Launch the rollout explorer
#'
#' Opens the interactive comparison of vaccination coverage and reported cases.
#' @return The result of \code{shiny::runApp()}, invisibly.
#' @seealso [summarise_rollout()], [rollout_weeks]
#' @examples
#' if (interactive()) run_app()
#' @importFrom bslib bs_theme
#' @export
run_app <- function() {
  shiny::runApp(system.file("app", package = "rolloutcompare", mustWork = TRUE))
}
