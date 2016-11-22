#' Shiny app for plotting mazes
#'
#' @return NULL
#' @export
#'
mazeUI <- function() {
  runApp(system.file("extfiles/mazeapp.R", package = "maze"))
}
