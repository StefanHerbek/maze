#' Shiny app for plotting mazes
#'
#' @return NULL
#' @export
#'
mazeUI <- function() {
  runApp(system.file("shiny/app.R", package = "maze"))
}
