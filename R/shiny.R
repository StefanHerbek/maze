#' Shiny app for plotting mazes
#'
#' @return NULL
#' @export
#'
mazeUI <- function() {
  runApp(system.file("shiny/maze/app.R", package = "maze"))
}
