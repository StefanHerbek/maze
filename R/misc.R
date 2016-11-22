#' Generate a square maze
#'
#' Generate a square maze of the specified dimentions using a specified weight function.
#'
#' @param nrow number of rows.
#' @param ncol number of columns.
#' @param weight.fun weight function.
#'
#' @return An object of class igraph. A minimum spaning tree.
#' @export
#'
#' @examples
#' make_maze(2, 2)
make_maze <- function(nrow = 1, ncol = 1, weight.fun = "rnorm") {
  weight.fun <- match.arg(weight.fun, c("rnorm", "runif"))
  g <- graph.lattice(c(nrow, ncol))

  g$ncol <- ncol
  g$nrow <- nrow

  switch(weight.fun,
         runif = wf <- runif,
         rnorm = wf <- rnorm
  )
  w <- wf(ecount(g))

  s <- mst(g, weights = w, algorithm = "prim")
  s$layout <- layout_on_grid(s, width = nrow, height = ncol)
  s
}
