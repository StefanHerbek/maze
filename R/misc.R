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

  g$ncol <- nrow
  g$nrow <- ncol

  switch(weight.fun,
         runif = wf <- runif,
         rnorm = wf <- rnorm
  )
  w <- wf(ecount(g))

  s <- mst(g, weights = w, algorithm = "prim")
  V(s)$labels <- seq_len(vcount(s))
  s$layout <- layout_on_grid(s, width = nrow, height = ncol)
  s
}

create_square_maze <- function(nrow = 3, ncol = 3) {
  g <- create_lattice(c(nrow, ncol))
  ne <- g %>% activate(edges) %>% as_tibble() %>% nrow()
  w <- rnorm(ne)
  g <- g %>% morph(to_minimum_spanning_tree, weights = w) %>% crystallize()
  g[["graph"]][[1]]
}


plot_maze2 <- function(x) {
  UseMethod("plot_maze2")
}

plot_maze2.layout_igraph <- function(x) {
  ggraph(x) + geom_edge_link()
}

plot_maze2.tbl_graph <- function(x) {
  l <- create_layout(x, "grid")
  plot_maze2(l)
}

plot_graph2 <- function(x) {
  UseMethod("plot_graph2")
}

plot_graph2.layout_igraph <- function(x) {
  ggraph(x) + geom_edge_link() + geom_node_point() + coord_fixed() + theme_graph()
}

plot_graph2.tbl_graph <- function(x) {
  l <- create_layout(x, "grid")
  plot_graph2(l)
}


# convert graph object to data.frame format.
graph_to_df <- function(x, nrow = NULL, ncol = NULL, tile.show = FALSE, tile.size = .5, wall.size = 1) {
  if (is.null(nrow))
    nrow <- x$nrow

  if (is.null(ncol))
    ncol <- x$ncol

  if (is.null(nrow) || is.null(ncol))
    stop("some of the maze dimensions are NULL")

  d <- list()
  for (i in 1:nrow) {
    for (j in 2:ncol) {
      i1 <- i2 <- i
      j1 <- j - 1
      j2 <- j
      v1 <- (i1 - 1) * ncol + j1
      v2 <- (i2 - 1) * ncol + j2
      #message(v1, ":", v2)
      if (!are_adjacent(x, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j - .5, y0 = i - .5, y1 = i + .5, size = wall.size)
      else
        if (tile.show)
          d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j - .5, y0 = i - .5, y1 = i + .5, size = tile.size)
      #lines(x = c(j - .5, j - .5), y = c(i - .5, i + .5), col = "black", lwd = 5)
    }
  }
  for (i in 2:nrow) {
    for (j in 1:ncol) {
      i1 <- i - 1
      i2 <- i
      j1 <- j2 <- j
      v1 <- (i1 - 1) * ncol + j1
      v2 <- (i2 - 1) * ncol + j2
      #message(v1, ":", v2)
      if (!are_adjacent(x, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i - .5, size = wall.size)
      else
        if (tile.show)
          d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i - .5, size = tile.size)
      #lines(x = c(j - .5, j + .5), y = c(i - .5, i - .5), col = "black", lwd = 5)
    }
  }
  do.call(rbind, d)
}
