
#' Plot a simple maze.
#'
#' @param g graph with maze structure.
#' @param wall.size size of walls.
#' @param tile.show logical; whether to show tile borders.
#' @param tile.size size of tile borders.
#' @param tile.color color of tile borders.
#' @param tile.number.show logical; whether to show tile number.
#' @param tile.number.size size of tile numbers.
#' @param path.show logical; whether to show the path between two tiles.
#' @param path.start starting point for path.
#' @param path.end end point for path.
#'
#' @return NULL
#' @export
#'
#' @examples
#' NULL
plot_maze <- function(g, wall.size = 5.0, tile.show = FALSE, tile.size = 1, tile.color = "white", tile.number.show = FALSE, tile.number.size = 5, path.show = FALSE, path.start = 1, path.end = vcount(g)) {
  nc <- g$ncol
  nr <- g$nrow

  d <- graph_to_df(g, tile.show = tile.show, tile.size = tile.size, wall.size = wall.size)

  gg <- ggplot(d) +
    geom_rect(xmin = 1 - .5, xmax = nc + .5, ymin = 1 - .5, ymax = nr + .5, color = "black", fill = tile.color, lwd = wall.size) +
    geom_segment(aes_string(x = "x0", y = "y0", xend = "x1", yend = "y1"), size = d$size, lineend = "square") +
    xlim(0.5, nc + .5) +
    ylim(0.5, nr + .5) +
    theme_void() +
    guides(size = "none") +
    theme(aspect.ratio = nr / nc, panel.border = element_blank())

  if (tile.number.show) {
    d <- expand.grid(x = 1:nc, y = 1:nr)
    d$index <- 1:(nc * nr)
    gg <- gg + geom_text(data = d, mapping = aes_string(x = "x", y = "y", label = "index"), size = tile.number.size)
  }

  if (path.show) {
    idxs <- get.shortest.paths(g, path.start, path.end)$vpath[[1]]
    p <- list()
    for (idx in idxs) {
      ci <- idx %% nc
      ri <- idx %/% nc + 1
      if(ci == 0) {
        ci <- nc
        ri <- ri - 1
      }
      p[[length(p) + 1]] <- data.frame(x = ci, y = ri)
    }
    p <- do.call(rbind, p)
    gg <- gg + geom_path(data = p, mapping = aes_string(x = "x", y = "y"), color = "red", lwd = 1, lineend = "round")
  }
  gg
}

#' Plot a maze's graph.
#'
#' @param g graph with maze structure.
#' @param layout layout (default taken from g$layout).
#' @param labels logical; whether to plot node labels.
#' @param edge.color color used for edges.
#' @param vertex.color color used for node borders.
#' @param vertex.fill color used for nodes.
#' @param vertex.size node size.
#' @param vertex.label.color color used for node labels.
#' @param path.show logical; whether to show the path between two nodes.
#' @param path.start path starting node.
#' @param path.end path end node.
#' @param ... extra arguments passed down to geom_segment().
#'
#' @return NULL
#' @export
#'
plot_graph <- function(g, layout = NULL, labels = TRUE, edge.color = "black", vertex.color = "black", vertex.fill = "white", vertex.size = 15, vertex.label.color = "white", vertex.label.size = 1, path.show = FALSE, path.start = 1, path.end = vcount(g), ...) {
  if (path.show) {
    if (length(vertex.color) == 1)
      vertex.color <- rep(vertex.color, vcount(g))
    if (length(vertex.fill) == 1)
      vertex.fill <- rep(vertex.fill, vcount(g))
    if (length(edge.color) == 1)
      edge.color <- rep(edge.color, ecount(g))

    sp <- get.shortest.paths(g, from = path.start, to = path.end)$vpath[[1]]

    vertex.color[sp] <- "red"
    vertex.fill[sp] <- "red"
    edge.color[E(g, path = sp)] <- "red"
  }

  if (is.null(layout))
    layout <- g$layout
  m <- layout
  d_n <- data.frame(x = m[, 1], y = m[, 2])

  el <- get.edgelist(g)
  n_i <- el[,1]
  n_j <- el[,2]
  d_e <- data.frame(source = n_i, target = n_j, x = m[n_i, 1], y = m[n_i, 2], xend = m[n_j, 1], yend = m[n_j, 2])

  gg <- ggplot(d_e, aes_string(x = "x", y = "y")) +
    geom_segment(
      aes_string(xend = "xend", yend = "yend"),
      color = edge.color, ...) +
    geom_point(
      aes_string(x = "x", y = "y"),
      data = d_n,
      shape = 21,
      size = vertex.size,
      color = vertex.color,
      fill = vertex.fill,
      stroke = 1
    ) +
    theme_void() +
    labs(x = "", y = "") +
    theme(aspect.ratio = g$nrow /  g$ncol, panel.border = element_blank())
  if(labels) {
    if(! is.null(V(g)$labels)) {
      gg <- gg + geom_text(aes_string(x = "x", y = "y"), label = V(g)$labels, data = d_n, color = vertex.label.color, size = vertex.label.size)
    }
  }
  gg
}
