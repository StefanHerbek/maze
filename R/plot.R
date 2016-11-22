
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
plotMaze <- function(g, wall.size = 5.0, tile.show = FALSE, tile.size = 1, tile.color = "white", tile.number.show = FALSE, tile.number.size = 5, path.show = FALSE, path.start = 1, path.end = nrow*ncol) {
  nc <- g$ncol
  nr <- g$nrow
  d <- list()
  for (i in 1:nr) {
    for (j in 2:nc) {
      i1 <- i2 <- i
      j1 <- j - 1
      j2 <- j
      v1 <- (i1 - 1) * nc + j1
      v2 <- (i2 - 1) * nc + j2
      #message(v1, ":", v2)
      if (!are_adjacent(g, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j - .5, y0 = i - .5, y1 = i + .5, size = wall.size)
      else
        if (tile.show)
          d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j - .5, y0 = i - .5, y1 = i + .5, size = tile.size)
      #lines(x = c(j - .5, j - .5), y = c(i - .5, i + .5), col = "black", lwd = 5)
    }
  }
  for (i in 2:nr) {
    for (j in 1:nc) {
      i1 <- i - 1
      i2 <- i
      j1 <- j2 <- j
      v1 <- (i1 - 1) * nc + j1
      v2 <- (i2 - 1) * nc + j2
      #message(v1, ":", v2)
      if (!are_adjacent(g, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i - .5, size = wall.size)
      else
        if (tile.show)
          d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i - .5, size = tile.size)
      #lines(x = c(j - .5, j + .5), y = c(i - .5, i - .5), col = "black", lwd = 5)
    }
  }
  d <- do.call(rbind, d)

  gg <- ggplot(d) +
    geom_rect(xmin = 1 - .5, xmax = nc + .5, ymin = 1 - .5, ymax = nr + .5, color = "black", fill = tile.color, lwd = wall.size) +
    geom_segment(aes_string(x = "x0", y = "y0", xend = "x1", yend = "y1"), size = d$size, lineend = "square") +
    xlim(0.5, nc + .5) +
    ylim(0.5, nr + .5) +
    theme_void() +
    guides(size = "none") +
    theme(aspect.ratio = nr/nc)

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

plotGraph <- function(g, layout, labels = TRUE, edge.color = "black", vertex.color = "black", vertex.fill = "white", vertex.size = 15, vertex.curved = FALSE, vertex.label.color = "white", ...) {
  m <- layout
  d_n <- data.frame(x = m[,1], y = m[,2])
  el <- get.edgelist(g)
  n_i <- el[,1]
  n_j <- el[,2]
  d_e <- data.frame(source = n_i, target = n_j, x = m[n_i, 1], y = m[n_i, 2], xend = m[n_j, 1], yend = m[n_j, 2])
  if (vertex.curved)
    geom_edge <- geom_curve
  else
    geom_edge <- geom_segment
  gg <- ggplot(d_e, aes_string(x = "x", y = "y")) +
    geom_edge(aes_string(xend = "xend", yend = "yend"), color = edge.color, ...) +
    geom_point(aes_string(x = "x", y = "y"),
               data = d_n,
               shape = 21,
               size = vertex.size,
               color = vertex.color,
               fill = vertex.fill,
               stroke = 1
    ) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank()
    ) +
    labs(x = "", y = "")
  #
  if(labels) {
    l <- V(g)$label
    if(!is.null(l)) {
      gg <- gg + geom_text(aes_string(x = "x", y = "y", label = "l"), data = d_n, color = vertex.label.color)
    }
  }
  gg
}
