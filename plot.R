
plotMaze <- function(g, nrow, ncol, wall.size = 5.0, tile.show = FALSE, tile.size = 1, tile.color = "white", tile.number.show = FALSE, tile.number.size = 5, path.show = FALSE, path.start = 1, path.end = nrow*ncol) {
  nc <- ncol
  nr <- nrow
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
  gg <- ggplot(d) + geom_rect(xmin = 1 - .5, xmax = nc + .5, ymin = 1 - .5, ymax = nr + .5, color = "black", fill = tile.color, lwd = wall.size) + geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), size = d$size, lineend = "square") + xlim(0,nc + 1) + ylim(0,nr + 1) + theme_void() + guides(size = "none")

  if (tile.number.show) {
    d <- expand.grid(x = 1:nc, y = 1:nr)
    d$index <- 1:(nc * nr)
    gg <- gg + geom_text(data = d, mapping = aes(x=x,y=y,label=index), size = tile.number.size)
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
    gg <- gg + geom_path(data = p, mapping = aes(x = x, y = y), color = "red", lwd = 1, lineend = "round")
  }
  gg
}
