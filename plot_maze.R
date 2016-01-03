
plotMaze <- function(g, nrow, ncol) {
  nc <- ncol
  nr <- nrow
  d <- list()
  for(i in 1:nr) {
    for(j in 2:nc) {
      i1 <- i2 <- i
      j1 <- j - 1
      j2 <- j
      v1 <- (i1 - 1) * nc + j1
      v2 <- (i2 - 1) * nc + j2
      #message(v1, ":", v2)
      if(!are_adjacent(g, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j - .5, y0 = i - .5, y1 = i + .5)
      #lines(x = c(j - .5, j - .5), y = c(i - .5, i + .5), col = "black", lwd = 5)
    }
  }
  for(i in 2:nr) {
    for(j in 1:nc) {
      i1 <- i - 1
      i2 <- i
      j1 <- j2 <- j
      v1 <- (i1 - 1) * nc + j1
      v2 <- (i2 - 1) * nc + j2
      message(v1, ":", v2)
      if(!are_adjacent(g, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i - .5)
      #lines(x = c(j - .5, j + .5), y = c(i - .5, i - .5), col = "black", lwd = 5)
    }
  }
  d <- do.call(rbind, d)
  ggplot(d) + geom_rect(xmin = 1 - .5, xmax = nc + .5, ymin = 1 - .5, ymax = nr + .5, color = "black", fill = "white", lwd = 5) + geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), size = 5, lineend = "square") + xlim(0,nc + 1) + ylim(0,nr + 1) + theme(aspect.ratio = 1) + theme_void()
}
