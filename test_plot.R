library(igraph)

nr <- 5
nc <- 5

g <- graph.lattice(c(nr,nc))
l <- layout.grid(g)
plot(g, layout = l)

s <- mst(g, weights = rnorm(ecount(g)))
plot(s, layout = l)

plot(0, xlim = c(0,nc + 1), ylim = c(0,nr + 1), xlab = "j", ylab = "i")
rect(xleft = 1 - .5, ybottom = 1 - .5, xright = nc + .5, ytop = nr + .5, lwd = 5, border = "black")
for(i in 1:nr) {
  for(j in 1:nc) {
    #rect(xleft = j - .5, ybottom = i - .5, xright = j + .5, ytop = i + .5)
    text(x = j, y = i, labels = (i-1) * nc  + j)
  }
}
for(i in 1:nr) {
  for(j in 2:nc) {
    i1 <- i2 <- i
    j1 <- j - 1
    j2 <- j
    v1 <- (i1 - 1) * nc + j1
    v2 <- (i2 - 1) * nc + j2
    message(v1, ":", v2)
    if(!are_adjacent(s, v1, v2))
      lines(x = c(j - .5, j - .5), y = c(i - .5, i + .5), col = "black", lwd = 5)
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
    if(!are_adjacent(s, v1, v2))
      lines(x = c(j - .5, j + .5), y = c(i - .5, i - .5), col = "black", lwd = 5)
  }
}


plotMaze <- function(g, nrow, ncol) {
  d <- list()
  for(i in 1:nr) {
    for(j in 2:nc) {
      i1 <- i2 <- i
      j1 <- j - 1
      j2 <- j
      v1 <- (i1 - 1) * nc + j1
      v2 <- (i2 - 1) * nc + j2
      #message(v1, ":", v2)
      if(!are_adjacent(s, v1, v2))
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
      if(!are_adjacent(s, v1, v2))
        d[[length(d) + 1]] <- data.frame(x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i - .5)
        #lines(x = c(j - .5, j + .5), y = c(i - .5, i - .5), col = "black", lwd = 5)
    }
  }
  d <- do.call(rbind, d)
  ggplot(d) + geom_rect(xmin = 1 - .5, xmax = nc + .5, ymin = 1 - .5, ymax = nr + .5, color = "black", fill = "white", lwd = 5) + geom_segment(aes(x = x0, y = y0, xend = x1, yend = y1), size = 5, lineend = "square") + xlim(0,nc + 1) + ylim(0,nr + 1) + theme(aspect.ratio = 1)
}

plotMaze_ori <- function(g, layout) {
  m <- layout
  d_n <- data.frame(x = m[,1], y = m[,2])
  el <- get.edgelist(g)
  n_i <- el[,1]
  n_j <- el[,2]
  d_e <- data.frame(source = n_i, target = n_j, x = m[n_i, 1], y = m[n_i, 2], xend = m[n_j, 1], yend = m[n_j, 2])

  gg <- ggplot(d_e,aes(x=x, y=y)) + geom_segment(aes(xend=xend,yend=yend), color = "black") + geom_point(aes(x=x,y=y), data = d_n, shape = 22, size = 50, color = "red", fill = "transparent", stroke = 1) + geom_point(aes(x=x,y=y), data = d_n, shape = 21, size = 5, color = "red", fill = "black", stroke = 1) + theme(aspect.ratio = 1) + xlab("") + ylab("")
  #
  gg
}
