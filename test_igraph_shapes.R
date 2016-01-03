library(igraph)

g1 <- graph.lattice(c(10,10))
l <- layout.grid(g1)
plot(g1, layout = l, edge.width = 5)
g <- g1

v <- unlist(sapply(c(0, 10, 20), function(x) x + 1:3, simplify = F))

v <- unlist(sapply(c(0, 10, 20), function(x) {x + c(35,45,55)}, simplify = F))

g <- delete.vertices(g1, v)
plot(g, layout = l[-1 * v,], edge.width = 5)

w <- runif(ecount(g))
w <- rnorm(ecount(g))
s <- minimum.spanning.tree(g, weights = w)
plot(s, layout = l, edge.width = 5)

plot(s, layout = l[-1 * v,], edge.width = 5)


###
m1 <- read.table("map1.txt")
m1 <- as.matrix(m1)
g1 <- graph.lattice(c(10,10))
l <- layout.grid(g1)
plot(g1, layout = l, edge.width = 5)

nr <- nrow(m1)
nc <- ncol(m1)
v <- NULL
for (i in 1:nr) {
  for(j in 1:nc) {
    if (m1[i,j] == 0) {
      v <- c(v, nr * (j - 1) + i)
    }
  }
}
v

g <- delete.vertices(g1, v)
plot(g, layout = l[-1 * v,], edge.width = 5)

w <- rnorm(ecount(g))
s <- minimum.spanning.tree(g, weights = w)
plot(s, layout = l[-1 * v,], edge.width = 5)


##
library(png)
m <- readPNG("ubuntu.png")
m <- readPNG("twiter.png")
is(m)
dim(m)
dim(m[,,1])
m <- m[,,1]
dim(m)
image(t(m), col = c("white", "black"))


g1 <- graph.lattice(c(nrow(m),ncol(m)))
l <- layout.grid(g1)
plot(g1, layout = l, edge.width = 5, vertex.label = NA, vertex.size = 2, margin = -0.2)

nr <- nrow(m)
nc <- ncol(m)
v <- NULL
for (i in 1:nr) {
  for(j in 1:nc) {
    if (m[i,j] == 0) {
      v <- c(v, nr * (j - 1) + i)
    }
  }
}
v

g <- delete.vertices(g1, v)
plot(g, layout = l[-1 * v,], edge.width = 5, vertex.label = NA, vertex.size = 2, margin = -0.2)

w <- rnorm(ecount(g))
s <- minimum.spanning.tree(g, weights = w)
plot(s, layout = l[-1 * v,], edge.width = 5, vertex.label = NA, vertex.size = 2, margin = -0.2)
