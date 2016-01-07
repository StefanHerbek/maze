library(igraph)


g <- graph.lattice(c(3,3))
l <- layout.grid(g)
plot(g, layout = l)

s <- mst(g, weights = rnorm(ecount(g)))
plot(s, layout = l)
plot(s, layout = l, vertex.shape = "square", vertex.size = 100, vertex.frame.color = "transparent")

se <- sample(E(g), 1)
if(any(degree(g)[g[[se]][[1]]] < 2)) next

stop_ <- FALSE
while(! stop_) {
  d <- degree(g)
  if(all(d < 3)) stop_ <- TRUE
  else {
    s <- induced_subgraph(g, V(g)[ d > 2 ])
    es <- sample(E(s), 1)
    g <- delete_edges(g, es)
  }
}
plot(g)

plot(minimum.spanning.tree(g))


w <- 1/sample(1:12)
s <- minimum.spanning.tree(g, weights = w, algorithm = "prim")
data.frame(edges = E(g), weigth = w)
as.character(E(g))
plot()

#
nr <- 10
nc <- 10
g <- graph.lattice(c(nr, nc))
nv <- vcount(g)
ne <- ecount(g)
plot(g)
w <- 1/sample(1:ne)

s <- minimum.spanning.tree(g, weights = w, algorithm = "prim")
plot(s)

plot(NA, ylim = c(1, nr), xlim = c(1, nc))
lines(c(1,1),c(1,2), lwd = 3)
lines(c(1,1),c(1,3), lwd = 3)
lines(c(1,2),c(3,3), lwd = 3)
lines(c(2,3),c(3,3), lwd = 3)
sl <- get.edgelist(s)

for (v in V(g)) { # iterate over all lattice vertex.
  n <- neighbors(g, v) # pick neighbors in lattice graph.
  for (nv in n) { # iterate over all neighbors.

  }
}


###
###
###
nr <- 4
nc <- 4
g <- graph.lattice(c(nr, nc))
nv <- vcount(g)
ne <- ecount(g)
plot(g)
w <- 1/sample(1:ne)

set.seed(123)
s <- minimum.spanning.tree(g, weights = w, algorithm = "prim")
plot(s)
#plot(s, vertex.size = 2, vertex.label = NA, layout = layout.fruchterman.reingold)

plot(NA, ylim = c(1, nr), xlim = c(1, nc))
abline(h = 1:10, v = 1:10, col = "grey")
vl <- get.edgelist(g)[1:20,,drop = FALSE]
for (i in 1:nrow(vl)) {
  v1 <- vl[i, ][1]
  v2 <- vl[i, ][2]
  if (!are_adjacent(s, v1, v2)) {
    x1 <- v1 %/% nc + 1
    y1 <- v1 %% nr + 1
    x2 <- v2 %/% nc + 1
    y2 <- v2 %% nr + 1
    d1 <- abs(x1 - x2)
    d2 <- abs(y1 - y2)
    print(v1)
    print(v2)
    print(x1)
    print(y1)
    print(x2)
    print(y2)
    print(d1)
    print(d2)
    print("--------")
    lines(c(x1, x1 + d1), c(y1, y1 + d2), lwd = 3)
  }
}
