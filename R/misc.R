maze_graph <- function(nrow = 1, ncol = 1, weight.fun = "rnorm") {
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



