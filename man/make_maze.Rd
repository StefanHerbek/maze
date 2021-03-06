% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{make_maze}
\alias{make_maze}
\title{Generate a square maze}
\usage{
make_maze(nrow = 1, ncol = 1, weight.fun = "rnorm")
}
\arguments{
\item{nrow}{number of rows.}

\item{ncol}{number of columns.}

\item{weight.fun}{weight function.}
}
\value{
An object of class igraph. A minimum spaning tree.
}
\description{
Generate a square maze of the specified dimentions using a specified weight function.
}
\examples{
make_maze(2, 2)
}
