% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DummyDuplicated.R
\name{DummyDuplicated}
\alias{DummyDuplicated}
\title{Duplicated columns in dummy matrix}
\usage{
DummyDuplicated(x, idx = FALSE, rows = FALSE, rnd = FALSE)
}
\arguments{
\item{x}{A matrix}

\item{idx}{Indices returned when TRUE}

\item{rows}{Duplicated rows instead when TRUE}

\item{rnd}{Algorithm based on cross product with random numbers when TRUE (dummy matrix not required)}
}
\value{
Logical vectors specifying duplicated columns or vector of indices (first match)
}
\description{
The algorithm is based on \code{crossprod(x)} or \code{crossprod(x, u)} where \code{u} is a vector of random numbers
}
\details{
The efficiency of the default algorithm depends on the sparsity of \code{crossprod(x)}.
The random values are generated locally within the function without affecting the random value stream in R.
}
\examples{
x <- cbind(1, rbind(diag(2), diag(2)), diag(4)[, 1:2])
z <- Matrix::Matrix(x[c(1:4, 2:3), c(1, 2, 1:5, 5, 2)])

DummyDuplicated(z)
which(DummyDuplicated(z, rows = TRUE))

# Four ways to obtain the same result
DummyDuplicated(z, idx = TRUE)
DummyDuplicated(z, idx = TRUE, rnd = TRUE)
DummyDuplicated(Matrix::t(z), idx = TRUE, rows = TRUE)
DummyDuplicated(Matrix::t(z), idx = TRUE, rows = TRUE, rnd = TRUE)

# The unique values in four ways 
which(!DummyDuplicated(z), )
which(!DummyDuplicated(z, rnd = TRUE))
which(!DummyDuplicated(Matrix::t(z), rows = TRUE))
which(!DummyDuplicated(Matrix::t(z), rows = TRUE, rnd = TRUE))
}
\author{
Øyvind Langsrud
}
