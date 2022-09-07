\name{mvrnormArma}
\alias{mvrnormArma}
\title{DataCpp generate multivariate normal data in C++}
\description{
  This mini-package is only created to access the \code{mvrnormArma}-function in parallel threads, using the \code{furrr} package in \code{R}.
}
\usage{
mvrnormArma(n, mu, sigma)
}
\arguments{
  \item{n}{A scalar indicating the sample size.}
  \item{mu}{A vector with the means of the variables.}
  \item{sigma}{Covariance matrix of the variables.}
}
\value{
  \code{mvrnormArma()} returns a matrix containing multivariate normal data for \code{n} observations, with means \code{mu} and covariance \code{sigma}.
}
\details{
  The only purpose of this package is \code{R} recognizing the \code{mvrnormArma}-function when running the simulations in parallel computing clusters.
}
\references{
  See the documentation for Armadillo, and RcppArmadillo, for more details.
}
\examples{
  n     <- 100
  mu    <- c(0, 0, 0)
  sigma <- matrix(c(1, 0.3, 0.3,
                    0.3, 1, 0.3,
                    0.3, 0.3, 1),
                  nrow = 3, byrow = T)
  
  X <- mvrnormArma(n, mu, sigma)
}
\author{Thom Volker}

