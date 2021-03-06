% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/waic.R
\name{waic}
\alias{waic}
\title{Function for finding WAIC}
\source{
from Vehrari and Gelman (2014):
\url{http://www.stat.columbia.edu/~gelman/research/unpublished/waic_stan.pdf}
}
\usage{
waic(stanfit)
}
\arguments{
\item{stanfit}{a \code{stanfit} object with estimated \code{log_link}s.}
}
\description{
Function for finding WAIC
}

