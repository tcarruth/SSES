% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Landscape_stochasticity.R, R/Misc_exported.R
\name{mconv}
\alias{mconv}
\title{Convert a desired mean and standard deviation into a log normal mean}
\usage{
mconv(m, sd)

mconv(m, sd)
}
\arguments{
\item{m}{mean value of transformed variable}

\item{sd}{standard deviation of transformed variable}
}
\description{
Convert a desired mean and standard deviation into a log normal mean

Get lognormal mean parameter for a variable with m and sd transformed values
}
\examples{
\dontrun{
mconv(m=1,sd=0.5)
}
}
\author{
T. Carruthers

T. Carruthers
}
