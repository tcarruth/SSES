% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Landscape_plotting.R
\name{UTplot}
\alias{UTplot}
\title{Plot a map of landscape effort}
\usage{
UTplot(
  ED,
  aclassnam,
  popnams,
  lonlim,
  latlim,
  obj,
  region_list,
  cex.axis = 0.8
)
}
\arguments{
\item{ED}{an object of class 'Landscape'}

\item{lonlim}{vector 2 long, the y axis, longitude range of the plot}

\item{latlim}{vector 2 long, the x axis, latitude range of the plot}

\item{maxps}{real number, maximim size of points}

\item{nman}{positive integer, the number of managmeent options to plot}

\item{nsim}{positive integer, the number of simulations to plot}

\item{nice}{logical, should a word cloud arrangement of lake labels be used (can be unstable)?}

\item{dens}{logical, should effort density be plotted?}

\item{U}{logical, should utility be plotted instead of effort?}

\item{lcex}{the label font size}

\item{pcex}{the point size}

\item{lpick}{vector of either lake names or their index number, to be highlighted}
}
\description{
Plot a map of landscape effort
}
\author{
T. Carruthers
}
