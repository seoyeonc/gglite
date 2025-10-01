% Generated manually for one-variable (continuous) geoms
\name{histogram}
\alias{histogram}
\alias{density}
\alias{qq}
\alias{qq_line}
\title{One-variable (continuous) distribution helpers}
\description{
Wrappers that turn numeric vectors into layers for the one-variable continuous
geoms (histogram, density, quantile-quantile) described in Posit's guide.
}
\usage{
histogram(y, label = NULL, ...)
density(y, label = NULL, ...)
qq(y, label = NULL, ...)
qq_line(y, label = NULL, ...)
}
\arguments{
  \item{y}{Numeric vector containing the measurements to display.}
  \item{label}{Optional labels identifying groups within \code{y}.}
  \item{...}{Additional arguments for the corresponding geom.}
}
\value{
Each helper returns a ggplot2 layer.
}
\details{
\itemize{
  \item \code{histogram} calls \code{ggplot2::geom_histogram()} with density scaling.
  \item \code{density} calls \code{ggplot2::geom_density()} with default alpha.
  \item \code{qq} calls \code{ggplot2::geom_qq()} for sample quantiles.
  \item \code{qq_line} calls \code{ggplot2::geom_qq_line()} to add references.
}
}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gglite() + histogram(rnorm(200))
  gglite() + density(rnorm(200))
}
}
