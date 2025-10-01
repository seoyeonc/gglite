% Generated manually for continuous × continuous geoms
\name{line}
\alias{line}
\alias{point}
\alias{smooth}
\alias{step}
\title{Continuous × continuous geometry helpers}
\description{
Wrappers that convert compact vector or matrix inputs into tidy data for the
continuous × continuous geoms outlined in Posit's data visualisation guide.
}
\usage{
line(x, y = NULL, label = NULL, ...)
point(x, y = NULL, label = NULL, ...)
smooth(x, y = NULL, label = NULL, ...)
step(x, y = NULL, label = NULL, ...)
}
\arguments{
  \item{x}{Numeric values for the x-axis. When omitted, a sequence is generated.}
  \item{y}{Numeric values for the y-axis or \code{NULL}.}
  \item{label}{Optional grouping labels recycled to match the data length.}
  \item{...}{Additional arguments passed to the underlying geom.}
}
\value{
Each helper returns a ggplot2 layer.
}
\details{
\itemize{
  \item \code{line} calls \code{ggplot2::geom_line()} for trend lines.
  \item \code{point} calls \code{ggplot2::geom_point()} for scatter plots.
  \item \code{smooth} calls \code{ggplot2::geom_smooth()} for smoothed trends.
  \item \code{step} calls \code{ggplot2::geom_step()} for stepped trends.
}
}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gglite() + line(1:10, cumsum(rnorm(10)))
  gglite() + point(1:10, rnorm(10))
}
}
