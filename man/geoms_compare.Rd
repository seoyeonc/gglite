% Generated manually with aliases for comparison geoms
\name{col}
\alias{col}
\alias{boxplot}
\alias{violin}
\title{Comparison geometry helpers}
\description{
Wrappers that simplify building comparison charts (columns, boxplots, violins)
using the grammar from Posit's data visualisation reference.
}
\usage{
col(x, y = NULL, label = NULL, ...)
boxplot(x, y = NULL, label = NULL, ...)
violin(x, y = NULL, label = NULL, ...)
}
\arguments{
  \item{x}{Numeric or categorical values for the x-axis.}
  \item{y}{Numeric values for the y-axis, or \code{NULL} to treat \code{x} as \code{y}.}
  \item{label}{Grouping labels recycled to match the data.}
  \item{...}{Further arguments passed to the respective geom.}
}
\value{
Each helper returns a ggplot2 layer.
}
\details{
\itemize{
  \item \code{col} calls \code{ggplot2::geom_col()} with dodged positioning.
  \item \code{boxplot} calls \code{ggplot2::geom_boxplot()} for distribution comparison.
  \item \code{violin} calls \code{ggplot2::geom_violin()} with area scaling.
}
}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gglite() + col(matrix(rnorm(6), nrow = 3))
  gglite() + boxplot(y = rnorm(50))
}
}
