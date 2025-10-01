% Generated manually
\name{gglite}
\alias{gglite}
\title{Create a gglite themed plot}
\description{
Initialise a \pkg{ggplot2} plot with a clean black-and-white theme and consistent margins.
}
\usage{
gglite(...)
}
\arguments{
  \item{...}{Arguments forwarded to \code{ggplot2::ggplot()}.}
}
\value{
A ggplot object with gglite defaults applied.
}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gglite(mtcars, ggplot2::aes(wt, mpg)) + line(1:5, 5:1)
}
}
\seealso{\code{ggplot2::ggplot()}, \code{figsize()}}
