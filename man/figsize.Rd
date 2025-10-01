% Generated manually
\name{figsize}
\alias{figsize}
\title{Set figure sizing options for interactive plotting}
\description{
Adjust notebook repr options so ggplot output renders with consistent dimensions.
}
\usage{
figsize(width = 4, height = 3, res = 300)
}
\arguments{
  \item{width}{Width in inches.}
  \item{height}{Height in inches.}
  \item{res}{Resolution in dots per inch.}
}
\value{
Invisible list of previous plotting options.
}
\examples{
figsize(width = 6, height = 4)
}
