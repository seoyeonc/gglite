# gglite

`gglite` is a lightweight collection of ggplot2 helpers inspired by Posit's *Data Visualization* guide. The package provides intuitive wrappers for frequently used geoms and a starter theme (`gglite()`) so exploratory plots share a consistent style.

## Installation

```r
# install.packages("devtools") if needed
devtools::install_github("seoyeonc/gglite")
```

## Quick start

```r
library(gglite)
library(ggplot2)

gglite(mtcars, aes(wt, mpg)) +
  line() +
  point()

# One-dimensional distributions
gglite() +
  histogram(list(sample_a = rnorm(200), sample_b = rnorm(200, mean = 0.5)))

# Comparison plots
gglite() +
  col(matrix(rnorm(12), nrow = 4, dimnames = list(NULL, c("A", "B", "C"))))
```

## Function overview
- **Theme starter**: `gglite()`
- **One variable (continuous)**: `histogram()`, `density()`, `qq()`, `qq_line()`
- **Two variables (continuous × continuous)**: `point()`, `line()`, `smooth()`, `step()`
- **Two variables (discrete × continuous)**: `col()`, `boxplot()`, `violin()`, `jitter()`

These groupings follow the layout of Posit's *Data Visualization with ggplot2* cheat sheet. Use `?line`, `?histogram`, or `?gglite-package` inside R for detailed help topics.

## Reference

Posit (2022). *Data Visualization with ggplot2*.
