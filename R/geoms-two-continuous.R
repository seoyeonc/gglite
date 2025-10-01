#' Add a line layer using compact inputs
#'
#' This helper accepts vectors or matrices and converts them to tidy data that
#' can be plotted with [ggplot2::geom_line()].
#'
#' @param x Numeric values on the x-axis. When `y` is omitted the sequence
#'   `seq_along(y)` is used.
#' @param y Numeric values on the y-axis. Can be `NULL` when `x` is a matrix or
#'   vector of y-values.
#' @param label Optional grouping labels. Can recycle to match the data length.
#' @param ... Additional arguments passed to [ggplot2::geom_line()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + line(1:10, rnorm(10))
#' @export
line <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_line
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df, geom_type, aes, args)
}


#' Add points to a gglite plot
#'
#' @inheritParams line
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + point(1:5, 5:1)
#' @export
point <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_point
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df, geom_type, aes, args)
}


#' Add a smooth curve with gglite defaults
#'
#' @inheritParams line
#' @param ... Additional arguments passed to [ggplot2::geom_smooth()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + smooth(1:10, (1:10)^2)
#' @export
smooth <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_smooth
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df, geom_type, aes, args)
}


#' Draw a step chart
#'
#' @inheritParams line
#' @param ... Additional arguments passed to [ggplot2::geom_step()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + step(1:10, cumsum(rnorm(10)))
#' @export
step <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_step
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df, geom_type, aes, args)
}
