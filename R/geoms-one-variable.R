#' Draw a histogram with density scaling
#'
#' @param y Numeric vector. If named, names are ignored.
#' @param label Optional labels identifying groups.
#' @param ... Additional arguments passed to [ggplot2::geom_histogram()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + histogram(rnorm(100))
#' @export
histogram <- function(y, label = NULL, ...) {
  args <- list(...)
  df <- make_df(y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_histogram
  aes <- ggplot2::aes(x = y, y = ggplot2::after_stat(density), fill = label)
  if (is.null(args$alpha)) {
    args$alpha <- 0.5
  }
  if (is.null(args$position)) {
    args$position <- "identity"
  }
  processed <- prepare_geom_args(df, args)
  args <- processed$args
  make_geom(df, geom_type, aes, args, processed$manual, processed$aes_map)
}


#' Draw a kernel density estimate
#'
#' @inheritParams histogram
#' @param ... Additional arguments passed to [ggplot2::geom_density()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + density(rnorm(200))
#' @export
density <- function(y, label = NULL, ...) {
  args <- list(...)
  df <- make_df(y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_density
  aes <- ggplot2::aes(x = y, fill = label, col = label)
  if (is.null(args$alpha)) {
    args$alpha <- 0.25
  }
  processed <- prepare_geom_args(df, args)
  args <- processed$args
  make_geom(df, geom_type, aes, args, processed$manual, processed$aes_map)
}


#' Draw a Q-Q plot
#'
#' @inheritParams histogram
#' @param ... Additional arguments passed to [ggplot2::geom_qq()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + qq(rnorm(100))
#' @export
qq <- function(y, label = NULL, ...) {
  args <- list(...)
  df <- data.frame(y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_qq
  aes <- ggplot2::aes(sample = y, col = label)
  processed <- prepare_geom_args(df, args)
  args <- processed$args
  make_geom(df, geom_type, aes, args, processed$manual, processed$aes_map)
}


#' Add a Q-Q reference line
#'
#' @inheritParams histogram
#' @param ... Additional arguments passed to [ggplot2::geom_qq_line()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + qq(rnorm(100)) + qq_line(rnorm(100))
#' @export
qq_line <- function(y, label = NULL, ...) {
  args <- list(...)
  df <- data.frame(y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_qq_line
  aes <- ggplot2::aes(sample = y, col = label)
  processed <- prepare_geom_args(df, args)
  args <- processed$args
  make_geom(df, geom_type, aes, args, processed$manual, processed$aes_map)
}
