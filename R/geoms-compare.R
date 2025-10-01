#' Draw columns for grouped comparisons
#'
#' @inheritParams line
#' @param ... Additional arguments passed to [ggplot2::geom_col()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + col(matrix(rnorm(6), nrow = 3))
#' @export
col <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  df$x <- as.factor(df$x)
  geom_type <- ggplot2::geom_col
  aes <- ggplot2::aes(x = x, y = y, fill = label)
  if (is.null(args$position)) {
    args$position <- "dodge"
  }
  make_geom(df, geom_type, aes, args)
}


#' Draw a boxplot with sensible defaults
#'
#' @inheritParams line
#' @param ... Additional arguments passed to [ggplot2::geom_boxplot()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + boxplot(y = rnorm(50), label = rep("sample", 50))
#' @export
boxplot <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  if (is.null(y)) {
    y <- x
    x <- 0
  }
  df <- make_df(x, y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_boxplot
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df, geom_type, aes, args)
}


#' Draw a violin plot with area scaling
#'
#' @inheritParams line
#' @param ... Additional arguments passed to [ggplot2::geom_violin()].
#' @return A ggplot2 layer.
#' @examples
#' gglite() + violin(y = rnorm(100), label = rep("sample", 100))
#' @export
violin <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  if (is.null(y)) {
    y <- x
    x <- 0
  }
  df <- make_df(x, y)
  df <- apply_label(df, label)
  geom_type <- ggplot2::geom_violin
  aes <- ggplot2::aes(x = x, y = y, fill = label, color = label)
  if (is.null(args$alpha)) {
    args$alpha <- 0.5
  }
  if (is.null(args$scale)) {
    args$scale <- "area"
  }
  make_geom(df, geom_type, aes, args)
}
