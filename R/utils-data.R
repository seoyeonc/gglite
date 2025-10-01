#' Build a data frame from vector or matrix inputs
#'
#' @param x Numeric or categorical values for the x-axis. When `y` is omitted
#'   this argument supplies the y-values.
#' @param y Optional numeric or categorical values for the y-axis.
#' @return A data frame suitable for use in ggplot2 layers.
#' @keywords internal
#' @noRd
make_df <- function(x, y = NULL) {
  if (is.null(y)) {
    y <- x
    if (!is.vector(y)) {
      x <- seq_len(dim(y)[1])
    } else {
      x <- seq_along(y)
    }
  }

  if (!is.vector(y)) {
    dfx <- data.frame(x)
    dfy <- data.frame(y)
    df <- cbind(dfx, dfy)
    df <- tidyr::pivot_longer(df, cols = colnames(dfy),
                              names_to = "label", values_to = "y")
  } else {
    df <- data.frame(x = x, y = y)
  }
  df
}


#' Apply labels to a data frame of aesthetic mappings
#'
#' @param df A data frame produced by [make_df()].
#' @param label Optional character labels.
#' @return The input data frame with a `label` column if appropriate.
#' @keywords internal
#' @noRd
apply_label <- function(df, label) {
  if (is.null(label)) {
    return(df)
  }

  n <- nrow(df)
  if (n == 0) {
    return(df)
  }

  if ("label" %in% names(df)) {
    current <- as.character(df$label)
    unique_current <- unique(current)

    if (length(label) == length(unique_current)) {
      mapping <- stats::setNames(as.character(label), unique_current)
      df$label <- mapping[current]
      return(df)
    }
  }

  if (length(label) == n) {
    df$label <- label
  } else {
    df$label <- rep(label, length.out = n)
  }
  df
}


#' Create a ggplot2 geometry layer
#'
#' @param df Prepared data frame.
#' @param geom_type The ggplot2 geometry constructor.
#' @param aes A call to [ggplot2::aes()].
#' @param args Additional arguments supplied by the user.
#' @return A ggplot2 layer object.
#' @keywords internal
#' @noRd
make_geom <- function(df, geom_type, aes, args) {
  args1 <- list(data = df, mapping = aes)
  do.call(geom_type, c(args1, args))
}
