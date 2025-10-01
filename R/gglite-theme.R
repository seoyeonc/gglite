#' Create a gglite themed plot
#'
#' This is a thin wrapper around [ggplot2::ggplot()] that initialises plots with
#' a clean black-and-white theme and consistent margins.
#'
#' @inheritParams ggplot2::ggplot
#' @return A ggplot object with the gglite defaults applied.
#' @seealso [ggplot2::ggplot()]
#' @examples
#' gglite(mtcars, ggplot2::aes(wt, mpg)) + line()
#' @export
gglite <- function(...) {
  ggplot2::ggplot(...) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank()) +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) +
    ggplot2::theme(plot.title = ggplot2::element_text(
      size = ggplot2::rel(1), lineheight = 0.9, face = "bold"
    )) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(3, 3, 0, 0), "mm")) +
    ggplot2::labs(x = NULL, y = NULL)
}
