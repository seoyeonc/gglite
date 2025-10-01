#' Set figure sizing options for interactive plotting
#'
#' This helper adjusts the repr options used by RMarkdown and notebook front ends
#' so that ggplot output renders with consistent dimensions.
#'
#' @param width Numeric width in inches. Defaults to 4.
#' @param height Numeric height in inches. Defaults to 3.
#' @param res Resolution in dots per inch (DPI). Defaults to 300.
#' @return Invisibly returns the previous `repr.plot.*` options.
#' @examples
#' figsize(width = 6, height = 4)
#' @export
figsize <- function(width = 4, height = 3, res = 300) {
  old <- options(
    repr.plot.width = width,
    repr.plot.height = height,
    repr.plot.res = res
  )
  invisible(old)
}
