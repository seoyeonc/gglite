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
#' @param manual Manual scale definitions collected from user inputs.
#' @param aes_map Additional aesthetics to bind to the label column.
#' @return A ggplot2 layer object or wrapper with manual scale metadata.
#' @keywords internal
#' @noRd
make_geom <- function(df, geom_type, aes, args, manual = list(), aes_map = list()) {
  if (length(aes_map) > 0) {
    for (nm in names(aes_map)) {
      aes[[nm]] <- aes_map[[nm]]
    }
  }

  args1 <- list(data = df, mapping = aes)
  layer <- do.call(geom_type, c(args1, args))

  if (length(manual) == 0) {
    return(layer)
  }

  structure(list(layer = layer, manual = manual), class = "gglite_layer")
}

#' Extract labels from a prepared data frame
#'
#' @param df A data frame produced by [make_df()].
#' @return A character vector of labels or `NULL`.
#' @keywords internal
#' @noRd
extract_labels <- function(df) {
  if ("label" %in% names(df)) {
    return(df$label)
  }
  NULL
}

#' Reconcile manual aesthetic arguments with labels
#'
#' @param values User-supplied aesthetic values.
#' @param labels Character labels attached to the data.
#' @return A named vector suitable for manual scales.
#' @keywords internal
#' @noRd
prepare_manual_values <- function(values, labels) {
  if (is.null(values) || length(labels) == 0) {
    return(NULL)
  }

  levels <- unique(as.character(labels))
  if (length(levels) == 0) {
    return(NULL)
  }

  if (is.null(names(values))) {
    values <- rep(values, length.out = length(levels))
    return(stats::setNames(values, levels))
  }

  matched <- values[match(levels, names(values))]
  if (all(is.na(matched))) {
    matched <- rep(values, length.out = length(levels))
  } else {
    fallback <- values[which(!is.na(values))[1]]
    matched[is.na(matched)] <- fallback
  }

  stats::setNames(matched, levels)
}

#' Parse user arguments and extract manual aesthetic mappings
#'
#' @param labels Character labels associated with the data.
#' @param args List of arguments supplied by the user.
#' @return A list with updated `args`, `manual`, and `aes_map`.
#' @keywords internal
#' @noRd
check_args <- function(labels, args) {
  manual <- list()
  aes_map <- list()

  if (!is.null(labels)) {
    colour_keys <- intersect(names(args), c("col", "color", "colour"))
    if (length(colour_keys) > 0) {
      colour_value <- args[[colour_keys[1]]]
      args[colour_keys] <- NULL
      manual_colour <- prepare_manual_values(colour_value, labels)
      if (!is.null(manual_colour)) {
        manual$colour <- manual_colour
      }
    }

    fill_keys <- intersect(names(args), "fill")
    if (length(fill_keys) > 0) {
      fill_value <- args[[fill_keys[1]]]
      args[fill_keys] <- NULL
      manual_fill <- prepare_manual_values(fill_value, labels)
      if (!is.null(manual_fill)) {
        manual$fill <- manual_fill
      }
    }

    linetype_keys <- intersect(names(args), c("lty", "linetype"))
    if (length(linetype_keys) > 0) {
      linetype_value <- args[[linetype_keys[1]]]
      args[linetype_keys] <- NULL
      manual_linetype <- prepare_manual_values(linetype_value, labels)
      if (!is.null(manual_linetype)) {
        manual$linetype <- manual_linetype
        aes_map$linetype <- quote(label)
      }
    }
  }

  list(args = args, manual = manual, aes_map = aes_map)
}

#' Prepare geometry arguments with manual scale metadata
#'
#' @param df Prepared data frame.
#' @param args List of user-supplied arguments.
#' @return The result of [check_args()].
#' @keywords internal
#' @noRd
prepare_geom_args <- function(df, args) {
  labels <- extract_labels(df)
  check_args(labels, args)
}

#' Merge manual vectors from successive layers
#'
#' @param existing Previously stored manual values.
#' @param new New manual values to merge.
#' @return Combined named vector of manual values.
#' @keywords internal
#' @noRd
merge_manual_values <- function(existing, new) {
  if (is.null(existing)) {
    existing <- c()
  }
  existing[names(new)] <- new
  existing
}

#' Build a manual scale for a given aesthetic
#'
#' @param aesthetic One of `colour`, `color`, `fill`, or `linetype`.
#' @param values Named vector of aesthetic values.
#' @return A ggplot scale object flagged as gglite-generated.
#' @keywords internal
#' @noRd
new_manual_scale <- function(aesthetic, values, guide = NULL, override = NULL) {
  scale_fun <- switch(aesthetic,
    colour = ggplot2::scale_colour_manual,
    color = ggplot2::scale_colour_manual,
    fill = ggplot2::scale_fill_manual,
    linetype = ggplot2::scale_linetype_manual,
    stop(sprintf("Unsupported aesthetic '%s'", aesthetic))
  )
  args <- list(values = values)
  if (!is.null(override) && length(override) > 0) {
    args$guide <- ggplot2::guide_legend(override.aes = override)
  } else if (!is.null(guide)) {
    args$guide <- guide
  }
  scale <- do.call(scale_fun, args)
  scale$gglite_generated <- TRUE
  scale
}

#' Remove previously added manual scales for an aesthetic
#'
#' @param plot A ggplot object.
#' @param aesthetic Aesthetic name.
#' @return The plot without the generated scale.
#' @keywords internal
#' @noRd
remove_manual_scale <- function(plot, aesthetic) {
  if (length(plot$scales$scales) == 0) {
    return(plot)
  }

  keep <- vapply(
    plot$scales$scales,
    function(scale) {
      !(identical(scale$gglite_generated, TRUE) && aesthetic %in% scale$aesthetics)
    },
    logical(1)
  )

  plot$scales$scales <- plot$scales$scales[keep]
  plot
}

#' @export
ggplot_add.gglite_layer <- function(object, plot, object_name) {
  plot <- ggplot2::ggplot_add(object$layer, plot, object_name)

  manual <- object$manual
  if (length(manual) == 0) {
    return(plot)
  }

  if (!is.null(manual$linetype)) {
    values <- merge_manual_values(attr(plot, "gglite_manual_linetype"), manual$linetype)
    attr(plot, "gglite_manual_linetype") <- values
    plot <- remove_manual_scale(plot, "linetype")
    plot$scales$add(new_manual_scale("linetype", values, guide = "none"))
  }

  linetype_values <- attr(plot, "gglite_manual_linetype")

  if (!is.null(manual$colour)) {
    values <- merge_manual_values(attr(plot, "gglite_manual_colour"), manual$colour)
    attr(plot, "gglite_manual_colour") <- values
    override <- NULL
    if (!is.null(linetype_values)) {
      override_lty <- unname(linetype_values[names(values)])
      if (any(is.na(override_lty))) {
        fallback <- ggplot2::GeomPath$default_aes$linetype
        override_lty[is.na(override_lty)] <- fallback
      }
      override <- list(linetype = override_lty)
    }
    plot <- remove_manual_scale(plot, "colour")
    plot$scales$add(new_manual_scale("colour", values, override = override))
  }

  if (!is.null(manual$fill)) {
    values <- merge_manual_values(attr(plot, "gglite_manual_fill"), manual$fill)
    attr(plot, "gglite_manual_fill") <- values
    plot <- remove_manual_scale(plot, "fill")
    plot$scales$add(new_manual_scale("fill", values))
  }

  plot
}
