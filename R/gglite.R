figsize <- function(width=4,height=3,res=300){
    options(repr.plot.width=width,repr.plot.height=height,repr.plot.res=res)
}

gglite <- function(...){
    ggplot2::ggplot(...)+
    ggplot2::theme_bw()+
    ggplot2::theme(panel.border=ggplot2::element_blank(),axis.line=ggplot2::element_line(colour="black"))+
    ggplot2::theme(axis.title.x=ggplot2::element_text(size=ggplot2::rel(1),lineheight=0.9,face="bold.italic"))+
    ggplot2::theme(axis.title.y=ggplot2::element_text(size=ggplot2::rel(1),lineheight=0.9,face="bold.italic"))+
    ggplot2::theme(plot.title=ggplot2::element_text(size=ggplot2::rel(1),lineheight=0.9,face="bold"))+
    ggplot2::theme(plot.margin = ggplot2::unit(c(3,3,0,0), "mm"))
}

make_df <- function(x, y = NULL) {
  if (is.null(y)) {
    y=x
    if (!is.vector(y)) {
      x = 1:dim(y)[1]
    } else {
      x = 1:length(y)
    }
  }
  if (!is.vector(y)) {
      dfx = data.frame(x)
      dfy = data.frame(y)
      df = cbind(dfx,dfy)
      df = tidyr::pivot_longer(df,cols = colnames(dfy), names_to = "label", values_to = "y")
  } else {
      df = data.frame(x=x,y=y)
  }
  return(df)
}

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

    fill_keys <- intersect(names(args), c("fill"))
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

extract_labels <- function(df) {
  if ("label" %in% names(df)) {
    return(df$label)
  }
  NULL
}

make_geom <- function(df,geom_type,aes,args,manual=list(),aes_map=list()) {
  if (length(aes_map) > 0) {
    for (nm in names(aes_map)) {
      aes[[nm]] <- aes_map[[nm]]
    }
  }

  args1 <- list(data = df, mapping = aes)
  args2 <- args
  layer <- do.call(geom_type, c(args1,args2))

  if (length(manual) == 0) {
    return(layer)
  }

  structure(list(layer = layer, manual = manual), class = "gglite_layer")    
}


## main geoms

line <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels, args)
  args <- processed$args
  geom_type <- ggplot2::geom_line
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df, geom_type, aes, args, processed$manual, processed$aes_map)
}


point <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_point
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

## 2d geoms

smooth <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_smooth
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

step <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_step
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

jitter <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_jitter    
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}


## 1d geoms
histogram <- function(y,label=NULL, ...) {
  args <- list(...)
  df <- make_df(y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_histogram
  aes <- ggplot2::aes(x = y, y = stat(density), fill = label)
  args$alpha <- 0.5
  args$position <- "identity"
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

density <- function(y,label=NULL, ...) {
  args <- list(...)
  df <- make_df(y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_density
  aes <- ggplot2::aes(x = y, fill = label, col = label)
  args$alpha <- 0.25
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

qq <- function(y,label=NULL, ...) {
  args <- list(...)
  df <- data.frame(y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_qq
  aes <- ggplot2::aes(sample = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

qq_line <- function(y,label=NULL, ...) {
  args <- list(...)
  df <- data.frame(y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_qq_line
  aes <- ggplot2::aes(sample = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

## compare geoms

col <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  df$x <- as.factor(df$x)
  geom_type <- ggplot2::geom_col  
  aes <- ggplot2::aes(x = x, y = y, fill = label)
  args$position <- 'dodge'
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

boxplot <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  if (is.null(y)) {
    y=x
    x=0
  }
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_boxplot  
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

violin <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...)
  if (is.null(y)) {
    y=x
    x=0
  }
  df <- make_df(x, y)
  df <- apply_label(df, label)
  labels <- extract_labels(df)
  processed <- check_args(labels,args)
  args <- processed$args
  geom_type <- ggplot2::geom_violin 
  aes <- ggplot2::aes(x = x, y = y, fill = label, color = label)
  args$alpha <- 0.5
  args$scale <- 'area'
  make_geom(df,geom_type,aes,args,processed$manual,processed$aes_map)
}

merge_manual_values <- function(existing, new) {
  if (is.null(existing)) {
    existing <- c()
  }
  existing[names(new)] <- new
  existing
}

new_manual_scale <- function(aesthetic, values) {
  scale_fun <- switch(aesthetic,
    colour = ggplot2::scale_colour_manual,
    color = ggplot2::scale_colour_manual,
    fill = ggplot2::scale_fill_manual,
    linetype = ggplot2::scale_linetype_manual,
    stop(sprintf("Unsupported aesthetic '%s'", aesthetic))
  )
  scale <- scale_fun(values = values)
  scale$gglite_generated <- TRUE
  scale
}

remove_manual_scale <- function(plot, aesthetic) {
  if (length(plot$scales$scales) == 0) {
    return(plot)
  }

  keep <- vapply(plot$scales$scales, function(scale) {
    !(identical(scale$gglite_generated, TRUE) && aesthetic %in% scale$aesthetics)
  }, logical(1))

  plot$scales$scales <- plot$scales$scales[keep]
  plot
}

ggplot_add.gglite_layer <- function(object, plot, object_name) {
  plot <- ggplot2::ggplot_add(object$layer, plot, object_name)

  manual <- object$manual
  if (length(manual) == 0) {
    return(plot)
  }

  if (!is.null(manual$colour)) {
    values <- merge_manual_values(attr(plot, "gglite_manual_colour"), manual$colour)
    attr(plot, "gglite_manual_colour") <- values
    plot <- remove_manual_scale(plot, "colour")
    plot$scales$add(new_manual_scale("colour", values))
  }

  if (!is.null(manual$fill)) {
    values <- merge_manual_values(attr(plot, "gglite_manual_fill"), manual$fill)
    attr(plot, "gglite_manual_fill") <- values
    plot <- remove_manual_scale(plot, "fill")
    plot$scales$add(new_manual_scale("fill", values))
  }

  if (!is.null(manual$linetype)) {
    values <- merge_manual_values(attr(plot, "gglite_manual_linetype"), manual$linetype)
    attr(plot, "gglite_manual_linetype") <- values
    plot <- remove_manual_scale(plot, "linetype")
    plot$scales$add(new_manual_scale("linetype", values))
  }

  plot
}
