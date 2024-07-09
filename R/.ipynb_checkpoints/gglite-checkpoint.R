options(jupyter.plot_scale=4)
options(repr.plot.width=6,repr.plot.height=2.5,repr.plot.res=300)
figsize <- function(width=6,height=2.5){
    options(repr.plot.width=width,repr.plot.height=height,repr.plot.res=300)
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

# check_args <- function(label, args) {
#   if (!is.null(label) && any(names(args) %in% c("col", "color", "colour","fill"))) {
#     stop("You cannot use the combination of (label, color) or (label, fill) options because when you use the 'label' option, 'color' or 'fill' is automatically set based on the categories.")
#   }
# }

make_geom <- function(df,geom_type,aes,args) {
  args1 <- list(data = df, mapping = aes)
  args2 <- args
  do.call(geom_type, c(args1,args2))    
}


## main geoms

line <- function(x, y = NULL, label = NULL, ...) {
  args <- list(...)
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_line
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args)
}


point <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_point
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args)
}

## 2d geoms

smooth <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_smooth
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args)
}

step <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_step
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args)
}

jitter <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_jitter    
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args)
}


## 1d geoms
histogram <- function(y,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(y)
  geom_type <- ggplot2::geom_histogram
  aes <- ggplot2::aes(x = y, y = stat(density), fill = label)
  args$alpha <- 0.5
  args$position <- "identity"
  make_geom(df,geom_type,aes,args)
}

density <- function(y,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(y)
  geom_type <- ggplot2::geom_density
  aes <- ggplot2::aes(x = y, fill = label, col = label)
  args$alpha <- 0.25
  make_geom(df,geom_type,aes,args)
}

qq <- function(y,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- data.frame(y)
  geom_type <- ggplot2::geom_qq
  aes <- ggplot2::aes(sample = y, col = label)
  make_geom(df,geom_type,aes,args)
}

qq_line <- function(y,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- data.frame(y)
  geom_type <- ggplot2::geom_qq_line
  aes <- ggplot2::aes(sample = y, col = label)
  make_geom(df,geom_type,aes,args)
}

## compare geoms

col <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  df <- make_df(x, y)
  df$x <- as.factor(df$x)
  geom_type <- ggplot2::geom_col  
  aes <- ggplot2::aes(x = x, y = y, fill = label)
  args$position <- 'dodge'
  make_geom(df,geom_type,aes,args)
}

boxplot <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  if (is.null(y)) {
    y=x
    x=0
  }
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_boxplot  
  aes <- ggplot2::aes(x = x, y = y, col = label)
  make_geom(df,geom_type,aes,args)
}

violin <- function(x, y=NULL,label=NULL, ...) {
  args <- list(...); check_args(label,args)
  if (is.null(y)) {
    y=x
    x=0
  }
  df <- make_df(x, y)
  geom_type <- ggplot2::geom_violin 
  aes <- ggplot2::aes(x = x, y = y, fill = label, color = label)
  args$alpha <- 0.5
  args$scale <- 'area'
  make_geom(df,geom_type,aes,args)
}