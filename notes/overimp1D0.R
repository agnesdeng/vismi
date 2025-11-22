# overimputation plot for a single variable
#' @export
overimp1D <- function(obj, var.name, train.data, test.data = NULL, num.plot = "ridge", int.plot="ridge", cat.plot = "bar") {

  Names <- obj$params$Names
  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  Types <- obj$params$Types

  if (Types[var.name] == "numeric" ) {
    if (num.plot == "ridge") {
      overimp1D_ridge(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (num.plot == "density") {
      overimp1D_density(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (num.plot == "qq") {
      overimp1D_qq(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (num.plot == "qqline") {
      overimp1D_qqline(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else {
      stop("The value specified in `num.plot` is not valid. Please use `ridge`, `density`,`qq`, or `qqline`.")
    }
  }else if(Types[var.name] == "integer"){
    if (int.plot == "ridge") {
      overimp1D_ridge(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (int.plot == "density") {
      overimp1D_density(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (int.plot == "qq") {
      overimp1D_qq(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (int.plot == "qqline") {
      overimp1D_qqline(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (int.plot == "bar") {
      overimp1D_bar(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (int.plot == "dodge") {
      overimp1D_dodge(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    }  else {
      stop("The value specified in `int.plot` is not valid. Please use `ridge`, `density`,`qq`, or `qqline`.")
    }
  } else {
    if (cat.plot == "bar") {
      overimp1D_bar(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else if (cat.plot == "dodge") {
      overimp1D_dodge(obj = obj, var.name = var.name, train.data = train.data, test.data = test.data)
    } else {
      stop("The value specified in `cat.plot` is not valid. Please use `bar` or `dodge`.")
    }
  }
}







#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot_build ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
#'
overimp1D_ridge <- function(obj, var.name, train.data, test.data = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  if (Types[var.name] != "numeric" & Types[var.name] != "integer") {
    stop("The variable name specified in `var.name` is not numeric or integer. Please use overimp1D_bar() or overimp1D_dodge() instead.")
  }


  long.df <- .overimp1d_long_data(obj, var.name, train.data, dataset = "train")
  sub.title <- .overimp1d_subtitle(m, var.name)

  # blue. "#0c71ff", "#bcd8ff"
  long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  colfunc <- colorRampPalette(c("#002cb3", "#85aeff"))
  traincolor <- c(colfunc(m), "gray40")


  # true values as points and bars
  P1 <- ggplot(data = long.df, aes(x = .data[[var.name]])) +
    geom_density_ridges(alpha = 0.8, aes(y = set, fill = set)) +
    scale_fill_manual(values = traincolor, guide = guide_legend(reverse = TRUE)) +
    guides(fill = "none") +
    #theme_ridges(center_axis_labels = TRUE) +
    labs(title = "Training Data", subtitle = sub.title, y = "Imputed sets")






  # test data ---------------------------------------------------------------

  if (!is.null(test.data)) {
    long.df2 <- .overimp1d_long_data(obj, var.name, test.data, dataset = "test")


    long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))
    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(m + 3)[-(1:3)]
    # Reds <- rev(Reds)
    # testcolor <- c(Reds, "#030303")
    # yellow "#FFBF00", "#ffe69d". "#FFBF00","#cc7700"
    colfunc <- colorRampPalette(c("#cc7700", "#ffd24d"))
    testcolor <- c(colfunc(m), "gray40")




    xrange <- ggplot_build(P1)$layout$panel_params[[1]]$x.range


    # true values as points and bars
    P2 <- ggplot(data = long.df2, aes(x = .data[[var.name]])) +
      geom_density_ridges(alpha = 0.8, aes(y = set, fill = set)) +
      coord_cartesian(xlim = xrange) +
      scale_fill_manual(values = testcolor, guide = guide_legend(reverse = TRUE)) +
      guides(fill = "none") +
      #theme_ridges(center_axis_labels = TRUE) +
      labs(title = "Test Data", subtitle = sub.title, y = "Imputed sets")


    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}

#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot_build ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp1D_density <- function(obj, var.name, train.data, test.data = NULL,ylim = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  if (Types[var.name] != "numeric" & Types[var.name] != "integer") {
    stop("The variable name specified in `var.name` is not numeric or integer. Please use overimp1D_bar() or overimp1D_dodge() instead.")
  }

  long.df <- .overimp1d_long_data(obj, var.name, train.data, dataset = "train")
  sub.title <- .overimp1d_subtitle(m, var.name)

  # blue
  long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  colfunc <- colorRampPalette(c("#002cb3", "#85aeff"))
  traincolor <- c(colfunc(m), "black")

  P1 <- ggplot(data = long.df, aes(.data[[var.name]], color = set)) +
    geom_density() +
    scale_color_manual(values = traincolor) +
    labs(title = "Training Data", subtitle = sub.title)

 if(!is.null(ylim)){
    P1 <- P1 + ylim(ylim)
  }

  # test data ---------------------------------------------------------------

  if (!is.null(test.data)) {
    long.df2 <- .overimp1d_long_data(obj, var.name, test.data, dataset = "test")


    long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))
    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(m + 3)[-(1:3)]
    # Reds <- rev(Reds)
    # testcolor <- c(Reds, "#030303")
    # yellow
    colfunc <- colorRampPalette(c("#cc7700", "#ffd24d"))
    testcolor <- c(colfunc(m), "black")




    xrange <- ggplot_build(P1)$layout$panel_params[[1]]$x.range

    P2 <- ggplot(data = long.df2, aes(.data[[var.name]], color = set)) +
      geom_density() +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = testcolor) +
      labs(title = "Test Data", subtitle = sub.title)

    if(!is.null(ylim)){
      P2 <- P2 + ylim(ylim)
    }

    gridExtra::grid.arrange(P1, P2, ncol = 2)

  } else {
    P1
  }
}




#' plot qqplot for numeric
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot_build ggplot ylim aes vars stat_qq geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp1D_qq <- function(obj, var.name, train.data, test.data = NULL, point.size = 1,ylim = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  if (Types[var.name] != "numeric" & Types[var.name] != "integer") {
    stop("The variable name specified in `var.name` is not numeric or integer. Please use overimp1D_bar() or overimp1D_dodge() instead.")
  }

  long.df <- .overimp1d_long_data(obj, var.name, train.data, dataset = "train")
  sub.title <- .overimp1d_subtitle(m, var.name)

  # blue
  long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  colfunc <- colorRampPalette(c("#002cb3", "#85aeff"))
  traincolor <- c(colfunc(m), "black")




  # true values as points and bars
  P1 <- ggplot(data = long.df, aes(sample = .data[[var.name]], color = set)) +
    stat_qq(size = point.size) +
    # stat_qq_line()+
    # geom_density()+
    scale_color_manual(values = traincolor) +
    labs(title = "Training Data", subtitle = sub.title)

  if(!is.null(ylim)){
    P1 <- P1 + ylim(ylim)
  }




  # test data ---------------------------------------------------------------

  if (!is.null(test.data)) {
    long.df2 <- .overimp1d_long_data(obj, var.name, test.data, dataset = "test")


    long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))
    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(m + 3)[-(1:3)]
    # Reds <- rev(Reds)
    # testcolor <- c(Reds, "#030303")
    # yellow
    colfunc <- colorRampPalette(c("#cc7700", "#ffd24d"))
    testcolor <- c(colfunc(m), "black")




    xrange <- ggplot_build(P1)$layout$panel_params[[1]]$x.range

    P2 <- ggplot(data = long.df2, aes(sample = .data[[var.name]], color = set)) +
      stat_qq(size = point.size) +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = testcolor) +
      labs(title = "Test Data", subtitle = sub.title)

    if(!is.null(ylim)){
      P2 <- P2 + ylim(ylim)
    }


    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}






# qqline ------------------------------------------------------------------
#' plot qqline plot for numeric
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot_build ggplot ylim aes vars stat_qq_line geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp1D_qqline <- function(obj, var.name, train.data, test.data = NULL,ylim = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  if (Types[var.name] != "numeric" & Types[var.name] != "integer") {
    stop("The variable name specified in `var.name` is not numeric or integer. Please use overimp1D_bar() or overimp1D_dodge() instead.")
  }

  long.df <- .overimp1d_long_data(obj, var.name, train.data, dataset = "train")
  sub.title <- .overimp1d_subtitle(m, var.name)

  # blue
  long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  colfunc <- colorRampPalette(c("#002cb3", "#85aeff"))
  traincolor <- c(colfunc(m), "black")




  # true values as points and bars
  P1 <- ggplot(data = long.df, aes(sample = .data[[var.name]], color = set)) +
    # stat_qq(size=1)+
    stat_qq_line() +
    # geom_density()+
    scale_color_manual(values = traincolor) +
    labs(title = "Training Data", subtitle = sub.title)

  if(!is.null(ylim)){
    P1 <- P1 + ylim(ylim)
  }

  # test data ---------------------------------------------------------------

  if (!is.null(test.data)) {
    long.df2 <- .overimp1d_long_data(obj, var.name, test.data, dataset = "test")


    long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))
    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(m + 3)[-(1:3)]
    # Reds <- rev(Reds)
    # testcolor <- c(Reds, "#030303")
    # yellow
    colfunc <- colorRampPalette(c("#cc7700", "#ffd24d"))
    testcolor <- c(colfunc(m), "black")


    xrange <- ggplot_build(P1)$layout$panel_params[[1]]$x.range

    P2 <- ggplot(data = long.df2, aes(sample = .data[[var.name]], color = set)) +
      # stat_qq(size=1)+
      stat_qq_line() +
      coord_cartesian(xlim = xrange) +
      scale_color_manual(values = testcolor) +
      labs(title = "Test Data", subtitle = sub.title)

    if(!is.null(ylim)){
      P2 <- P2 + ylim(ylim)
    }

    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}


#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp1D_bar <- function(obj, var.name, train.data, test.data = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  if (Types[var.name] == "numeric") {
    stop("The variable name specified in `var.name` is numeric. Please use overimp1D_ridge(), overimp1D_density(), overimp1D_qq(), overimp1D_qqline() instead.")
  }

  long.df <- .overimp1d_long_data(obj, var.name, train.data, dataset = "train")
  sub.title <- .overimp1d_subtitle(m, var.name)

  # blue
  long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  colfunc <- colorRampPalette(c("#002cb3", "#85aeff"))
  traincolor <- c(colfunc(m), "gray40")

  if (Types[var.name] == "integer") {
    warning("The variable name specified in `var.name` is integer. If you want to treat it as numeric, please use overimp1D_ridge(), overimp1D_density(), overimp1D_qq(), overimp1D_qqline() instead.")
    long.df[[var.name]] <- as.factor(long.df[[var.name]])
  }

  P1 <- ggplot(data = long.df, aes(x = .data[[var.name]], alpha = .data[[var.name]]), stat = "identity") +
    geom_bar(aes(fill = set)) +
    scale_alpha_discrete(range = c(0.8, 1)) +
    facet_grid(vars(fct_rev(set)), switch = "y") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "black"),
      strip.text.y.left = element_text(angle = 0)
    ) +
    labs(title = "Training Data", subtitle = sub.title, y = "Imputed sets") +
    scale_fill_manual(values = traincolor) +
    guides(fill = "none", alpha = "none")




  # test data ---------------------------------------------------------------

  if (!is.null(test.data)) {
    long.df2 <- .overimp1d_long_data(obj, var.name, test.data, dataset = "test")


    long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))
    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(m + 3)[-(1:3)]
    # Reds <- rev(Reds)
    # testcolor <- c(Reds, "#030303")
    # yellow
    colfunc <- colorRampPalette(c("#cc7700", "#ffd24d"))
    testcolor <- c(colfunc(m), "gray40")


    if (Types[var.name] == "integer") {
      long.df2[[var.name]] <- as.factor(long.df2[[var.name]])
    }

    P2 <- ggplot(data = long.df2, aes(x = .data[[var.name]], alpha = .data[[var.name]])) +
      geom_bar(aes(fill = set)) +
      scale_alpha_discrete(range = c(0.8, 1)) +
      facet_grid(vars(fct_rev(set)), switch = "y") +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black"),
        strip.text.y.left = element_text(angle = 0)
      ) +
      labs(title = "Test Data", subtitle = sub.title, y = "Imputed sets") +
      scale_fill_manual(values = testcolor) +
      guides(fill = "none", alpha = "none")




    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}


#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp1D_dodge <- function(obj, var.name, train.data, test.data = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }


  if (Types[var.name] == "numeric") {
    stop("The variable name specified in `var.name` is numeric. Please use overimp1D_ridge(), overimp1D_density(), overimp1D_qq(), overimp1D_qqline() instead.")
  }
  long.df <- .overimp1d_long_data(obj, var.name, train.data, dataset = "train")
  sub.title <- .overimp1d_subtitle(m, var.name)

  # blue
  long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  #colfunc <- colorRampPalette(c("#3d2bfe", "#2b83fe"))
  colfunc <- colorRampPalette(c("#002cb3", "#85aeff"))
  traincolor <- c(colfunc(m), "black")

  if (Types[var.name] == "integer") {
    warning("The variable name specified in `var.name` is integer. If you want to treat it as numeric, please use overimp1D_ridge(), overimp1D_density(), overimp1D_qq(), overimp1D_qqline() instead.")
    long.df[[var.name]] <- as.factor(long.df[[var.name]])
  }


  traincolor <- c(colfunc(m), "gray20")
  long.df$set <- factor(long.df$set, levels = rev(levels(long.df$set)))
  traincolor <- c("gray20", rev(colfunc(m)))

  P1 <- ggplot(data = long.df, aes(x = .data[[var.name]], fill = set)) +
    geom_bar(aes(alpha = set), position = "dodge") +
    scale_color_manual(values = traincolor) +
    scale_fill_manual(values = traincolor) +
    scale_alpha_discrete(range = c(0.8, 1)) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(color = "black")
    ) +
    labs(title = "Training Data", subtitle = sub.title)
  # guides(fill="none",alpha="none")







  # test data ---------------------------------------------------------------

  if (!is.null(test.data)) {
    long.df2 <- .overimp1d_long_data(obj, var.name, test.data, dataset = "test")


    long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))
    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(m + 3)[-(1:3)]
    # Reds <- rev(Reds)
    # testcolor <- c(Reds, "#030303")
    # yellow
    #colfunc <- colorRampPalette(c("#ff7a00", "#ffe300"))
    colfunc <- colorRampPalette(c("#cc7700", "#ffd24d"))

    testcolor <- c(colfunc(m), "black")


    if (Types[var.name] == "integer") {
      long.df[[var.name]] <- as.factor(long.df[[var.name]])
    }


    long.df2$set <- factor(long.df2$set, levels = rev(levels(long.df2$set)))
    testcolor <- c("gray20", rev(colfunc(m)))

    P2 <- ggplot(data = long.df2, aes(x = .data[[var.name]], fill = set)) +
      geom_bar(aes(alpha = set), position = "dodge") +
      scale_color_manual(values = testcolor) +
      scale_fill_manual(values = testcolor) +
      scale_alpha_discrete(range = c(0.8, 1)) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(color = "black")
      ) +
      labs(title = "Test Data", subtitle = sub.title)
    # guides(fill="none",alpha="none")


    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}
