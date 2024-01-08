#' plot qqplot for overimputation of training set and test set
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp_traintest_qq <- function(obj, var.name, train.data, test.data, color.pal = NULL) {

  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.name %in% Names) {
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  if (Types[var.name] != "numeric" & Types[var.name] != "integer") {
    stop("The variable name specified in `var.name` is not numeric or integer. Please use overimp1D_bar() or overimp1D_dodge() instead.")
  }

  addNA.m <- obj$params$addNA.m



  imputed.traindata <- obj$imputed.traindata






  # train data --------------------------------------------------------------
  imputed <- lapply(imputed.traindata, function(x) x[[var.name]][addNA.m[, var.name]])
  Nrow <- length(imputed[[1]])

  L <- unlist(imputed)
  Mat <- matrix(L, nrow = Nrow, ncol = m)

  # original observed values for this variable
  if (is.data.table(train.data) | is_tibble(train.data)) {
    # data.table
    train.data <- as.data.frame(train.data)
  }

  # data.frame
  True <- train.data[, var.name][addNA.m[, var.name]]

  combine.df <- cbind.data.frame(Mat, True)
  colnames(combine.df) <- c(paste0("m", 1:m), "True")
  long.df <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = var.name)



  # blue
  #long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))

  addNA.m2 <- obj$params$addNA.m2
  imputed.testdata <- obj$imputed.testdata


  imputed <- lapply(imputed.testdata, function(x) x[[var.name]][addNA.m2[, var.name]])
  Nrow <- length(imputed[[1]])

  L <- unlist(imputed)
  Mat <- matrix(L, nrow = Nrow, ncol = m)

  # originally observed values
  if (is.data.table(test.data) | is_tibble(test.data)) {
    # data.table
    test.data <- as.data.frame(test.data)
  }


  True <- test.data[, var.name][addNA.m2[, var.name]]

  combine.df2 <- cbind.data.frame(Mat, True)
  colnames(combine.df2) <- c(paste0("m", 1:m), "True")
  long.df2 <- pivot_longer(data = combine.df2, cols = everything(), names_to = "set", values_to = var.name)


  #long.df2$set <- factor(long.df2$set, levels = c(paste0("m", m:1), "True"))







  #qqplot(long.df$BMPHEAD, long.df2$BMPHEAD)
 # train.true<-subset(long.df,set=="True")
 # test.true<-subset(long.df2,set=="True")
 # qqplot(train.true$BMPHEAD, test.true$BMPHEAD)

  quantiles.df <- lapply(unique(long.df$set), function(set) {
    sample1 <- long.df[[var.name]][long.df$set == set]
    sample2 <- long.df2[[var.name]][long.df2$set == set]
    probs <- seq(0, 1, length.out = min(length(sample1), length(sample2)))

    data.frame(
      Set = set,
      Quantiles.Sample1 = quantile(sample1, probs = probs),
      Quantiles.Sample2 = quantile(sample2, probs = probs)
    )
  }) %>%
    dplyr::bind_rows()

  quantiles.df$Set <- factor(quantiles.df$Set, levels = c("True",paste0("m", 1:5)))



  # Determine the range for the breaks
  train.range<- range(quantiles.df$Quantiles.Sample1, na.rm = TRUE)
  test.range <- range(quantiles.df$Quantiles.Sample2, na.rm = TRUE)

  axis.min<-max(min(train.range),min(test.range))
  axis.max<-min(max(train.range),max(test.range))

  #combined.range <- range(train.range, test.range)

  # Decide on the number of breaks
  num.breaks <- 4  # For example, 6 breaks
  break.points<-round(seq(axis.min, axis.max, length.out = num.breaks))

  if (is.null(color.pal)) {
    color.pal <- c("gray20", scales::hue_pal()(m))
  }

  ggplot(quantiles.df, aes(x = Quantiles.Sample1, y = Quantiles.Sample2, color= Set)) +
    geom_point() +
    geom_abline(aes(slope = 1, intercept = 0), linetype = "solid")+
    scale_x_continuous(breaks = break.points) +
    scale_y_continuous(breaks = break.points) +
    facet_wrap(~ Set, ncol=m+1) +  # Facets for each set
    scale_color_manual(values = color.pal)+
    xlab("Quantiles of training data") +
    ylab("Quantiles of test data") +
    labs(title = "QQ plots for overimputation in training set and test set", subtitle = paste(" for variable: ", var.name)) +
    guides(color="none")+
    theme(
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(face = "bold")
    )

}


