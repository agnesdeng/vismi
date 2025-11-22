# overimputation plot for two variables
#' @export
overimp2D <- function(obj, var.x, var.y, train.data, test.data = NULL, group.by.color = FALSE) {
  Names <- obj$params$Names

  if (!var.x %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  if (!var.y %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  Types <- obj$params$Types

  if (all(Types[c(var.x, var.y)] == "numeric" | Types[c(var.x, var.y)] == "integer")) {
    # both numeric/integer
    overimp2D_scatter(obj = obj, var.x = var.x, var.y = var.y, train.data = train.data, test.data = test.data)
  } else if ((is.factor(train.data[[var.x]]) || is.ordered(train.data[[var.x]])) && (is.factor(train.data[[var.y]]) || is.ordered(train.data[[var.y]]))) {
    # both factor
    overimp2D_bar(obj = obj, var.x = var.x, var.y = var.y, train.data = train.data, test.data = test.data, group.by.color = group.by.color)
  } else {
    # one factor, one numeric
    overimp2D_box(obj = obj, var.x = var.x, var.y = var.y, train.data = train.data, test.data = test.data)
  }
}


# plot overimputation for 2 numeric variables
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_point geom_density coord_cartesian facet_grid facet_wrap labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp2D_scatter <- function(obj, var.x, var.y, train.data, test.data = NULL) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.x %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  if (!var.y %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  addNA.m <- obj$params$addNA.m
  imputed.traindata <- obj$imputed.traindata




  # train data --------------------------------------------------------------


  # originally missing values
  missing <- which(is.na(train.data[, var.x]) | is.na(train.data[, var.y]))
  # missing=which(trainNA.m[,var.names[1]]| trainNA.m[,var.names[2]])

  # originally observed values (make missing = TRUE)
  observed <- which(addNA.m[, var.x] | addNA.m[, var.y])

  idx <- observed[!observed %in% missing]


  # originally observed values for this variable
  if (is.data.table(train.data) | is_tibble(train.data)) {
    # data.table
    train.data <- as.data.frame(train.data)
  }

  # data.frame
  true.df <- train.data[idx, c(var.x, var.y)]


  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])


  if (is.data.table(imputed.traindata[[1]])) {
    imputed <- lapply(imputed.traindata, function(x) x[idx, c(var.x, var.y), with = FALSE])
  } else {
    imputed <- lapply(imputed.traindata, function(x) x[idx, c(var.x, var.y)])
  }

  imputed.df <- do.call(rbind.data.frame, imputed)

  combine.df <- rbind(true.df, imputed.df)

  nobs <- length(idx)

  combine.df$m <- rep(c("True", paste0("m", 1:m)), each = nobs)
  combine.df$m <- factor(combine.df$m, levels = c("True", paste0("m", 1:m)))
  # combine.df$m <- factor(combine.df$m, levels = c("True", 1:m))
  # long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  var.names <- colnames(combine.df)


  # Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(m + 4)[-(1:4)]
  # traincolor <- c("#030303", Blues) "#bcd8ff" "#0c71ff"

  colfunc <- colorRampPalette(c("#85aeff", "#002cb3"))
  traincolor <- c("gray40", colfunc(m))

  sub.title <- paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")


  if (all(Types[c(var.x, var.y)] == "numeric" | Types[c(var.x, var.y)] == "integer")) {
    P1 <- ggplot(data = combine.df, aes(x = .data[[var.x]], y = .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(color = factor(m))) +
      facet_grid(rows = vars(m)) +
      scale_colour_manual(values = traincolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Training Data", subtitle = sub.title) +
      guides(color = "none")
  } else {
    stop("In `overimp2D_scatter()`, the variables specified in `var.x` and `var.y` must be numeric or integer")
  }



  # test data --------------------------------------------------------------
  if (!is.null(test.data)) {
    addNA.m2 <- obj$params$addNA.m2
    imputed.testdata <- obj$imputed.testdata


    # original missing values
    missing2 <- which(is.na(test.data[, var.x]) | is.na(test.data[, var.y]))
    # original observed values (make missing)
    observed2 <- which(addNA.m2[, var.x] | addNA.m2[, var.y])

    idx2 <- observed2[!observed2 %in% missing2]

    # originally observed values for this variable
    if (is.data.table(test.data) | is_tibble(test.data)) {
      # data.table
      test.data <- as.data.frame(test.data)
    }

    # data.frame
    true.df2 <- test.data[idx2, c(var.x, var.y)]

    # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
    # plot(true.df$Sepal.Length~true.df$Petal.Length)
    # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])

    if (is.data.table(imputed.testdata[[1]])) {
      imputed2 <- lapply(imputed.testdata, function(x) x[idx2, c(var.x, var.y), with = FALSE])
    } else {
      imputed2 <- lapply(imputed.testdata, function(x) x[idx2, c(var.x, var.y)])
    }

    imputed.df2 <- do.call(rbind.data.frame, imputed2)

    combine.df2 <- rbind(true.df2, imputed.df2)

    nobs <- length(idx2)

    combine.df2$m <- rep(c("True", paste0("m", 1:m)), each = nobs)
    combine.df2$m <- factor(combine.df2$m, levels = c("True", paste0("m", 1:m)))

    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 4)[-(1:4)]
    # testcolor <- c("#030303", Reds)
    # yellow "#ffe69d", "#FFBF00"
    colfunc <- colorRampPalette(c("#ffd24d", "#cc7700"))
    testcolor <- c("gray40", colfunc(m))

    xrange <- ggplot_build(P1)$layout$panel_scales_x[[1]]$range$range
    yrange <- ggplot_build(P1)$layout$panel_scales_y[[1]]$range$range


    # both numeric
    sub.title <- paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")

    P2 <- ggplot(data = combine.df2, aes(x = .data[[var.x]], y = .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(color = factor(m))) +
      coord_cartesian(xlim = xrange, ylim = yrange) +
      facet_grid(rows = vars(m)) +
      scale_colour_manual(values = testcolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Test Data", subtitle = sub.title) +
      guides(color = "none")




    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}




# plot overimputation for two factors
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_point geom_density coord_cartesian facet_grid facet_wrap labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp2D_bar <- function(obj, var.x, var.y, train.data, test.data = NULL, group.by.color = F) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.x %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  if (!var.y %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  addNA.m <- obj$params$addNA.m
  imputed.traindata <- obj$imputed.traindata


  # train data --------------------------------------------------------------


  # originally missing values
  missing <- which(is.na(train.data[, var.x]) | is.na(train.data[, var.y]))
  # missing=which(trainNA.m[,var.names[1]]| trainNA.m[,var.names[2]])

  # originally observed values (make missing = TRUE)
  observed <- which(addNA.m[, var.x] | addNA.m[, var.y])

  idx <- observed[!observed %in% missing]


  # originally observed values for this variable
  if (is.data.table(train.data) | is_tibble(train.data)) {
    # data.table
    train.data <- as.data.frame(train.data)
  }

  # data.frame
  true.df <- train.data[idx, c(var.x, var.y)]


  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])


  if (is.data.table(imputed.traindata[[1]])) {
    imputed <- lapply(imputed.traindata, function(x) x[idx, c(var.x, var.y), with = FALSE])
  } else {
    imputed <- lapply(imputed.traindata, function(x) x[idx, c(var.x, var.y)])
  }

  imputed.df <- do.call(rbind.data.frame, imputed)

  combine.df <- rbind(true.df, imputed.df)

  nobs <- length(idx)

  combine.df$m <- rep(c("True", paste0("m", 1:m)), each = nobs)
  combine.df$m <- factor(combine.df$m, levels = c("True", paste0("m", 1:m)))

  # combine.df$m <- factor(combine.df$m, levels = c("True", 1:m))
  # long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  var.names <- colnames(combine.df)


  # Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(m + 4)[-(1:4)]
  # traincolor <- c("#030303", Blues)

  colfunc <- colorRampPalette(c("#85aeff", "#002cb3"))
  traincolor <- c("gray40", colfunc(m))

  sub.title <- paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")

  convert.x.fac<-NULL
  convert.y.fac<-NULL

  if ((is.factor(combine.df[[var.x]]) || is.ordered(combine.df[[var.x]])) && (is.factor(combine.df[[var.y]]) || is.ordered(combine.df[[var.y]]))) {

    #both.fac<-TRUE

  } else if ((is.factor(combine.df[[var.x]]) || is.ordered(combine.df[[var.x]]) || is.integer(combine.df[[var.x]])) &&
    (is.factor(combine.df[[var.y]]) || is.ordered(combine.df[[var.y]]) || is.integer(combine.df[[var.y]]))) {
    if (is.integer(combine.df[[var.x]])) {
      warning("The variable ", var.x, " is of integer type. It will be treated as a factor for bar plots.\n")
      combine.df[[var.x]] <- as.factor(combine.df[[var.x]])
      convert.x.fac <- TRUE
    }
    if (is.integer(combine.df[[var.y]])) {
      warning("The variable ", var.y, " is of integer type. It will be treated as a factor for bar plots.\n")
      combine.df[[var.y]] <- as.factor(combine.df[[var.y]])
      convert.y.fac <- TRUE
    }
  } else if ((is.factor(combine.df[[var.x]]) || is.ordered(combine.df[[var.x]]) || is.logical(combine.df[[var.x]])) &&
    (is.factor(combine.df[[var.y]]) || is.ordered(combine.df[[var.y]]) || is.logical(combine.df[[var.y]]))) {
    if (is.logical(combine.df[[var.x]])) {
      warning("The variable ", var.x, " is of logical type. It will be treated as a factor for bar plots.\n")
      combine.df[[var.x]] <- as.factor(combine.df[[var.x]])
      convert.x.fac <- TRUE
    }
    if (is.logical(combine.df[[var.y]])) {
      warning("The variable ", var.y, " is of logical type. It will be treated as a factor for bar plots.\n")
      combine.df[[var.y]] <- as.factor(combine.df[[var.y]])
      convert.y.fac <- TRUE
    }
  } else {
    stop("In `overimp2D_bar()`, the variables specified in `var.x` and `var.y` must be both factor, or integer treated as factor.")
  }

  if (group.by.color) {
    # aes_string(x = var.names[1], fill = var.names[2])
    Ptrain <- ggplot(data = combine.df, aes(x = .data[[var.names[1]]], fill = .data[[var.names[2]]])) +
      geom_bar() +
      facet_wrap(~m, nrow = m + 1) +
      labs(title = "Training Data", subtitle = sub.title)+
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90")
      )

    g <- ggplot_gtable(ggplot_build(Ptrain))
    # original order: "strip-t-1-6","strip-t-1-5",...,"strip-t-1-1"
    stript <- rev(which(grepl("strip-t", g$layout$name)))
    fills <- c("#A0A0A0", rep("#D0D0D0", m))
    k <- 1
    for (i in stript) {
      j <- which(grepl("rect", g$grobs[[i]]$grobs[[1]]$childrenOrder))
      g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
      k <- k + 1
    }
  } else {
    Ptrain <- ggplot(data = combine.df, aes(x = .data[[var.names[1]]], alpha = .data[[var.names[2]]])) +
      geom_bar(aes(fill = m, color = m), size = 0.5) +
      scale_alpha_discrete(range = c(0.1, 1)) +
      facet_wrap(~m, nrow = m + 1) +
      scale_fill_manual(values = traincolor) +
      scale_color_manual(values = traincolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Training Data", subtitle = sub.title) +
      guides(color = "none", fill = "none")+
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        panel.grid.minor.y = element_line(color = "gray90")
      )
  }






  # test data --------------------------------------------------------------
  if (!is.null(test.data)) {
    addNA.m2 <- obj$params$addNA.m2
    imputed.testdata <- obj$imputed.testdata


    # original missing values
    missing2 <- which(is.na(test.data[, var.x]) | is.na(test.data[, var.y]))
    # original observed values (make missing)
    observed2 <- which(addNA.m2[, var.x] | addNA.m2[, var.y])

    idx2 <- observed2[!observed2 %in% missing2]

    # originally observed values for this variable
    if (is.data.table(test.data) | is_tibble(test.data)) {
      # data.table
      test.data <- as.data.frame(test.data)
    }

    # data.frame
    true.df2 <- test.data[idx2, c(var.x, var.y)]

    # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
    # plot(true.df$Sepal.Length~true.df$Petal.Length)
    # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])

    if (is.data.table(imputed.testdata[[1]])) {
      imputed2 <- lapply(imputed.testdata, function(x) x[idx2, c(var.x, var.y), with = FALSE])
    } else {
      imputed2 <- lapply(imputed.testdata, function(x) x[idx2, c(var.x, var.y)])
    }

    imputed.df2 <- do.call(rbind.data.frame, imputed2)

    combine.df2 <- rbind(true.df2, imputed.df2)

    nobs <- length(idx2)

    combine.df2$m <- rep(c("True", paste0("m", 1:m)), each = nobs)
    combine.df2$m <- factor(combine.df2$m, levels = c("True", paste0("m", 1:m)))

    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 4)[-(1:4)]
    # testcolor <- c("#030303", Reds)
    # yellow"#ffe69d"
    colfunc <- colorRampPalette(c("#ffd24d", "#cc7700"))
    testcolor <- c("gray40", colfunc(m))

    # xrange <- ggplot_build(P1)$layout$panel_scales_x[[1]]$range$range
    # yrange <- ggplot_build(P1)$layout$panel_scales_y[[1]]$range$range



    sub.title <- paste("Distribution of overimputed values:", paste(var.names[2], "vs", var.names[1]), sep = "\n")


    if (isTRUE(convert.x.fac)) {
      combine.df2[[var.x]] <- as.factor(combine.df2[[var.x]])
    }

    if (isTRUE(convert.y.fac)) {
      combine.df2[[var.y]] <- as.factor(combine.df2[[var.y]])
    }


    if (group.by.color) {
      Ptest <- ggplot(data = combine.df2, aes(x = .data[[var.names[1]]], fill = .data[[var.names[2]]])) +
        geom_bar() +
        facet_wrap(~m, nrow = m + 1) +
        labs(title = "Test Data", subtitle = sub.title)+
        theme(
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor.y = element_line(color = "gray90")
        )

      g2 <- ggplot_gtable(ggplot_build(Ptest))
      # original order: "strip-t-1-6","strip-t-1-5",...,"strip-t-1-1"
      stript <- rev(which(grepl("strip-t", g2$layout$name)))
      fills <- c("#A0A0A0", rep("#D0D0D0", m))
      k <- 1
      for (i in stript) {
        j <- which(grepl("rect", g2$grobs[[i]]$grobs[[1]]$childrenOrder))
        g2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
        k <- k + 1
      }


      gridExtra::grid.arrange(g, g2, ncol = 2)
    } else {
      Ptest <- ggplot(data = combine.df2, aes(x = .data[[var.names[1]]], alpha = .data[[var.names[2]]])) +
        geom_bar(aes(fill = m, color = m), size = 0.5) +
        scale_alpha_discrete(range = c(0.1, 1)) +
        facet_wrap(~m, nrow = m + 1) +
        scale_fill_manual(values = testcolor) +
        scale_color_manual(values = testcolor) +
        labs(title = "Training Data", subtitle = sub.title) +
        guides(color = "none", fill = "none")+
        theme(
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          panel.grid.minor.y = element_line(color = "gray90")
        )
      gridExtra::grid.arrange(Ptrain, Ptest, ncol = 2)
    }
  } else {
    # only training data
    if (group.by.color) {
      g
    } else {
      Ptrain
    }
  }
}




# plot overimputation for one factor one numeric variable
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_point geom_density coord_cartesian facet_grid facet_wrap labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank
#' @export
overimp2D_box <- function(obj, var.x, var.y, train.data, test.data = NULL) {

  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if (!var.x %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  if (!var.y %in% Names) {
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  addNA.m <- obj$params$addNA.m
  imputed.traindata <- obj$imputed.traindata


  # train data --------------------------------------------------------------


  # originally missing values
  missing <- which(is.na(train.data[, var.x]) | is.na(train.data[, var.y]))
  # missing=which(trainNA.m[,var.names[1]]| trainNA.m[,var.names[2]])

  # originally observed values (make missing = TRUE)
  observed <- which(addNA.m[, var.x] | addNA.m[, var.y])

  idx <- observed[!observed %in% missing]


  # originally observed values for this variable
  if (is.data.table(train.data) | is_tibble(train.data)) {
    # data.table
    train.data <- as.data.frame(train.data)
  }

  # data.frame
  true.df <- train.data[idx, c(var.x, var.y)]


  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])


  if (is.data.table(imputed.traindata[[1]])) {
    imputed <- lapply(imputed.traindata, function(x) x[idx, c(var.x, var.y), with = FALSE])
  } else {
    imputed <- lapply(imputed.traindata, function(x) x[idx, c(var.x, var.y)])
  }

  imputed.df <- do.call(rbind.data.frame, imputed)

  combine.df <- rbind(true.df, imputed.df)

  nobs <- length(idx)

  combine.df$m <- rep(c("True", paste0("m", 1:m)), each = nobs)
  combine.df$m <- factor(combine.df$m, levels = c("True", paste0("m", 1:m)))
  # combine.df$m <- factor(combine.df$m, levels = c("True", 1:m))
  # long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  var.names <- colnames(combine.df)


  # Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(m + 4)[-(1:4)]
  # traincolor <- c("#030303", Blues)

  colfunc <- colorRampPalette(c("#85aeff", "#002cb3"))
  traincolor <- c("gray40", colfunc(m))

  sub.title <- paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")


  if ((is.factor(combine.df[[var.x]]) & !is.factor(combine.df[[var.y]])) | (!is.factor(combine.df[[var.x]]) & is.factor(combine.df[[var.y]]))) {
    # one factor one numeric: boxplot
    # x-axis: factor,  y-axis: numeric/integer
    if (is.factor(combine.df[[var.x]])) {
      fac.idx <- 1
      num.idx <- 2
    } else {
      fac.idx <- 2
      num.idx <- 1
    }


    # yaxis.idx <- which(Types[var.names] == "numeric" | Types[var.names] == "integer")

    # xaxis.idx <- which(Types[var.names] == "factor")
    # yaxis.idx <- which(Types[var.names] == "numeric" | Types[var.names] == "integer")
    sub.title <- paste("Distribution of overimputed values:", paste(var.names[num.idx], "vs", var.names[fac.idx]), sep = "\n")

    Ptrain <- ggplot(data = combine.df, aes(x = .data[[var.names[fac.idx]]], y = .data[[var.names[num.idx]]])) +
      geom_jitter(position = position_jitter(width = 0.05), aes(colour = m), alpha = 0.7) +
      geom_boxplot(alpha = 0.7, aes(fill = m, color = m)) +
      facet_grid(rows = vars(m)) +
      scale_color_manual(values = traincolor) +
      scale_fill_manual(values = traincolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Training Data", subtitle = sub.title) +
      guides(color = "none", fill = "none")+
      theme(
        #panel.border = element_rect(linetype = "solid", fill = NA),
        #panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_line(color = "gray90"),
        #panel.grid.minor.y = element_line(color = "gray90")
      )
  } else {
    stop("In `overimp2D_box()`, the variables specified in `var.x` and `var.y` must be one factor and one numeric.")
  }


  # test data --------------------------------------------------------------
  if (!is.null(test.data)) {
    addNA.m2 <- obj$params$addNA.m2
    imputed.testdata <- obj$imputed.testdata


    # original missing values
    missing2 <- which(is.na(test.data[, var.x]) | is.na(test.data[, var.y]))
    # original observed values (make missing)
    observed2 <- which(addNA.m2[, var.x] | addNA.m2[, var.y])

    idx2 <- observed2[!observed2 %in% missing2]

    # originally observed values for this variable
    if (is.data.table(test.data) | is_tibble(test.data)) {
      # data.table
      test.data <- as.data.frame(test.data)
    }

    # data.frame
    true.df2 <- test.data[idx2, c(var.x, var.y)]

    # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
    # plot(true.df$Sepal.Length~true.df$Petal.Length)
    # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])

    if (is.data.table(imputed.testdata[[1]])) {
      imputed2 <- lapply(imputed.testdata, function(x) x[idx2, c(var.x, var.y), with = FALSE])
    } else {
      imputed2 <- lapply(imputed.testdata, function(x) x[idx2, c(var.x, var.y)])
    }

    imputed.df2 <- do.call(rbind.data.frame, imputed2)

    combine.df2 <- rbind(true.df2, imputed.df2)

    nobs <- length(idx2)

    combine.df2$m <- rep(c("True", paste0("m", 1:m)), each = nobs)
    combine.df2$m <- factor(combine.df2$m, levels = c("True", paste0("m", 1:m)))

    # Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 4)[-(1:4)]
    # testcolor <- c("#030303", Reds)
    # yellow
    colfunc <- colorRampPalette(c("#ffd24d", "#cc7700"))
    testcolor <- c("gray40", colfunc(m))

    # xrange <- ggplot_build(P1)$layout$panel_scales_x[[1]]$range$range
    # yrange <- ggplot_build(P1)$layout$panel_scales_y[[1]]$range$range


    # one factor one numeric: boxplot
    # x-axis: factor,  y-axis: numeric/integer

    sub.title <- paste("Distribution of overimputed values:", paste(var.names[num.idx], "vs", var.names[fac.idx]), sep = "\n")

    Ptest <- ggplot(data = combine.df2, aes(x = .data[[var.names[fac.idx]]], y = .data[[var.names[num.idx]]])) +
      geom_jitter(position = position_jitter(width = 0.05), aes(colour = m), alpha = 0.7) +
      geom_boxplot(alpha = 0.7, aes(fill = m, color = m)) +
      facet_grid(rows = vars(m)) +
      scale_color_manual(values = testcolor) +
      scale_fill_manual(values = testcolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Test Data", subtitle = sub.title) +
      guides(color = "none", fill = "none")+
      theme(
        #panel.border = element_rect(linetype = "solid", fill = NA),
        #panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
       #panel.grid.major.y = element_line(color = "gray90"),
       # panel.grid.minor.y = element_line(color = "gray90")
      )

    gridExtra::grid.arrange(Ptrain, Ptest, ncol = 2)
  } else {
    Ptrain
  }
}
