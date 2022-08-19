#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_point geom_density coord_cartesian facet_grid facet_wrap labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank

plot2D <- function(obj, var.x, var.y, train.data, test.data = NULL, plotfac.type = 1) {





  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if(!var.x %in% Names){
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  if(!var.y %in% Names){
    stop("The variable name specified in `var.x` does not exist in train.data")
  }

  addNA.m <- obj$params$addNA.m
  imputed.traindata <- obj$imputed.traindata




  # train data --------------------------------------------------------------


  # originally missing values
  missing <- which(is.na(train.data[, var.x]) | is.na(train.data[,var.y]))
  # missing=which(trainNA.m[,var.names[1]]| trainNA.m[,var.names[2]])

  # originally observed values (make missing = TRUE)
  observed <- which(addNA.m[, var.x] | addNA.m[, var.y])

  idx <- observed[!observed %in% missing]


  # originally observed values for this variable
  if(is.data.table(train.data)| is_tibble(train.data)){
    #data.table
    train.data <- as.data.frame(train.data)
  }

  #data.frame
  true.df <- train.data[idx, c(var.x, var.y)]


  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])


  if(is.data.table(imputed.traindata[[1]])){
    imputed <- lapply(imputed.traindata, function(x) x[idx,c(var.x, var.y),with = FALSE])
  }else{
    imputed <- lapply(imputed.traindata, function(x) x[idx,c(var.x, var.y)])
  }

  imputed.df <- do.call(rbind.data.frame, imputed)

  combine.df <- rbind(true.df, imputed.df)

  nobs <- length(idx)

  combine.df$m <- rep(c("True", paste0("m",1:m)), each = nobs)
  combine.df$m <- factor(combine.df$m, levels = c("True",paste0("m", 1:m)))
  #combine.df$m <- factor(combine.df$m, levels = c("True", 1:m))
  #long.df$set <- factor(long.df$set, levels = c(paste0("m", m:1), "True"))
  var.names <- colnames(combine.df)


  #Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(m + 4)[-(1:4)]
  #traincolor <- c("#030303", Blues)

  colfunc <- colorRampPalette(c("#bcd8ff","#0c71ff"))
  traincolor<-c("gray40",colfunc(m))

  sub.title <- paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")
  if (all(Types[c(var.x,var.y)] == "numeric" | Types[c(var.x,var.y)] == "integer")) {
    # both numeric: scatterplot



    P1 <- ggplot(data = combine.df, aes(x = .data[[var.x]], y =  .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(color = factor(m))) +
      facet_grid(rows = vars(m)) +
      scale_colour_manual(values = traincolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Training Data", subtitle = sub.title)+
      guides(color="none")

  } else if (is.factor(combine.df[,var.x]) & is.factor(combine.df[,var.y])) {
    # both factor

    if (plotfac.type == 1) {
      P1 <- ggplot(data = combine.df, aes(x = .data[[var.x]], y =  .data[[var.y]])) +
        geom_bar(stat="identity") +
        facet_wrap(m) +
        labs(title = "Training Data", subtitle = sub.title)

      g <- ggplot_gtable(ggplot_build(Ptrain))
      # original order: "strip-t-1-6","strip-t-1-5",...,"strip-t-1-1"
      stript <- rev(which(grepl("strip-t", g$layout$name)))
      fills <- c("#A0A0A0", rep("#D0D0D0", M))
      k <- 1
      for (i in stript) {
        j <- which(grepl("rect", g$grobs[[i]]$grobs[[1]]$childrenOrder))
        g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
        k <- k + 1
      }
    } else {
      Ptrain <- ggplot(data = combine.df, aes_string(x = var.names[1], alpha = var.names[2])) +
        geom_bar(aes(fill = M, color = M), size = 0.2) +
        scale_alpha_discrete(range = c(0.1, 1)) +
        facet_wrap(~M, nrow = M + 1) +
        scale_fill_manual(values = traincolor) +
        scale_color_manual(values = traincolor) +
        theme(strip.text.y = element_text(angle = 0)) +
        labs(title = "Training Data", subtitle = sub.title)
    }
  } else {
    # one factor one numeric: boxplot
    # x-axis: factor,  y-axis: numeric/integer
    xaxis.idx <- which(Types[var.names] == "factor")
    yaxis.idx <- which(Types[var.names] == "numeric" | Types[var.names] == "integer")
    sub.title <- paste("Distribution of overimputed values:", paste(var.names[yaxis.idx], "vs", var.names[xaxis.idx]), sep = "\n")

    Ptrain <- ggplot(data = combine.df, aes_string(x = var.names[xaxis.idx], y = var.names[yaxis.idx])) +
      geom_jitter(position = position_jitter(width = 0.05), aes(colour = factor(M)), alpha = 0.7) +
      geom_boxplot(alpha = 0.7, aes(fill = factor(M), color = factor(M))) +
      facet_grid(rows = vars(M)) +
      scale_color_manual(values = traincolor) +
      scale_fill_manual(values = traincolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Training Data", subtitle = sub.title)
  }


  # test data --------------------------------------------------------------
  if(!is.null(test.data)){
    addNA.m2 <- obj$params$addNA.m2
    imputed.testdata <- obj$imputed.testdata


  # original missing values
  missing2 <- which(is.na(test.data[, var.x]) | is.na(test.data[,var.y]))
  # original observed values (make missing)
  observed2 <- which(addNA.m2[, var.x] | addNA.m2[, var.y])

  idx2 <- observed2[!observed2 %in% missing2]

  # originally observed values for this variable
  if(is.data.table(test.data)| is_tibble(test.data)){
    #data.table
    test.data <- as.data.frame(test.data)
  }

  #data.frame
  true.df2 <- test.data[idx2, c(var.x, var.y)]

  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])

  if(is.data.table(imputed.testdata[[1]])){
    imputed2 <- lapply(imputed.testdata, function(x) x[idx2,c(var.x, var.y),with = FALSE])
  }else{
    imputed2 <- lapply(imputed.testdata, function(x) x[idx2,c(var.x, var.y)])
  }

  imputed.df2 <- do.call(rbind.data.frame, imputed2)

  combine.df2 <- rbind(true.df2, imputed.df2)

  nobs <- length(idx2)

  combine.df2$m <- rep(c("True", paste0("m",1:m)), each = nobs)
  combine.df2$m <- factor(combine.df2$m, levels = c("True",paste0("m", 1:m)))

  #Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 4)[-(1:4)]
  #testcolor <- c("#030303", Reds)
  #yellow
  colfunc <- colorRampPalette(c("#ffe69d","#FFBF00"))
  testcolor<-c("gray40",colfunc(m))

  xrange <- ggplot_build(P1)$layout$panel_scales_x[[1]]$range$range
  yrange <- ggplot_build(P1)$layout$panel_scales_y[[1]]$range$range

  if (all(Types[c(var.x,var.y)] == "numeric" | Types[c(var.x,var.y)] == "integer")) {
    # both numeric
    sub.title <- paste("Distribution of overimputed values:", paste(var.y, "vs", var.x), sep = "\n")

    P2 <- ggplot(data = combine.df2, aes(x = .data[[var.x]], y =  .data[[var.y]])) +
      geom_point(alpha = 0.6, aes(color = factor(m))) +
      coord_cartesian(xlim = xrange, ylim = yrange) +
      facet_grid(rows = vars(m)) +
      scale_colour_manual(values = testcolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Test Data", subtitle = sub.title)+
      guides(color="none")



  } else if (all(Types[var.names] == "factor")) {
    # both factor

    sub.title <- paste("Distribution of overimputed values:", paste(var.names[2], "vs", var.names[1]), sep = "\n")

    if (plotfac.type == 1) {
      Ptest <- ggplot(data = combine.df2, aes_string(x = var.names[1], fill = var.names[2])) +
        geom_bar() +
        facet_wrap(~M, nrow = M + 1) +
        labs(title = "Test Data", subtitle = sub.title)

      g2 <- ggplot_gtable(ggplot_build(Ptest))
      # original order: "strip-t-1-6","strip-t-1-5",...,"strip-t-1-1"
      stript <- rev(which(grepl("strip-t", g2$layout$name)))
      fills <- c("#A0A0A0", rep("#D0D0D0", M))
      k <- 1
      for (i in stript) {
        j <- which(grepl("rect", g2$grobs[[i]]$grobs[[1]]$childrenOrder))
        g2$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
        k <- k + 1
      }


      gridExtra::grid.arrange(g, g2, ncol = 2)
    } else {
      Ptest <- ggplot(data = combine.df2, aes_string(x = var.names[1], alpha = var.names[2])) +
        geom_bar(aes(fill = M, color = M), size = 0.2) +
        scale_alpha_discrete(range = c(0.1, 1)) +
        facet_wrap(~M, nrow = M + 1) +
        scale_fill_manual(values = testcolor) +
        scale_color_manual(values = testcolor) +
        labs(title = "Training Data", subtitle = sub.title)
      gridExtra::grid.arrange(Ptrain, Ptest, ncol = 2)
    }
  } else {
    # one factor one numeric: boxplot
    # x-axis: factor,  y-axis: numeric/integer
    xaxis.idx <- which(Types[var.names] == "factor")
    yaxis.idx <- which(Types[var.names] == "numeric" | Types[var.names] == "integer")
    sub.title <- paste("Distribution of overimputed values:", paste(var.names[yaxis.idx], "vs", var.names[xaxis.idx]), sep = "\n")

    Ptest <- ggplot(data = combine.df2, aes_string(x = var.names[xaxis.idx], y = var.names[yaxis.idx])) +
      geom_jitter(position = position_jitter(width = 0.05), aes(colour = factor(M)), alpha = 0.7) +
      geom_boxplot(alpha = 0.7, aes(fill = factor(M), color = factor(M))) +
      facet_grid(rows = vars(M)) +
      scale_color_manual(values = testcolor) +
      scale_fill_manual(values = testcolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Test Data", subtitle = sub.title)

    gridExtra::grid.arrange(Ptrain, Ptest, ncol = 2)
  }
  gridExtra::grid.arrange(P1, P2, ncol = 2)

  }else{
    P1
  }
}
