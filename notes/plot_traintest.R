obj=iris.obj
var.names = c( "Sepal.Length","Species")
plot.all=FALSE
train.data=trainNA.df
test.data=testNA.df

colnames(iris)
var="Sepal.Length"
var="Species"

plot1D.entries <- function(obj, var.names = c( "Sepal.Length","Sepal.Width"), plot.all = FALSE, train.data = trainNA.df, test.data = testNA.df) {

  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m


  if (length(unique(var.names)) > length(Names)) {
    stop("The number of variables specified in var.names exceeds the number of variables in train.data.")
  }


  if (!is.numeric(var.names)) {
    if (all(var.names %in% Names)) {
      var.names <- match(var.names, Names)
    } else {
      stop("Variable names specified in var.names do not match with the names in train.data")
    }
  }else{
    stop("Names of variables need to be specified in `var.names`.")
  }


  addNA.m <- obj$params$addNA.m
  addNA.m2 <- obj$params$addNA.m2


  imputed.traindata <- obj$imputed.traindata
  imputed.testdata <- obj$imputed.testdata


  if (plot.all) {
    var.names <- colnames(train.data)
  }


  # train data --------------------------------------------------------------

  for(var in var.names){

    imputed <- lapply(imputed.traindata, function(x) x[[var]][addNA.m[,var]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # originally observed values for this variable
    if(is.data.table(train.data)){
      #data.table


    }else{
      #data.frame
      True <- train.data[, var][addNA.m[, var]]
    }


    # If the number of originall observed values exceed 9, randomly sample 9 entries
    if (Nrow > 9) {
      idx <- sample(1:Nrow, 9, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- 9
    }

    combine.df <- cbind.data.frame(Mat, True)
    colnames(combine.df) <- c(paste0("m", 1:m), "True")
    combine.df$entry <- factor(1:Nrow, levels = Nrow:1)

    long.df <- pivot_longer(data = combine.df, cols = !entry, names_to = "set", values_to = var)
    long.df <- long.df %>% mutate(
      set = factor(set, levels=c(paste0("m", 1:m), "True"))
    )


    msg1 <- paste("Distribution of", m, "sets of imputed values")
    msg2 <- paste(paste("for randomly selected", Nrow), "entries")
    msg3 <- paste("in variable", var)
    sub.title <- paste(msg1, msg2, msg3, sep = "\n")




    if (Types[var] == "numeric" | Types[var] == "integer") {
      true.df <- cbind.data.frame(1:Nrow, True)
      colnames(true.df)[1] <- c("entry")

      Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(Nrow + 3)[-(1:3)]
      Blues <- rev(Blues)

      # true values as points and bars
      P <- ggplot(data = long.df, aes(x = .data[[var]])) +
        geom_density_ridges(alpha = 0.5, aes(y = entry, fill = entry)) +
        scale_fill_manual(values = Blues, guide = guide_legend(reverse = TRUE)) +
        geom_point(data = true.df, aes(x = True, y = entry), color = "blue") +
        geom_segment(data = true.df, aes(x = True, xend = True, y = as.numeric(entry), yend = as.numeric(entry) + 0.5), color = "blue") +
        theme_ridges(center_axis_labels = TRUE) +
        labs(title = "Training Data", subtitle = sub.title)

    } else {
      true.df <- cbind.data.frame(1:Nrow, True)
      colnames(true.df) <- c("entry", var)

      # factor

      P <- ggplot(data = long.df, aes(x = .data[[var]], fill = var)) +
        geom_bar() +
        scale_fill_discrete(drop = FALSE) +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(~entry, nrow = Nrow, strip.position = "right") +
        geom_label(
          data = true.df, aes(x = .data[[var]], y = entry, label = deparse("True")),
          show.legend = F, position = position_fill(vjust = 1.5),
          fill = "white",
          alpha = 0.5
        ) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.y = element_text(angle = 0)
        ) +
        labs(title = "Training Data", subtitle = sub.title)


    }

    assign(paste0("Ptrain", i), P)
  }



  # test data ---------------------------------------------------------------

  for(var in var.names){

    imputed <- lapply(imputed.testdata, function(x) x[[var]][addNA.m2[,var]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # originally observed values in the 1st columns
    if(is.data.table(test.data)){
      #data.table
    }else{
      True <- test.data[, var][addNA.m2[, var]]
    }


    if (Nrow > 9) {
      idx <- sample(1:Nrow, 9, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- 9
    }

    combine.df2 <- cbind.data.frame(Mat, True)
    colnames(combine.df2) <- c(paste0("m=", 1:m), "True")
    combine.df2$entry <- factor(1:Nrow, levels = Nrow:1)

    long.df2 <- pivot_longer(data = combine.df2, cols = !entry, names_to = "set", values_to = var)
    long.df2 <- long.df2 %>% mutate(
      set = factor(set, levels=c(paste0("m", 1:m), "True"))
    )

    msg1 <- paste("Distribution of", m, "sets of imputed values")
    msg2 <- paste(paste("for randomly selected", Nrow), "entries")
    msg3 <- paste("in variable", var)
    sub.title <- paste(msg1, msg2, msg3, sep = "\n")


    if (Types[var] == "numeric" | Types[var] == "integer") {
      Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(Nrow + 3)[-(1:3)]
      Reds <- rev(Reds)

      obj <- get(paste0("Ptrain", i))
      xrange <- ggplot_build(obj)$layout$panel_params[[1]]$x.range
      # wrong: xrange=ggplot_build(obj)$layout$panel_scales_x[[1]]$range$range

      true.df2 <- cbind.data.frame(1:Nrow, True)
      colnames(true.df2)[1] <- "entry"


      # true values as points and bars
      P <- ggplot(data = long.df2, aes(x =  .data[[var]])) +
        geom_density_ridges(alpha = 0.5, aes(y = entry, fill = entry)) +
        coord_cartesian(xlim = xrange) +
        scale_fill_manual(values = Reds, guide = guide_legend(reverse = TRUE)) +
        geom_point(data = true.df2, aes(x = True, y = entry), color = "red") +
        geom_segment(data = true.df2, aes(x = True, xend = True, y = as.numeric(entry), yend = as.numeric(entry) + 0.5), color = "red") +
        theme_ridges(center_axis_labels = TRUE) +
        labs(title = "Test Data", subtitle = sub.title)
    } else {
      # factor
      true.df2 <- cbind.data.frame(1:Nrow, True)
      colnames(true.df2) <- c("entry", var)

      P <- ggplot(data = long.df2, aes(x =  .data[[var]], fill = var)) +
        geom_bar() +
        scale_fill_discrete(drop = FALSE) +
        scale_x_discrete(drop = FALSE) +
        facet_wrap(~entry, nrow = Nrow, strip.position = "right") +
        geom_label(
          data = true.df2, aes(x = .data[[var]], y = entry, label = deparse("True")),
          show.legend = F, position = position_fill(vjust = 1.5),
          fill = "white",
          alpha = 0.5
        ) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.y = element_text(angle = 0)
        ) +
        labs(title = "Test Data", subtitle = sub.title)
    }


    assign(paste0("Ptest", i), P)
  }

  for (i in 1:K) {
    par(ask = TRUE)
    a <- get(paste0("Ptrain", i))
    b <- get(paste0("Ptest", i))
    k <- var.names[i]
    if (Types[k] == "numeric" | Types[k] == "integer") {
      # gridExtra::grid.arrange(a,b,nrow=2)
      gridExtra::grid.arrange(a, b, ncol = 2)
    } else {
      gridExtra::grid.arrange(a, b, ncol = 2)
    }
  }


  par(ask = FALSE)
}


plot1D.entriesV2 <- function(obj, var.names = c(1, 2), plot.all = FALSE, train.data = trainNA.df, test.data = testNA.df) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  M <- obj$params$M


  if (length(unique(var.names)) > length(Names)) {
    stop("The number of variables specified in var.names exceeds the number of variables in train.data.")
  }


  if (!is.numeric(var.names)) {
    if (all(var.names %in% Names)) {
      var.names <- match(var.names, Names)
    } else {
      stop("Variable names specified in var.names do not match with the names in train.data")
    }
  }


  addNA.m <- obj$params$addNA.m
  addNA.m2 <- obj$params$addNA.m2


  imputed.traindata <- obj$imputed.traindata
  imputed.testdata <- obj$imputed.testdata


  if (plot.all) {
    K <- length(Names)
    if (length(var.names) != K) {
      var.names <- 1:K
    }
  } else {
    K <- length(var.names)
  }



  # train data --------------------------------------------------------------
  for (i in 1:K) {
    k <- var.names[i]
    imputed <- lapply(imputed.traindata, function(x) x[, k][addNA.m[, k]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = M)

    # originally observed values for this variable
    True <- train.data[, k][addNA.m[, k]]

    # If the number of originall observed values exceed 30, randomly sample 30 entries
    if (Nrow > 50) {
      idx <- sample(1:Nrow, 50, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- 50
    }


    if (Types[k] == "numeric" | Types[k] == "integer") {
      diff.m <- Mat - True
      colnames(diff.m) <- paste0("m=", 1:M)
      diff.df <- as.data.frame(diff.m)
      diff.df$entry <- factor(1:Nrow, levels = Nrow:1)



      long.df <- pivot_longer(data = diff.df, cols = !entry, names_to = "set", values_to = Names[k])
      long.df$set <- as.factor(long.df$set)

      msg1 <- paste("Distribution of differeces between")
      msg2 <- paste("m imputed values and true value")
      msg3 <- paste(paste("for randomly selected", Nrow), "entries")
      msg4 <- paste("in variable", Names[k])

      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")



      Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(Nrow + 3)[-(1:3)]
      Blues <- rev(Blues)

      # true values as points and bars
      P <- ggplot(data = long.df, aes_string(x = Names[k])) +
        geom_density_ridges(scale = 4, rel_min_height = 0.005, size = 0.3, alpha = 0.9, aes(y = entry, fill = entry)) +
        geom_vline(xintercept = 0, colour = "navy", linetype = "longdash") +
        scale_fill_manual(values = Blues) +
        theme_ridges(grid = T) +
        guides(fill = F) +
        theme_ridges(center_axis_labels = TRUE) +
        theme(axis.text.y = element_text(size = 10)) +
        xlab(paste("Error in ", Names[k])) +
        labs(title = "Training Data", subtitle = sub.title)
    } else {
      msg1 <- paste("Distribution of m imputed values")
      msg2 <- paste(paste("for randomly selected", Nrow), "entries")
      msg3 <- paste("in variable", Names[k])
      msg4 <- paste("True values are labelled in text.")

      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")

      # factor
      # combine.df=cbind.data.frame(Mat,True)
      Mat.df <- as.data.frame(Mat)
      colnames(Mat.df) <- c(paste0("m=", 1:M))
      Mat.df$entry <- factor(1:Nrow, levels = 1:Nrow)

      long.df <- pivot_longer(data = Mat.df, cols = !entry, names_to = "set", values_to = Names[k])
      long.df <- dplyr::mutate_if(long.df, is.character, as.factor)
      long.df <- as.data.frame(long.df)

      # reverse levels to change the position of stage 1>2>3>4 from left to right on the plot
      # without this step, the position would be 4>3>2>1 from left to right on the plot.
      revlevel <- rev(levels(long.df[, Names[k]]))
      long.df[, Names[k]] <- factor(long.df[, Names[k]], levels = revlevel)



      true.df <- cbind.data.frame(1:Nrow, True)
      colnames(true.df) <- c("entry", Names[k])
      true.df[, Names[k]] <- as.factor(true.df[, Names[k]])


      # factor
      pos <- rep(NA, Nrow)
      p.table <- prop.table(table(long.df$entry, long.df$stage), margin = 1)
      p.m <- as.matrix(p.table)
      p.m <- p.m[, ncol(p.m):1]

      p.rowsum <- t(apply(p.m, MARGIN = 1, FUN = cumsum))
      p.rowsum <- as.data.frame(p.rowsum)


      True <- as.numeric(True)


      ## vectorised
      pos.m <- cbind(rep(0, Nrow), p.rowsum)
      L <- ncol(pos.m)
      Pos.M <- (pos.m[, 2:L] + pos.m[, 1:(L - 1)]) / 2


      for (j in 1:Nrow) {
        if (p.m[j, True[j]] == 0) {

          # if the m imputations do not contain the true value, plot the true value outside (position= -0.1 on x-axis)
          pos[j] <- -0.1
        } else {

          # if(True[j]==1){
          #   pos[j]=p.rowsum[j,True[j]]/2
          # }else{
          #   pos[j]=(p.rowsum[j,True[j]]+p.rowsum[j,True[j]-1])/2
          # }

          pos[j] <- Pos.M[j, True[j]]
        }
      }

      true.df$pos <- pos



      P <- ggplot(data = long.df, aes_string(x = "entry", fill = Names[k])) +
        geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set2") +
        geom_text(data = true.df, aes(label = true.df[, Names[k]], x = entry, y = pos), colour = "navy") +
        labs(title = "Training Data", subtitle = sub.title) +
        ylab("proportion") +
        coord_flip() +
        theme(panel.grid.major = element_blank())

      # panel.background = element_blank()

      # P=ggplot(data=long.df,aes(x=entry,fill=forcats::fct_rev(study)))+
      # geom_bar(position="fill")+
      # scale_fill_brewer(palette="Set2")+
      # geom_text(data=true.df,aes(label="T",x=entry,y=pos))+
      # labs(title="Training Data",subtitle = sub.title)+
      # ylab("proportion")+
      # coord_flip()
    }

    assign(paste0("Ptrain", i), P)
  }



  # test data ---------------------------------------------------------------

  for (i in 1:K) {
    k <- var.names[i]
    imputed <- lapply(imputed.testdata, function(x) x[, k][addNA.m2[, k]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = M)

    # originally observed values in the 1st columns
    True <- test.data[, k][addNA.m2[, k]]

    if (Nrow > 50) {
      idx <- sample(1:Nrow, 50, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- 50
    }




    if (Types[k] == "numeric" | Types[k] == "integer") {
      diff.m2 <- Mat - True
      colnames(diff.m2) <- paste0("m=", 1:M)
      diff.df2 <- as.data.frame(diff.m2)
      diff.df2$entry <- factor(1:Nrow, levels = Nrow:1)



      long.df2 <- pivot_longer(data = diff.df2, cols = !entry, names_to = "set", values_to = Names[k])
      long.df2$set <- as.factor(long.df2$set)

      msg1 <- paste("Distribution of differeces between")
      msg2 <- paste("m imputed values and true value")
      msg3 <- paste(paste("for randomly selected", Nrow), "entries")
      msg4 <- paste("in variable", Names[k])
      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")
      Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(Nrow + 3)[-(1:3)]
      Reds <- rev(Reds)

      obj <- get(paste0("Ptrain", i))
      xrange <- ggplot_build(obj)$layout$panel_params[[1]]$x.range
      # wrong: xrange=ggplot_build(obj)$layout$panel_scales_x[[1]]$range$range

      # true values as points and bars
      P <- ggplot(data = long.df2, aes_string(x = Names[k])) +
        geom_density_ridges(scale = 4, rel_min_height = 0.005, size = 0.3, alpha = 0.9, aes(y = entry, fill = entry)) +
        geom_vline(xintercept = 0, colour = "red3", linetype = "longdash") +
        scale_fill_manual(values = Reds) +
        theme_ridges(grid = T) +
        guides(fill = F) +
        theme_ridges(center_axis_labels = TRUE) +
        theme(axis.text.y = element_text(size = 10)) +
        xlab(paste("Error in ", Names[k])) +
        labs(title = "Test Data", subtitle = sub.title)
    } else {
      msg1 <- paste("Distribution of m imputed values")
      msg2 <- paste(paste("for randomly selected", Nrow), "entries")
      msg3 <- paste("in variable", Names[k])
      msg4 <- paste("True values are labelled in text.")

      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")


      # factor
      # factor
      # combine.df=cbind.data.frame(Mat,True)
      Mat.df <- as.data.frame(Mat)
      colnames(Mat.df) <- c(paste0("m=", 1:M))
      Mat.df$entry <- factor(1:Nrow, levels = 1:Nrow)

      long.df2 <- pivot_longer(data = Mat.df, cols = !entry, names_to = "set", values_to = Names[k])
      long.df2 <- dplyr::mutate_if(long.df2, is.character, as.factor)
      long.df2 <- as.data.frame(long.df2)

      # reverse levels to change the position of stage 1>2>3>4 from left to right on the plot
      # without this step, the position would be 4>3>2>1 from left to right on the plot.
      revlevel <- rev(levels(long.df2[, Names[k]]))
      long.df2[, Names[k]] <- factor(long.df2[, Names[k]], levels = revlevel)



      true.df2 <- cbind.data.frame(1:Nrow, True)
      colnames(true.df2) <- c("entry", Names[k])
      true.df2[, Names[k]] <- as.factor(true.df2[, Names[k]])


      # factor
      pos <- rep(NA, Nrow)
      p.table <- prop.table(table(long.df2$entry, long.df2$stage), margin = 1)
      p.m <- as.matrix(p.table)
      p.m <- p.m[, ncol(p.m):1]

      p.rowsum <- t(apply(p.m, MARGIN = 1, FUN = cumsum))
      p.rowsum <- as.data.frame(p.rowsum)


      True <- as.numeric(True)


      ## vectorised
      pos.m <- cbind(rep(0, Nrow), p.rowsum)
      L <- ncol(pos.m)
      Pos.M <- (pos.m[, 2:L] + pos.m[, 1:(L - 1)]) / 2


      for (j in 1:Nrow) {
        if (p.m[j, True[j]] == 0) {

          # if the m imputations do not contain the true value, plot the true value outside (position= -0.1 on x-axis)
          pos[j] <- -0.1
        } else {

          # if(True[j]==1){
          #   pos[j]=p.rowsum[j,True[j]]/2
          # }else{
          #   pos[j]=(p.rowsum[j,True[j]]+p.rowsum[j,True[j]-1])/2
          # }

          pos[j] <- Pos.M[j, True[j]]
        }
      }


      true.df2$pos <- pos


      P <- ggplot(data = long.df2, aes_string(x = "entry", fill = Names[k])) +
        geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set2") +
        geom_text(data = true.df2, aes(label = true.df2[, Names[k]], x = entry, y = pos), colour = "red3") +
        labs(title = "Test Data", subtitle = sub.title) +
        ylab("proportion") +
        coord_flip() +
        theme(panel.grid.major = element_blank())
    }


    assign(paste0("Ptest", i), P)
  }

  for (i in 1:K) {
    par(ask = TRUE)
    a <- get(paste0("Ptrain", i))
    b <- get(paste0("Ptest", i))
    k <- var.names[i]
    if (Types[k] == "numeric" | Types[k] == "integer") {
      gridExtra::grid.arrange(a, b, ncol = 2)
    } else {
      gridExtra::grid.arrange(a, b, ncol = 2)
    }
  }


  par(ask = FALSE)
}





#

plot1D.imputations <- function(obj, var.names = c(1, 2), plot.all = FALSE, train.data = trainNA.df, test.data = testNA.df) {
  Names <- obj$params$Names
  Types <- obj$params$Types
  M <- obj$params$M


  if (length(unique(var.names)) > length(Names)) {
    stop("The number of variables specified in var.names exceeds the number of variables in train.data.")
  }


  if (!is.numeric(var.names)) {
    if (all(var.names %in% Names)) {
      var.names <- match(var.names, Names)
    } else {
      stop("Variable names specified in var.names do not match with the names in train.data")
    }
  }


  addNA.m <- obj$params$addNA.m
  addNA.m2 <- obj$params$addNA.m2


  imputed.traindata <- obj$imputed.traindata
  imputed.testdata <- obj$imputed.testdata


  if (plot.all) {
    K <- length(Names)
    if (length(var.names) != K) {
      var.names <- 1:K
    }
  } else {
    K <- length(var.names)
  }


  # train data --------------------------------------------------------------



  for (i in 1:K) {
    k <- var.names[i]
    imputed <- lapply(imputed.traindata, function(x) x[, k][addNA.m[, k]])

    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = M)

    # original observed values for this variable
    True <- train.data[, k][addNA.m[, k]]

    combine.df <- cbind.data.frame(Mat, True)
    colnames(combine.df) <- c(paste0("m=", 1:M), "True")
    long.df <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = Names[k])


    sub.title <- paste(paste("Distribution of m imputed values", "in variable", sep = "\n"), Names[k])


    if (Types[k] == "numeric" | Types[k] == "integer") {
      long.df$set <- factor(long.df$set, levels = c(paste0("m=", M:1), "True"))
      # long.df$set=factor(long.df$set,levels=c(M:1,"True"))


      Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(M + 3)[-(1:3)]
      Blues <- rev(Blues)
      traincolor <- c(Blues, "#030303")

      # true values as points and bars
      P <- ggplot(data = long.df, aes_string(x = Names[k])) +
        geom_density_ridges(alpha = 0.5, aes(y = set, fill = set)) +
        scale_fill_manual(values = traincolor, guide = guide_legend(reverse = TRUE)) +
        theme_ridges(center_axis_labels = TRUE) +
        labs(title = "Training Data", subtitle = sub.title)
    } else {
      # factor

      long.df$set <- factor(long.df$set, levels = c("True", paste0("m=", 1:M)))

      # Blues=rep("#08306B",M)
      Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(M + 3)[-(1:3)]
      traincolor <- c("#030303", Blues)


      P <- ggplot(data = long.df, aes_string(x = Names[k], alpha = Names[k])) +
        geom_bar(aes(fill = set)) +
        scale_alpha_discrete(range = c(0.5, 1)) +
        # scale_alpha_manual(values =seq(0.5,1,length.out=nlevels(combine.df$value)))+
        # scale_fill_discrete(drop=FALSE) +
        # scale_x_discrete(drop=FALSE)+
        # geom_label(data=true.df,aes(x=value,y=entry,label="True"),vjust="bottom")+
        facet_wrap(~set, nrow = M + 1) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color = "black")
        ) +
        labs(title = "Training Data", subtitle = sub.title) +
        scale_fill_manual(values = traincolor)
    }

    assign(paste0("Ptrain", i), P)
  }



  # test data ---------------------------------------------------------------


  for (i in 1:K) {
    k <- var.names[i]
    imputed <- lapply(imputed.testdata, function(x) x[, k][addNA.m2[, k]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = M)

    # originally observed values
    True <- test.data[, k][addNA.m2[, k]]

    combine.df2 <- cbind.data.frame(Mat, True)
    colnames(combine.df2) <- c(paste0("m=", 1:M), "True")
    long.df2 <- pivot_longer(data = combine.df2, cols = everything(), names_to = "set", values_to = Names[k])

    sub.title <- paste(paste("Distribution of m imputed values", "in variable", sep = "\n"), Names[k])

    if (Types[k] == "numeric" | Types[k] == "integer") {
      long.df2$set <- factor(long.df2$set, levels = c(paste0("m=", M:1), "True"))


      Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 3)[-(1:3)]
      Reds <- rev(Reds)
      testcolor <- c(Reds, "#030303")

      obj <- get(paste0("Ptrain", i))
      xrange <- ggplot_build(obj)$layout$panel_params[[1]]$x.range

      # xrange=ggplot_build(obj)$layout$panel_scales_x[[1]]$range$range

      # true values as points and bars
      P <- ggplot(data = long.df2, aes_string(x = Names[k])) +
        geom_density_ridges(alpha = 0.5, aes(y = set, fill = set)) +
        coord_cartesian(xlim = xrange) +
        scale_fill_manual(values = testcolor, guide = guide_legend(reverse = TRUE)) +
        theme_ridges(center_axis_labels = TRUE) +
        labs(title = "Test Data", subtitle = sub.title)
    } else {
      long.df2$set <- factor(long.df2$set, levels = c("True", paste0("m=", 1:M)))

      # Reds=rep("#7F0000",M)
      Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 3)[-(1:3)]
      testcolor <- c("#030303", Reds)


      P <- ggplot(data = long.df2, aes_string(x = Names[k], alpha = Names[k])) +
        geom_bar(aes(fill = set)) +
        scale_alpha_discrete(range = c(0.5, 1)) +
        facet_wrap(~set, nrow = M + 1) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color = "black")
        ) +
        labs(title = "Test Data", subtitle = sub.title) +
        scale_fill_manual(values = testcolor)
    }


    assign(paste0("Ptest", i), P)
  }

  for (i in 1:K) {
    par(ask = TRUE)
    a <- get(paste0("Ptrain", i))
    b <- get(paste0("Ptest", i))
    k <- var.names[i]
    if (Types[k] == "numeric") {
      gridExtra::grid.arrange(a, b, nrow = 2)
    } else {
      gridExtra::grid.arrange(a, b, ncol = 2)
    }
  }



  par(ask = FALSE)
}


#


plot2D <- function(obj, var.names = c(1, 2), train.data = trainNA.df, test.data = testNA.df, plotfac.type = 1) {
  if (length(var.names) != 2) {
    stop("var.names can only have two values: either the names of two variables or the indices of two variables.")
  }



  Names <- obj$params$Names
  Types <- obj$params$Types
  M <- obj$params$M

  if (!is.numeric(var.names)) {
    if (all(var.names %in% Names)) {
      var.names <- match(var.names, Names)
    } else {
      stop("At least one variable specified in var.names do not match with any variable name in train.data")
    }
  }

  addNA.m <- obj$params$addNA.m
  addNA.m2 <- obj$params$addNA.m2

  # trainNA.m=obj$params$trainNA.m
  # testNA.m=obj$params$testNA.m

  imputed.traindata <- obj$imputed.traindata
  imputed.testdata <- obj$imputed.testdata


  # train data --------------------------------------------------------------


  # originally missing values
  missing <- which(is.na(train.data[, var.names[1]]) | is.na(train.data[var.names[2]]))
  # missing=which(trainNA.m[,var.names[1]]| trainNA.m[,var.names[2]])

  # originally observed values (make missing)
  observed <- which(addNA.m[, var.names[1]] | addNA.m[, var.names[2]])

  idx <- observed[!observed %in% missing]
  true.df <- train.data[idx, var.names]


  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])

  imputed <- lapply(imputed.traindata, function(x) x[idx, var.names])
  imputed.df <- do.call(rbind.data.frame, imputed)

  combine.df <- rbind(true.df, imputed.df)

  nobs <- length(idx)

  combine.df$M <- rep(c("True", 1:M), each = nobs)
  combine.df$M <- factor(combine.df$M, levels = c("True", 1:M))

  var.names <- colnames(combine.df)


  Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(M + 4)[-(1:4)]
  traincolor <- c("#030303", Blues)


  if (all(Types[var.names] == "numeric" | Types[var.names] == "integer")) {
    # both numeric: scatterplot

    sub.title <- paste("Distribution of overimputed values:", paste(var.names[2], "vs", var.names[1]), sep = "\n")

    Ptrain <- ggplot(data = combine.df, aes_string(x = var.names[1], y = var.names[2])) +
      geom_point(alpha = 0.6, aes(colour = factor(M))) +
      facet_grid(rows = vars(M)) +
      scale_colour_manual(values = traincolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Training Data", subtitle = sub.title)
  } else if (all(Types[var.names] == "factor")) {
    # both factor
    sub.title <- paste("Distribution of overimputed values:", paste(var.names[2], "vs", var.names[1]), sep = "\n")

    if (plotfac.type == 1) {
      Ptrain <- ggplot(data = combine.df, aes_string(x = var.names[1], fill = var.names[2])) +
        geom_bar() +
        facet_wrap(~M, nrow = M + 1) +
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
  # original missing values
  missing2 <- which(is.na(test.data[, var.names[1]]) | is.na(test.data[var.names[2]]))
  # original observed values (make missing)
  observed2 <- which(addNA.m2[, var.names[1]] | addNA.m2[, var.names[2]])

  idx2 <- observed2[!observed2 %in% missing2]
  true.df2 <- test.data[idx2, var.names]


  # bothNA=which(addNA.m[,var.names[1]] & addNA.m[,var.names[2]])
  # plot(true.df$Sepal.Length~true.df$Petal.Length)
  # plot(imputed.data[[1]]$Sepal.Length[idx]~imputed.data[[1]]$Petal.Length[idx])

  imputed2 <- lapply(imputed.testdata, function(x) x[idx2, var.names])
  imputed.df2 <- do.call(rbind.data.frame, imputed2)

  combine.df2 <- rbind(true.df2, imputed.df2)

  nobs <- length(idx2)

  combine.df2$M <- rep(c("True", 1:M), each = nobs)
  combine.df2$M <- factor(combine.df2$M, levels = c("True", 1:M))

  Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(M + 4)[-(1:4)]
  testcolor <- c("#030303", Reds)

  xrange <- ggplot_build(Ptrain)$layout$panel_scales_x[[1]]$range$range
  yrange <- ggplot_build(Ptrain)$layout$panel_scales_y[[1]]$range$range

  if (all(Types[var.names] == "numeric" | Types[var.names] == "integer")) {
    # both numeric
    sub.title <- paste("Distribution of overimputed values:", paste(var.names[2], "vs", var.names[1]), sep = "\n")

    Ptest <- ggplot(data = combine.df2, aes_string(x = var.names[1], y = var.names[2])) +
      geom_point(alpha = 0.6, aes(colour = factor(M))) +
      coord_cartesian(xlim = xrange, ylim = yrange) +
      facet_grid(rows = vars(M)) +
      scale_colour_manual(values = testcolor) +
      theme(strip.text.y = element_text(angle = 0)) +
      labs(title = "Test Data", subtitle = sub.title)

    gridExtra::grid.arrange(Ptrain, Ptest, ncol = 2)
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
}
