
#plot actual for each entry
#' @importFrom tibble is_tibble
#' @importFrom gridExtra grid.arrange
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom forcats fct_rev
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_point geom_segment geom_label geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete scale_x_discrete guides theme element_text element_blank

plot1D.entries <- function(obj, var.name, train.data, test.data = NULL, n.entries = 20) {

  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m

  if(!var.name %in% Names){
    stop("The variable name specified in `var.name` does not exist in train.data")
  }


  addNA.m <- obj$params$addNA.m



  imputed.traindata <- obj$imputed.traindata




  # train data --------------------------------------------------------------



    imputed <- lapply(imputed.traindata, function(x) x[[var.name]][addNA.m[,var.name]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # originally observed values for this variable
     if(is.data.table(train.data)| is_tibble(train.data)){
      #data.table
      train.data <- as.data.frame(train.data)
    }

    #data.frame
    True <- train.data[, var.name][addNA.m[, var.name]]

    # If the number of originall observed values exceed n.entries, randomly sample n.entries entries
    if (Nrow > n.entries) {
      idx <- sample(1:Nrow, n.entries, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- n.entries
    }

    combine.df <- cbind.data.frame(Mat, True)
    colnames(combine.df) <- c(paste0("m", 1:m), "True")
    combine.df$entry <- factor(1:Nrow, levels = Nrow:1)

    long.df <- pivot_longer(data = combine.df, cols = !entry, names_to = "set", values_to = var.name)
    long.df$set <- factor(long.df$set, levels=c(paste0("m", 1:m), "True"))



    msg1 <- paste("Distribution of", m, "sets of imputed values")
    msg2 <- paste(paste("for randomly selected", Nrow), "entries")
    msg3 <- paste("in variable", var.name)
    sub.title <- paste(msg1, msg2, msg3, sep = "\n")


    #blue

    colfunc <- colorRampPalette(c("#0c71ff","#bcd8ff"))
    traincolor<-colfunc(Nrow)



    if (Types[var.name] == "numeric" | Types[var.name] == "integer") {
      true.df <- cbind.data.frame(1:Nrow, True)
      colnames(true.df)[1] <- c("entry")

      #Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(Nrow + 3)[-(1:3)]
      #Blues <- rev(Blues)

      # true values as points and bars
      P1 <- ggplot(data = long.df, aes(x = .data[[var.name]])) +
        geom_density_ridges(alpha = 0.5, aes(y = entry, fill = entry)) +
        scale_fill_manual(values = traincolor) +
        geom_point(data = true.df, aes(x = True, y = entry,color="True")) +
        geom_segment(data = true.df, aes(x = True, xend = True, y = as.numeric(entry), yend = as.numeric(entry) + 0.5,color="True")) +
        scale_color_manual(values = "royalblue2")+
         theme_ridges(center_axis_labels = TRUE) +
        labs(title = "Training Data", subtitle = sub.title, color = "Masked True")+
        guides(fill="none",alpha="none")+
        theme(legend.position = "bottom",
              legend.justification="centre",
              legend.text = element_blank())

    } else {
      true.df <- cbind.data.frame(1:Nrow, True)
      colnames(true.df) <- c("entry", var.name)

      #Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(Nrow + 3)[-(1:3)]
      #Blues <- rev(Blues)
      true.df$entry <- factor(1:Nrow, levels = Nrow:1)

      # factor
      P1 <- ggplot(data = long.df, aes(x = .data[[var.name]], fill = entry)) +
        geom_bar() +
        scale_fill_manual(values = traincolor)+
        #scale_fill_discrete(drop = FALSE) +
        scale_x_discrete(drop = FALSE) +
        facet_grid(vars(fct_rev(entry)),switch = "y")+
        #facet_wrap(~entry, nrow = Nrow, strip.position = "right") +
        geom_label(
          data = true.df, aes(x = .data[[var.name]], y = entry, label = bquote("True")),
          show.legend = F,position = position_fill(vjust = 4),size=2.5,
          fill = "white",
          alpha = 0.5
        ) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.y.left= element_text(angle = 0)
        ) +
        labs(title = "Training Data", subtitle = sub.title)+
        guides(fill="none",alpha="none")




    }




  # test data ---------------------------------------------------------------

    if(!is.null(test.data)){
      addNA.m2 <- obj$params$addNA.m2
      imputed.testdata <- obj$imputed.testdata


      imputed <- lapply(imputed.testdata, function(x) x[[var.name]][addNA.m2[,var.name]])
      Nrow <- length(imputed[[1]])

      L <- unlist(imputed)
      Mat <- matrix(L, nrow = Nrow, ncol = m)

      # originally observed values in the 1st columns

      if(is.data.table(test.data)| is_tibble(test.data)){
        #data.table
        test.data <- as.data.frame(test.data)
      }


      True <- test.data[, var.name][addNA.m2[, var.name]]


      if (Nrow > n.entries) {
        idx <- sample(1:Nrow, n.entries, replace = F)
        Mat <- Mat[idx, ]
        True <- True[idx]
        Nrow <- n.entries
      }

      combine.df2 <- cbind.data.frame(Mat, True)
      colnames(combine.df2) <- c(paste0("m=", 1:m), "True")
      combine.df2$entry <- factor(1:Nrow, levels = Nrow:1)

      long.df2 <- pivot_longer(data = combine.df2, cols = !entry, names_to = "set", values_to = var.name)
      long.df2$set <- factor(long.df2$set, levels=c(paste0("m", 1:m), "True"))

      msg1 <- paste("Distribution of", m, "sets of imputed values")
      msg2 <- paste(paste("for randomly selected", Nrow), "entries")
      msg3 <- paste("in variable", var.name)
      sub.title <- paste(msg1, msg2, msg3, sep = "\n")


      colfunc <- colorRampPalette(c("#FFBF00","#ffe69d"))
      testcolor<-colfunc(Nrow)


      if (Types[var.name] == "numeric" | Types[var.name] == "integer") {
        #Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(Nrow + 3)[-(1:3)]
        #Reds <- rev(Reds)


        xrange <- ggplot_build(P1)$layout$panel_params[[1]]$x.range
        # wrong: xrange=ggplot_build(obj)$layout$panel_scales_x[[1]]$range$range

        true.df2 <- cbind.data.frame(1:Nrow, True)
        colnames(true.df2)[1] <- "entry"


        # true values as points and bars
        P2 <- ggplot(data = long.df2, aes(x =  .data[[var.name]])) +
          geom_density_ridges(alpha = 0.5, aes(y = entry, fill = entry)) +
          coord_cartesian(xlim = xrange) +
          scale_fill_manual(values = testcolor) +
          geom_point(data = true.df2, aes(x = True, y = entry,color="True")) +
          geom_segment(data = true.df2, aes(x = True, xend = True, y = as.numeric(entry), yend = as.numeric(entry) + 0.5,color="True")) +
          scale_color_manual(values = "darkorange1")+
          theme_ridges(center_axis_labels = TRUE) +
          labs(title = "Test Data", subtitle = sub.title, color = "Masked True")+
          guides(fill="none",alpha="none")+
          theme(legend.position = "bottom",
                legend.justification="centre",
                legend.text = element_blank())



      } else {
        # factor
        true.df2 <- cbind.data.frame(1:Nrow, True)
        colnames(true.df2) <- c("entry", var.name)

        #Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(Nrow + 3)[-(1:3)]
        #Reds <- rev(Reds)

        P2 <- ggplot(data = long.df2, aes(x =  .data[[var.name]], fill = entry)) +
          geom_bar() +
          scale_fill_manual(values = testcolor)+
          #scale_fill_discrete(drop = FALSE) +
          scale_x_discrete(drop = FALSE) +
          facet_grid(vars(fct_rev(entry)),switch = "y")+
          geom_label(
            data = true.df, aes(x = .data[[var.name]], y = entry, label = bquote("True")),
            show.legend = F,position = position_fill(vjust = 4),size=2.5,
            fill = "white",
            alpha = 0.5
          ) +
          theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            strip.text.y = element_text(angle = 0)
          ) +
          labs(title = "Test Data", subtitle = sub.title)+
          guides(fill="none",alpha="none")
      }

      gridExtra::grid.arrange(P1, P2, ncol = 2)

    }else{
      P1
    }







}



#plot error for each entry

plot1D.entriesV2 <- function(obj, var.name, train.data, test.data = NULL, n.entries = 20) {

  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m


  if(!var.name %in% Names){
    stop("The variable name specified in `var.name` does not exist in train.data")
  }

  addNA.m <- obj$params$addNA.m


  imputed.traindata <- obj$imputed.traindata




  # train data --------------------------------------------------------------


    imputed <- lapply(imputed.traindata, function(x) x[[var.name]][addNA.m[, var.name]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # originally observed values for this variable

    if(is.data.table(train.data)| is_tibble(train.data)){
      #data.table
      train.data <- as.data.frame(train.data)
    }

    #data.frame
    True <- train.data[, var.name][addNA.m[, var.name]]

    # If the number of originall observed values exceed n.entries, randomly sample n.entries entries
    if (Nrow > n.entries) {
      idx <- sample(1:Nrow, n.entries, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- n.entries
    }

    colfunc <- colorRampPalette(c("#0c71ff","#bcd8ff"))
    traincolor<-colfunc(Nrow)

    if (Types[var.name] == "numeric" | Types[var.name] == "integer") {
      diff.m <- Mat - True
      colnames(diff.m) <- paste0("m", 1:m)
      diff.df <- as.data.frame(diff.m)
      diff.df$entry <- factor(1:Nrow, levels = Nrow:1)



      long.df <- pivot_longer(data = diff.df, cols = !entry, names_to = "set", values_to = var.name)
      long.df$set <- factor(long.df$set, levels=paste0("m", 1:m))

      msg1 <- paste("Distribution of differeces between")
      msg2 <- paste(m,"imputed values and true value")
      msg3 <- paste(paste("for randomly selected", Nrow), "entries")
      msg4 <- paste("in variable", var.name)

      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")



      #Blues <- colorRampPalette(brewer.pal(name = "Blues", n = 9))(Nrow + 3)[-(1:3)]
      #Blues <- rev(Blues)

      # true values as points and bars
      P1 <- ggplot(data = long.df,  aes(x = .data[[var.name]])) +
        geom_density_ridges(scale = 4, rel_min_height = 0.005, size = 0.3, alpha = 0.9, aes(y = entry, fill = entry)) +
        geom_vline(xintercept = 0, colour = "navy", linetype = "longdash") +
        scale_fill_manual(values = traincolor) +
        theme_ridges(grid = T) +
        guides(fill = "none") +
        theme_ridges(center_axis_labels = TRUE) +
        theme(axis.text.y = element_text(size = 10)) +
        xlab(paste("Error in ", var.name)) +
        labs(title = "Training Data", subtitle = sub.title)

    } else {

      msg1 <- paste("Distribution of ", m, "imputed values")
      msg2 <- paste(paste("for randomly selected", Nrow), "entries")
      msg3 <- paste("in variable", var.name)
      msg4 <- paste("True values are labelled in text.")

      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")

      # factor
      # combine.df=cbind.data.frame(Mat,True)
      Mat.df <- as.data.frame(Mat)
      colnames(Mat.df) <- c(paste0("m", 1:m))
      Mat.df$entry <- factor(1:Nrow, levels = 1:Nrow)

      long.df <- pivot_longer(data = Mat.df, cols = !entry, names_to = "set", values_to = var.name)
      #long.df <- dplyr::mutate_if(long.df, is.character, as.factor)
      long.df <- as.data.frame(long.df)

      # reverse levels to change the position of stage 1>2>3>4 from left to right on the plot
      # without this step, the position would be 4>3>2>1 from left to right on the plot.
      #revlevel <- rev(levels(train.data[, var.name]))
      revlevel <- levels(train.data[, var.name])
      long.df[, var.name] <- factor(long.df[, var.name], levels = revlevel)



      true.df <- cbind.data.frame(1:Nrow, True)
      colnames(true.df) <- c("entry", var.name)
      true.df[, var.name] <- factor(true.df[, var.name],levels = revlevel)


      # factor
      pos <- rep(NA, Nrow)
      p.table <- prop.table(table(long.df$entry, long.df[[var.name]]), margin = 1)
      p.m <- as.matrix(p.table)
      #p.m <- p.m[, ncol(p.m):1]

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
          #pos[j] <- -0.05

          if(True[j] == 1){
            pos[j] <- -0
          }else if(True[j] == length(revlevel)){
            pos[j] <- 1
          }else{
            pos[j]<-Pos.M[j,True[j]]
          }


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



      P1 <- ggplot(data = long.df, aes(x = entry, fill = .data[[var.name]])) +
        geom_bar(position = position_fill(reverse = TRUE))+
        #geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set2",drop = FALSE) +
        geom_text(data = true.df, aes(label = true.df[, var.name], x = entry, y = pos), colour = "navy") +
        labs(title = "Training Data", subtitle = sub.title) +
        ylab("proportion") +
        coord_flip() +
        theme(panel.grid.major = element_blank(),
              legend.position="bottom")


    }





  # test data ---------------------------------------------------------------

    if(!is.null(test.data)){
      addNA.m2 <- obj$params$addNA.m2
      imputed.testdata <- obj$imputed.testdata

    imputed <- lapply(imputed.testdata, function(x) x[[var.name]][addNA.m2[, var.name]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # originally observed values in the 1st columns
    if(is.data.table(test.data)| is_tibble(test.data)){
      #data.table
      test.data <- as.data.frame(test.data)
    }


    True <- test.data[, var.name][addNA.m2[, var.name]]


    if (Nrow > n.entries) {
      idx <- sample(1:Nrow, n.entries, replace = F)
      Mat <- Mat[idx, ]
      True <- True[idx]
      Nrow <- n.entries
    }

    colfunc <- colorRampPalette(c("#FFBF00","#ffe69d"))
    testcolor<-colfunc(Nrow)


    if (Types[var.name] == "numeric" | Types[var.name] == "integer") {
      diff.m2 <- Mat - True
      colnames(diff.m2) <- paste0("m", 1:m)
      diff.df2 <- as.data.frame(diff.m2)
      diff.df2$entry <- factor(1:Nrow, levels = Nrow:1)



      long.df2 <- pivot_longer(data = diff.df2, cols = !entry, names_to = "set", values_to = var.name)
      #long.df2$set <- as.factor(long.df2$set)
      long.df2$set <- factor(long.df2$set, levels=paste0("m", 1:m))

      msg1 <- paste("Distribution of differeces between")
      msg2 <- paste(m, "imputed values and true value")
      msg3 <- paste(paste("for randomly selected", Nrow), "entries")
      msg4 <- paste("in variable", var.name)
      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")

      #Reds <- colorRampPalette(brewer.pal(name = "OrRd", n = 9))(Nrow + 3)[-(1:3)]
      #Reds <- rev(Reds)

      #obj <- get(paste0("Ptrain", i))
      xrange <- ggplot_build(P1)$layout$panel_params[[1]]$x.range
      # wrong: xrange=ggplot_build(obj)$layout$panel_scales_x[[1]]$range$range

      # true values as points and bars
      P2 <- ggplot(data = long.df2, aes(x =  .data[[var.name]])) +
        geom_density_ridges(scale = 4, rel_min_height = 0.005, size = 0.3, alpha = 0.9, aes(y = entry, fill = entry)) +
        geom_vline(xintercept = 0, colour = "red3", linetype = "longdash") +
        coord_cartesian(xlim = xrange) +
        scale_fill_manual(values = testcolor) +
        theme_ridges(grid = T) +
        guides(fill = "none") +
        theme_ridges(center_axis_labels = TRUE) +
        theme(axis.text.y = element_text(size = 10)) +
        xlab(paste("Error in ", var.name)) +
        labs(title = "Test Data", subtitle = sub.title)
    } else {

      msg1 <- paste("Distribution of ", m, "imputed values")
      msg2 <- paste(paste("for randomly selected", Nrow), "entries")
      msg3 <- paste("in variable", var.name)
      msg4 <- paste("True values are labelled in text.")

      sub.title <- paste(msg1, msg2, msg3, msg4, sep = "\n")


      # factor
      # factor
      # combine.df=cbind.data.frame(Mat,True)
      Mat.df <- as.data.frame(Mat)
      colnames(Mat.df) <- c(paste0("m", 1:m))
      Mat.df$entry <- factor(1:Nrow, levels = 1:Nrow)

      long.df2 <- pivot_longer(data = Mat.df, cols = !entry, names_to = "set", values_to = var.name)
      #long.df2 <- dplyr::mutate_if(long.df2, is.character, as.factor)
      long.df2 <- as.data.frame(long.df2)

      # reverse levels to change the position of stage 1>2>3>4 from left to right on the plot
      # without this step, the position would be 4>3>2>1 from left to right on the plot.
      #revlevel <- rev(levels(test.data[, var.name]))
      revlevel <- levels(test.data[, var.name])
      long.df2[, var.name] <- factor(long.df2[, var.name], levels = revlevel)



      true.df2 <- cbind.data.frame(1:Nrow, True)
      colnames(true.df2) <- c("entry", var.name)
      true.df2[, var.name] <- factor(true.df2[, var.name], levels = revlevel)


      # factor
      pos <- rep(NA, Nrow)
      p.table <- prop.table(table(long.df2$entry, long.df2[[var.name]]), margin = 1)
      p.m <- as.matrix(p.table)
      #p.m <- p.m[, ncol(p.m):1]

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
          #pos[j] <- -0.1

          if(True[j] == 1){
            pos[j] <- 0
          }else if(True[j] == length(revlevel)){
            pos[j] <- 1
          }else{
            pos[j]<-Pos.M[j,True[j]]
          }

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


      P2 <- ggplot(data = long.df2, aes(x = entry, fill = .data[[var.name]])) +
        geom_bar(position = position_fill(reverse = TRUE))+
        #geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set2",drop = FALSE) +
        geom_text(data = true.df2, aes(label = true.df2[, var.name], x = entry, y = pos), colour = "red3") +
        labs(title = "Test Data", subtitle = sub.title) +
        ylab("proportion") +
        coord_flip() +
        theme(panel.grid.major = element_blank(),
              legend.position="bottom")
    }

      gridExtra::grid.arrange(P1, P2, ncol = 2)

    }else{
      P1
    }



}



