#' plot continuous variables using trelliscope
#' @importFrom trelliscopejs facet_trelliscope
#' @importFrom tibble is_tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank

plotcon.trelliscope <- function(obj, train.data, test.data = NULL,type="ridges") {

Names <- obj$params$Names
Types <- obj$params$Types
m <- obj$params$m


#matrix
addNA.m <- obj$params$addNA.m
#data.table
imputed.traindata <- obj$imputed.traindata


continuous.vars <- Names[Types == "numeric" | Types == "integer"]


var.list<-vector("list",length=length(continuous.vars))
names(var.list)<-continuous.vars

for ( var.name in continuous.vars){
  imputed <- lapply(imputed.traindata, function(x) x[[var.name]][addNA.m[,var.name]])
  Nrow <- length(imputed[[1]])

  L <- unlist(imputed)
  Mat <- matrix(L, nrow = Nrow, ncol = m)

  # original observed values for this variable
  if(is.data.table(train.data)| is_tibble(train.data)){
    #data.table
    train.data <- as.data.frame(train.data)
  }

  #data.frame
  True <- train.data[, var.name][addNA.m[, var.name]]

  combine.df <- cbind.data.frame(Mat, True)
  colnames(combine.df) <- c(paste0("m", 1:m), "True")
  var.list[[var.name]] <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = "value")
  var.list[[var.name]] <- var.list[[var.name]]%>% tibble::add_column(variable = var.name)

  }



combine.tb<-do.call(rbind,var.list)



if(is.null(test.data)){
  #only plot training data

  if(type=="ridges"){
    #blue
    combine.tb$set <- factor(combine.tb$set, levels = c(paste0("m", m:1), "True"))
    #light: #bcd8ff
    #dark: #0c71ff
    colfunc <- colorRampPalette(c("#0c71ff","#bcd8ff"))
    traincolor<-c(colfunc(m),"gray40")

    ggplot(data =combine.tb , aes(x = value)) +
      geom_density_ridges(alpha = 0.8, aes(y = set, fill = set)) +
      scale_fill_manual(values = traincolor) +
      guides(fill="none")+
      theme_ridges(center_axis_labels = TRUE) +
      labs(title = "Training Data", y = "Imputed sets")+
      facet_trelliscope(~ variable, nrow = 2, ncol = 5,scales = "free")

  }else if (type=="density"){

    #blue
    combine.tb$set <- factor(combine.tb$set, levels = c("True",paste0("m", 1:m)))
    #light: #bcd8ff
    #dark: #0c71ff
    colfunc <- colorRampPalette(c("#bcd8ff","#0c71ff"))
    traincolor<-c("gray40",colfunc(m))

    ggplot(data=combine.tb,aes(x=value,color=set))+
      geom_density()+
      scale_color_manual(values=traincolor)+
      labs(title = "Training Data")+
     facet_trelliscope(~ variable, nrow = 2, ncol = 5,scales = "free")

  }else if (type=="qq"){

    #blue
    combine.tb$set <- factor(combine.tb$set, levels = c("True",paste0("m", 1:m)))
    #light: #bcd8ff
    #dark: #0c71ff
    colfunc <- colorRampPalette(c("#bcd8ff","#0c71ff"))
    traincolor<-c("gray40",colfunc(m))

    ggplot(data=combine.tb,aes(sample=value,color=set))+
      stat_qq(size=1)+
      #stat_qq_line()+
      #geom_density()+
      scale_color_manual(values=traincolor)+
      labs(title = "Training Data")+
      facet_trelliscope(~ variable, nrow = 2, ncol = 5,scales = "free")

  }else(
    stop("Plotting type should be one of the following: `ridges`, `error.ridges`,`density` or `qq`. ")
  )




}else{

   #plot training + test data
  combine.tb<-combine.tb %>%
    tibble::add_column(dataset = "Training Data",
                       colorset = rep(paste0("train",c(paste0("m", 1:m), "True")),time=nrow(combine.tb)/(m+1))
    )




  addNA.m2 <- obj$params$addNA.m2
  imputed.testdata <- obj$imputed.testdata

  var.list<-vector("list",length=length(continuous.vars))
  names(var.list)<-continuous.vars

  for ( var.name in continuous.vars){
    imputed <- lapply(imputed.testdata, function(x) x[[var.name]][addNA.m2[,var.name]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # original observed values for this variable
    if(is.data.table(test.data)| is_tibble(test.data)){
      #data.table
      test.data <- as.data.frame(test.data)
    }

    #data.frame
    True <- test.data[, var.name][addNA.m2[, var.name]]

    combine.df <- cbind.data.frame(Mat, True)
    colnames(combine.df) <- c(paste0("m", 1:m), "True")
    var.list[[var.name]] <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = "value")
    var.list[[var.name]] <- var.list[[var.name]]%>% tibble::add_column(variable = var.name)

  }



  combine.tb2<-do.call(rbind,var.list)




  combine.tb2<-combine.tb2 %>%
    tibble::add_column(dataset = "Test Data",
                       colorset = rep(paste0("test",c(paste0("m", 1:m), "True")),time=nrow(combine.tb2)/(m+1))
    )



  combine.both <-rbind(combine.tb,combine.tb2)



  combine.both$dataset <-factor(combine.both$dataset, levels=c("Training Data","Test Data"))





  if(type=="ridges"){

    combine.both$set <-factor(combine.both$set, levels = c(paste0("m", m:1), "True"))
    combine.both$colorset <- factor(combine.both$colorset,levels=c(paste0("train",c(paste0("m", m:1), "True")),paste0("test",c(paste0("m", m:1), "True"))))

    #dark: #0c71ff
    #light: #bcd8ff
    colfunc <- colorRampPalette(c("#0c71ff","#bcd8ff"))
    traincolor<-c(colfunc(m),"gray40")

    #dark:#FFBF00
    #light:#ffe69d
    colfunc <- colorRampPalette(c("#FFBF00","#ffe69d"))
    testcolor<-c(colfunc(m),"gray40")

    allcolor<-c(traincolor,testcolor)

    #plot both training +test
    ggplot(data =combine.both , aes(x = value,fill = colorset)) +
      geom_density_ridges(alpha = 0.8, aes(y = set)) +
      scale_fill_manual(values = allcolor, guide = guide_legend(reverse = TRUE)) +
      facet_wrap(~dataset,ncol=2)+
      guides(fill="none")+
      #theme_ridges(center_axis_labels = TRUE) +
      labs(y = "Imputed sets")+
      facet_trelliscope(~ variable, nrow=2,ncol = 5,scales = "free")
  }else if (type=="density"){

    combine.both$set <-factor(combine.both$set, levels = c("True",paste0("m", 1:m)))
    combine.both$colorset <- factor(combine.both$colorset,levels=c(paste0("train",c("True",paste0("m", 1:m))),paste0("test",c("True",paste0("m", 1:m)))))

    #light: #bcd8ff
    #dark: #0c71ff
    colfunc <- colorRampPalette(c("#bcd8ff","#0c71ff"))
    traincolor<-c("gray40",colfunc(m))

    #light:#ffe69d
    #dark:#FFBF00
    colfunc <- colorRampPalette(c("#ffe69d","#FFBF00"))
    testcolor<-c("gray40",colfunc(m))

    allcolor<-c(traincolor,testcolor)


    ggplot(data=combine.both,aes(x=value,color=colorset))+
      geom_density()+
      scale_color_manual(values=allcolor)+
      facet_wrap(~dataset,ncol=2)+
      #guides(color="none")+
      #labs(title = "Training Data")+
      facet_trelliscope(~ variable, nrow = 2, ncol = 5,scales = "free")


  }else if (type=="qq"){

    combine.both$set <-factor(combine.both$set, levels = c("True",paste0("m", 1:m)))
    combine.both$colorset <- factor(combine.both$colorset,levels=c(paste0("train",c("True",paste0("m", 1:m))),paste0("test",c("True",paste0("m", 1:m)))))

    #light: #bcd8ff
    #dark: #0c71ff
    colfunc <- colorRampPalette(c("#bcd8ff","#0c71ff"))
    traincolor<-c("gray40",colfunc(m))

    #light:#ffe69d
    #dark:#FFBF00
    colfunc <- colorRampPalette(c("#ffe69d","#FFBF00"))
    testcolor<-c("gray40",colfunc(m))

    allcolor<-c(traincolor,testcolor)

    ggplot(data=combine.both,aes(sample=value,color=colorset))+
      stat_qq(size=1)+
      #stat_qq_line()+
      #geom_density()+
      scale_color_manual(values=allcolor)+
      facet_wrap(~dataset,ncol=2)+
      #guides(color="none")+
      #labs(title = "Training Data")+
      facet_trelliscope(~ variable, nrow = 2, ncol = 5,scales = "free")

  }else(
    stop("Plotting type should be one of the following: `ridges`, `error.ridges`,`density` or `qq`. ")
  )



}

}



#
#' plot categorical variables using trelliscope
#' @importFrom trelliscopejs facet_trelliscope
#' @importFrom tibble is_tibble as_tibble
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggplot2 ggplot aes vars geom_bar geom_density coord_cartesian facet_grid labs scale_color_manual scale_fill_manual scale_alpha_discrete guides theme element_text element_blank

plotcat.trelliscope <- function(obj, train.data, test.data = NULL, type = "dodge") {

  Names <- obj$params$Names
  Types <- obj$params$Types
  m <- obj$params$m


  #matrix
  addNA.m <- obj$params$addNA.m
  #data.table
  imputed.traindata <- obj$imputed.traindata


  categorical.vars <- Names[Types != "numeric"]


  var.list<-vector("list",length=length(categorical.vars))
  names(var.list)<-categorical.vars

  for ( var.name in categorical.vars){
    imputed <- lapply(imputed.traindata, function(x) x[[var.name]][addNA.m[,var.name]])
    Nrow <- length(imputed[[1]])

    L <- unlist(imputed)
    Mat <- matrix(L, nrow = Nrow, ncol = m)

    # original observed values for this variable
    if(is.data.table(train.data)| is_tibble(train.data)){
      #data.table
      train.data <- as.data.frame(train.data)
    }

    #data.frame
    True <- train.data[, var.name][addNA.m[, var.name]]

    combine.df <- cbind.data.frame(Mat, True)
    colnames(combine.df) <- c(paste0("m", 1:m), "True")
    var.list[[var.name]] <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = "value")
    var.list[[var.name]] <- var.list[[var.name]]%>% tibble::add_column(variable = var.name)

  }



  combine.tb<-do.call(rbind,var.list)

  #blue
  combine.tb$set <- factor(combine.tb$set, levels = c("True",paste0("m", 1:m)))
  #light: #bcd8ff
  #dark: #0c71ff
  colfunc <- colorRampPalette(c("#bcd8ff","#0c71ff"))
  traincolor<-c("gray40",colfunc(m))

  if(is.null(test.data)){
    #only plot training data

    if(type!="dodge"){

      ggplot(data = combine.tb, aes(x = value, alpha =  value),stat = "identity") +
        geom_bar(aes(fill = set)) +
        scale_alpha_discrete(range = c(0.8, 1)) +
        facet_grid(vars(set),switch = "y")+
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color = "black"),
          strip.text.y.left= element_text(angle = 0)
        ) +
        labs(title = "Training Data", y = "Imputed sets") +
        scale_fill_manual(values = traincolor)+
        guides(fill="none",alpha="none")+
        facet_trelliscope(~ variable, nrow = 2, ncol =5,scales = "free")

    }else{
      ggplot(data = combine.tb, aes(x = value,  fill = set)) +
        geom_bar(aes(alpha = set),position="dodge") +
        #scale_color_manual(values=traincolor)+
        scale_fill_manual(values=traincolor)+
        scale_alpha_discrete(range = c(0.8, 1))+
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(color = "black")
        ) +
        labs(title = "Training Data")+
        facet_trelliscope(~ variable, nrow = 2, ncol =5,scales = "free")
    }


    #vars(fct_rev(set))
  }else{
    #plot training + test data
    combine.tb<-combine.tb %>%
      tibble::add_column(dataset = "Training Data",
                         colorset = rep(paste0("train",c(paste0("m", 1:m), "True")),time=nrow(combine.tb)/length(traincolor))
      )




    addNA.m2 <- obj$params$addNA.m2
    imputed.testdata <- obj$imputed.testdata


    for ( var.name in categorical.vars){
      imputed <- lapply(imputed.testdata, function(x) x[[var.name]][addNA.m2[,var.name]])
      Nrow <- length(imputed[[1]])

      L <- unlist(imputed)
      Mat <- matrix(L, nrow = Nrow, ncol = m)

      # original observed values for this variable
      if(is.data.table(test.data)| is_tibble(test.data)){
        #data.table
        test.data <- as.data.frame(test.data)
      }

      #data.frame
      True <- test.data[, var.name][addNA.m2[, var.name]]

      combine.df <- cbind.data.frame(Mat, True)
      colnames(combine.df) <- c(paste0("m", 1:m), "True")
      var.list[[var.name]] <- pivot_longer(data = combine.df, cols = everything(), names_to = "set", values_to = "value")
      var.list[[var.name]] <- var.list[[var.name]]%>% tibble::add_column(variable = var.name)

    }



    combine.tb2<-do.call(rbind,var.list)

    #light:#ffe69d
    #dark:#FFBF00
    colfunc <- colorRampPalette(c("#ffe69d","#FFBF00"))
    testcolor<-c("gray40",colfunc(m))


    combine.tb2<-combine.tb2 %>%
      tibble::add_column(dataset = "Test Data",
                         colorset = rep(paste0("test",c(paste0("m", 1:m), "True")),time=nrow(combine.tb2)/length(testcolor))
      )

    combine.both <-rbind(combine.tb,combine.tb2)
    combine.both$set <-factor(combine.both$set, levels = c("True",paste0("m", 1:m)))
    combine.both$dataset <-factor(combine.both$dataset, levels=c("Training Data","Test Data"))
    combine.both$colorset <- factor(combine.both$colorset,levels=c(paste0("train",c("True",paste0("m", 1:m))),paste0("test",c("True",paste0("m", 1:m)))))

    allcolor<-c(traincolor,testcolor)

   if(type!="dodge"){
     ggplot(data = combine.both, aes(x = value, alpha =  value),stat = "identity") +
       geom_bar(aes(fill = colorset)) +
       scale_alpha_discrete(range = c(0.8, 1)) +
       facet_grid(vars(set),vars(dataset),switch = "y")+
       theme(
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.x = element_text(color = "black"),
         strip.text.y.left= element_text(angle = 0)
       ) +
       labs(y = "Imputed sets") +
       scale_fill_manual(values = allcolor)+
       guides(fill="none",alpha="none")+
       facet_trelliscope(~ variable, nrow = 2, ncol = 5,scales = "free")
   }else{
     ggplot(data = combine.both, aes(x = value,  fill = colorset)) +
       geom_bar(aes(alpha = set),position="dodge") +
       #scale_color_manual(values=traincolor)+
       scale_fill_manual(values=allcolor)+
       scale_alpha_discrete(range = c(0.8, 1))+
       facet_grid(vars(dataset))+
       theme(
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.text.x = element_text(color = "black")
       ) +
       guides(alpha="none")+
       #labs(title = "Training Data")+
       facet_trelliscope(~ variable, nrow = 2, ncol =5,scales = "free")
   }





  }


}
