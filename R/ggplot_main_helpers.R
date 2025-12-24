# histogram+density -------------------------------------------------------
.ggplot_histogram_density<-function(all_dt, x, color_pal,nbins){

  if(is.null(nbins)){
    breaks <- pretty(range(all_dt[[x]]),
                     n = grDevices::nclass.Sturges(all_dt[[x]]),
                     min.n = 1
    )
  }else{
    breaks = seq(min(all_dt[[x]], na.rm=TRUE),
                 max(all_dt[[x]], na.rm=TRUE),
                 length.out = nbins + 1)
  }


  ggplot(all_dt, aes(x = .data[[x]])) +
    geom_histogram(aes(y = after_stat(density),fill=Group,color=Group),  # scale histogram to density
                   linewidth = 0.2, alpha = 0.2, position = "identity", breaks = breaks) +
    geom_density(aes(y = after_stat(density),color=Group),           # density scaled to match histogram
                 linewidth = 1, alpha = 1)+
    facet_grid(~Group)+
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    labs(title = paste("Observed vs multiply-imputed values for", x),
         x = x, y = "Density")

}





# scatter ----------------------------------------------------------
.ggplot_scatter<- function(all_dt,x,y,point_size,alpha,color_pal){
  ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = alpha, aes(color = Group , fill = Group), size = point_size) +
    facet_grid(cols = vars(Group)) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal)+
    labs(x=x,y=y,title=paste("Observed vs multiply-imputed values:",  y, "vs", x))

}



# scatter facet ----------------------------------------------------------
.ggplot_scatter_facet<- function(all_dt,x,y,z,point_size,alpha,color_pal){

  ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = alpha, aes(color = Group , fill = Group), size = point_size) +
    #facet_grid(.data[[z]]~Group) +
    facet_grid(.data[[z]]~Group,labeller = labeller(
      .rows = function(level) paste(z, "=", level),   # only the row facet gets "z = level"
      .cols = label_value   # column facet just shows values, no "Group = "
    ))+
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal) +
    #scale_y_continuous(sec.axis = sec_axis(~., name = z, breaks = NULL, labels = NULL))+
    labs(x=x,y=y,title=paste("Observed vs multiply-imputed values:",  y, "vs", x), subtitle = paste("Faceted by", z) )


}



# box ---------------------------------------------------------------------
.ggplot_box<- function(all_dt,x,y,point_size,alpha,color_pal,boxpoints){

  if(isFALSE(boxpoints)){
    fig<-ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  }else if(boxpoints=="all"){
    fig<- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_jitter(alpha = alpha, position = position_jitter(seed=2025), aes(color = Group, fill = Group),size=point_size)+
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  }else if(boxpoints=="outliers"){
    fig<- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group, color=Group), outlier.size=point_size, outlier.alpha=alpha)
  }
    fig+
    facet_grid(cols = vars(Group)) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal)+
    labs(x=x,y=y,title=paste("Observed vs multiply-imputed values:",  y, "vs", x))


}


# box ---------------------------------------------------------------------
.ggplot_box_facet<- function(all_dt,x,y, z, color_pal,point_size,alpha,boxpoints){

  if(isFALSE(boxpoints)){
    fig<-ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  }else if(boxpoints=="all"){
    fig<- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_jitter(alpha = alpha, position = position_jitter(seed=2025), aes(color = Group, fill = Group),size=point_size)+
      geom_boxplot(alpha = alpha, aes(fill = Group), outlier.shape = NA)
  }else if(boxpoints=="outliers"){
    fig<- ggplot(all_dt, aes(x = .data[[x]], y = .data[[y]])) +
      geom_boxplot(alpha = alpha, aes(fill = Group, color=Group), outlier.size=point_size, outlier.alpha=alpha)
  }
  fig+
    facet_grid(.data[[z]]~Group,labeller = labeller(
      .rows = function(level) paste(z, "=", level),   # only the row facet gets "z = level"
      .cols = label_value   # column facet just shows values, no "Group = "
    )) +
    scale_color_manual(values = color_pal) +
    scale_fill_manual(values = color_pal)+
    #scale_y_continuous(sec.axis = sec_axis(~., name = z, breaks = NULL, labels = NULL))+
    labs(x=x,y=y,title=paste("Observed vs multiply-imputed values:",  y, "vs", x), subtitle = paste("Faceted by", z) )



}


# bar ---------------------------------------------------------------------


.ggplot_bar<-function(all_dt,x,color_pal,alpha, width){
  ggplot(all_dt, aes(x = .data[[x]])) +
    geom_bar(stat = "count", alpha = alpha, width = width, aes(color = Group , fill = Group, y = after_stat(prop), group = Group)) +
    scale_y_continuous(labels = scales::percent) +
    ylab("proportion") +
    facet_grid(cols = vars(Group)) +
    labs(title = paste("Observed vs multiply-imputed values for", x)) +
    scale_fill_manual(values = color_pal) +
    scale_color_manual(values = color_pal)

}


# bar facet1 ---------------------------------------------------------------------
.ggplot_bar_facet1<-function(all_dt,x,y, color_pal,alpha, width){

  #propt <- prop.table(table(all_dt[[x]], all_dt[[y]], all_dt[["Group"]]), margin = 3)
  #prop_df <- as.data.frame(propt)
  all_sum <- all_dt |> group_by(Group,.data[[x]], .data[[y]]) |>
    summarise(count = n(), .groups = "drop")  |>
    tidyr::complete(
      Group = unique(all_dt$Group),
      !!sym(x) := unique(all_dt[[x]]),
      !!sym(y) := unique(all_dt[[y]]),
      fill = list(count = 0)
    ) |>
    group_by(Group) |>
    mutate(prop = count / sum(count)) |>
    ungroup()


  ggplot(all_sum, aes(x = .data[[x]], y = prop)) +
    geom_bar(stat = "identity", position = "dodge2", aes(fill = Group),alpha=alpha, width=width) +
    facet_grid(.data[[y]] ~ Group,labeller = labeller(
      .rows = function(level) paste(y, "=", level),   # only the row facet gets "z = level"
      .cols = label_value   # column facet just shows values, no "Group = "
    ))+
    scale_fill_manual(values = color_pal) +
   #scale_y_continuous(sec.axis = sec_axis(~., name = y, breaks = NULL, labels = NULL)) +
    labs(x=x,y="Proportion",title=paste("Observed vs multiply-imputed values:",  y, "vs", x))

}



