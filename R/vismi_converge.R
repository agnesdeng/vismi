#' Visualise onvergence diagnostics
#' @param obj A 'mixgb' object returned by \code{mixgb()} function or a 'mids' object returned by the \code{mice()} function.
#' @param var The name of the variable to plot convergence for.
#' @param tick_vals Optional numeric vector specifying x-axis tick values for iterations.
#' @param color_pal A vector of m color codes (e.g., hex codes). If NULL, default colors will be used.
#' @return A ggplot2 object showing the convergence plot for the specified variable.
#'@export
vismi_converge <- function(obj,var, tick_vals = NULL, color_pal = NULL) {
  UseMethod("vismi_converge")
}


#' Visualise onvergence diagnostics for a variable imputed by mixgb
#' @rdname vismi_converge
#' @method vismi_converge mixgb
#' @export
vismi_converge.mixgb<-function(obj, var, tick_vals = NULL, color_pal = NULL){

  if(!is.null(color_pal)){
    if (length(color_pal) < obj$params$m){
      stop(paste0("color_pal must have ", obj$params$m, " colors."))
    }
  }


  if (!inherits(obj, "mixgb")) stop("obj must be a mixgb object returned by mixgb() with `save.models` set to TRUE.")
  mn <- obj$IMP.MEAN[,var,, drop=TRUE]

  # colnames(mn)<-paste0("Set", seq_len(ncol(mn)))
  mean_df <- as.data.frame(mn) |>
    mutate(iteration = row_number()) |>
    pivot_longer(
      cols = starts_with("Set"),
      #cols = !iteration,
      names_to = "Imputation",
      values_to = paste0("Mean")
    )


  SD <- obj$IMP.VAR[,var,, drop=TRUE]
  #colnames(SD)<-paste0("Set", seq_len(ncol(SD)))
  SD_df <- as.data.frame(SD) |>
    mutate(iteration = row_number()) |>
    pivot_longer(
      cols = starts_with("Set"),
      #cols = !iteration,
      names_to = "Imputation",
      values_to = paste0("SD")
    )


  combine_df <- mean_df |>
    left_join(SD_df, by = c("iteration", "Imputation"))
  combine_df <- combine_df |> pivot_longer(
    cols = c("Mean","SD"),
    names_to = "target",
    values_to = "value"
  )

  if(is.null(tick_vals)){
    maxit <- nrow(mn)
    n_ticks<-5
    tick_vals <- unique(round(seq(0, maxit, length.out = n_ticks+1)))
  }


  mean_plot<-ggplot(data = mean_df,aes(x=.data$iteration,y=.data$Mean, colour=.data$Imputation))+
    geom_line(linewidth=0.8)+
    labs(
      x = "Iteration",
      y = paste0("Mean of imputed **", var, "**"),
      colour = "Imputation"
    )+
    scale_x_continuous(
      breaks = tick_vals
    )+
    theme_minimal()+
    theme(
      axis.title.y = element_markdown(),
      title = element_markdown()
    )+guides(colour="none")

  if(!is.null(color_pal)){
    mean_plot<-mean_plot+scale_color_manual(values = color_pal)
  }

  SD_plot<-ggplot(data = SD_df,aes(x=.data$iteration,y=.data$SD, colour=.data$Imputation))+
    geom_line(linewidth=0.8)+
    labs(
      x = "Iteration",
      y = paste0("SD of imputed **", var, "**"),
      colour = "Imputation"
    )+
    scale_x_continuous(
      breaks = tick_vals
    )+
    theme_minimal()+
    theme(
      axis.title.y = element_markdown(),
      title = element_markdown()
    )

  if(!is.null(color_pal)){
    SD_plot<-SD_plot+scale_color_manual(values = color_pal)
  }

  comb_title <- paste0("Convergence plot for ", var, " imputed by mixgb")

  mean_plot + SD_plot +
    plot_layout(ncol = 2) +
    plot_annotation(title = comb_title,
                    theme = theme(
                      plot.title = element_text(
                        size = 14,           # Font size
                        face = "bold",       # "plain", "italic", "bold", "bold.italic"
                        family = "sans",    # Font family (e.g., "sans", "serif", "mono")
                        hjust = 0.5,         # Center the title (0 = left, 1 = right)
                        color = "black"   # Font color
                      )))


}



#' Visualise convergence diagnostics for a variable imputed by MICE
#' @rdname vismi_converge
#' @method vismi_converge mids
#' @export
vismi_converge.mids<-function(obj, var, tick_vals = NULL, color_pal = NULL){

  if(!is.null(color_pal)){
    if (length(color_pal) < obj$params$m){
      stop(paste0("color_pal must have ", obj$params$m, " colors."))
    }
  }

  if (!inherits(obj, "mids")) stop("obj must be a mids object returned by mice().")
  mn <- obj$chainMean[var, , , drop = TRUE]

  colnames(mn)<-paste0("Set", seq_len(ncol(mn)))
  mean_df <- as.data.frame(mn) |>
    mutate(iteration = row_number()) |>
    pivot_longer(
      cols = starts_with("Set"),
      #cols = !iteration,
      names_to = "Imputation",
      values_to = paste0("Mean")
    )


  SD <- obj$chainVar[var, , , drop = TRUE]
  colnames(SD)<-paste0("Set", seq_len(ncol(SD)))
  SD_df <- as.data.frame(SD) |>
    mutate(iteration = row_number()) |>
    pivot_longer(
      cols = starts_with("Set"),
      #cols = !iteration,
      names_to = "Imputation",
      values_to = paste0("SD")
    )


  combine_df <- mean_df |>
    left_join(SD_df, by = c("iteration", "Imputation"))
  combine_df <- combine_df |> pivot_longer(
    cols = c("Mean","SD"),
    names_to = "target",
    values_to = "value"
  )

  if(is.null(tick_vals)){
    maxit <- nrow(mn)
    n_ticks<-5
    tick_vals <- unique(round(seq(0, maxit, length.out = n_ticks+1)))
  }


  mean_plot<-ggplot(data = mean_df,aes(x=.data$iteration,y=.data$Mean, colour=.data$Imputation))+
    geom_line(linewidth=0.8)+
    #facet_wrap(~target, scales = "free")+
    labs(
      x = "Iteration",
      y = paste0("Mean of imputed **", var, "**"),
      colour = "Imputation"
    )+
    scale_x_continuous(
      breaks = tick_vals
    )+
    theme_minimal()+
    theme(
      axis.title.y = element_markdown(),
      title = element_markdown()
    )+guides(colour="none")

  if(!is.null(color_pal)){
    mean_plot<-mean_plot+scale_color_manual(values = color_pal)
  }

  SD_plot<-ggplot(data = SD_df,aes(x=.data$iteration,y=.data$SD, colour=.data$Imputation))+
    geom_line(linewidth=0.8)+
    labs(
      x = "Iteration",
      y = paste0("SD of imputed **", var, "**"),
      colour = "Imputation"
    )+
    scale_x_continuous(
      breaks = tick_vals
    )+
    theme_minimal()+
    theme(
      axis.title.y = element_markdown(),
      title = element_markdown()
    )

  if(!is.null(color_pal)){
    SD_plot<-SD_plot+scale_color_manual(values = color_pal)
  }

  comb_title <- paste0("Convergence plot for ", var, " imputed by MICE")

  mean_plot + SD_plot +
      plot_layout(ncol = 2) +
      plot_annotation(title = comb_title,
                      theme = theme(
                        plot.title = element_text(
                          size = 14,           # Font size
                          face = "bold",       # "plain", "italic", "bold", "bold.italic"
                          family = "sans",    # Font family (e.g., "sans", "serif", "mono")
                          hjust = 0.5,         # Center the title (0 = left, 1 = right)
                          color = "black"   # Font color
                        )))


}
