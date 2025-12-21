#' Visualise Multiple Imputation via Overimputations
#' @describeIn vismi Visualise Multiple Imputation via Overimputations
#' @param obj Overimputation object of class 'overimp' created by the \code{overimpute()} function.
#' @param x A character string specifying the name of the variable to plot on the x
#' @param y A character string specifying the name of the variable to plot on the y
#' @param z A character string specifying the name of the variable to plot on the z
#' @param num_plot A character string specifying the type of plot for numeric variables. Options
#' are "qq" (default) for QQ plot.
#' @param int_plot A character string specifying the type of plot for integer variables. Options
#' are "ridge" (default) for ridge plot.
#' @param fac_plot A character string specifying the type of plot for categorical variables. Options
#' are "bar" (default) for bar plot.
#' @export
vismi.overimp <- function(obj, x=NULL, y=NULL, z=NULL, integerAsFactor = FALSE, num_plot= "qq", fac_plot = "bar",train_color_pal = NULL, test_color_pal = NULL,...) {

  #checking
  if(!inherits(obj, "overimp")) stop("Object must be of class 'overimp'")

  if (!is.null(x) && !is.character(x)){
    stop("x must be a variable name in character format.")
  }
  if (!is.null(y) && !is.character(y)){
    stop("y must be a variable name in character format.")
  }
  if (!is.null(z) && !is.character(z)){
    stop("z must be a variable name in character format.")
  }


  vars<-c(x,y,z)
  #remove any NULL
  vars<-vars[!sapply(vars,is.null)]

  #Names <- obj$params$Names
  nonexist_vars <- setdiff(vars, obj$params$Names)
  if (length(nonexist_vars > 0)) {
    stop("Please check your spelling. Variable(s) not found in data: ",
         paste(nonexist_vars, collapse = ", "))
  }



  #users_params<-list()
  users_params <- list(...)

  params <- modifyList(.vismi_overimp_params(), users_params)

  position <- params$position
  point_size <- params$point_size
  linewidth <- params$linewidth
  alpha <- params$alpha
  xlim <- params$xlim
  ylim <- params$ylim
  boxpoints <- params$boxpoints
  width <- params$width





  #Types <- obj$params$Types
  types <- sapply(obj$imputed_train[[1]][, ..vars], function(col) {
    if (inherits(col, "ordered")) {
      "factor"
    }else if(is.integer(col)){
      if(isTRUE(integerAsFactor)){
        "factor"
      }else{
        "numeric"
      }
    } else {
      class(col)
    }
  })


  #number of variables
  D <-length(vars)

  if(D==1){

    x<- vars[1]
    comb_title <-grid::textGrob(paste("Masked true vs multiply-imputed values:", x),
                                gp = grid::gpar(fontsize = 14, fontface = "bold"))



    plot_map <- list(
      numeric = list(
        qq = overimp1D_qq,
        qqline = overimp1D_qqline,
        ridge = overimp1D_ridge,
        density = overimp1D_density
      ),
      factor = list(
        bar = overimp1D_bar,
        dodge = overimp1D_dodge
      )
    )

    plot_which <- switch(
      types,
      numeric = num_plot,
      factor = fac_plot,
    )

    plot_fun <- plot_map[[types]][[plot_which]]

    if(is.null(plot_fun)){
      stop("The specified plot type is not valid for the variable type.")
    }

  }else if(D==2){
    x<- vars[1]
    y<- vars[2]
    comb_title <-grid::textGrob(paste("Masked true vs multiply-imputed values:", y, "vs", x),
                                gp = grid::gpar(fontsize = 14, fontface = "bold"))

    type_comb <- paste0(sort(types), collapse = "_")

    plot_fun <- switch(
      type_comb,
      "numeric_numeric"   = overimp2D_scatter,
      "factor_numeric"    = overimp2D_box,
     "factor_factor"     = overimp2D_bar
    )




  }else if(D==3){
    x<- vars[1]
    y<- vars[2]
    z<- vars[3]
    comb_title <-grid::textGrob(paste0(
      "Masked true vs multiply-imputed values: ", y, " vs ", x,
      "\nFaceted by ", z),
      gp = grid::gpar(fontsize = 14, fontface = "bold"))
  }


  plot_data <- .overimp_preprocess(obj=obj, vars=vars, integerAsFactor=integerAsFactor)
  if(is.null(train_color_pal)){
    train_color_pal <- plot_data$train$color_pal
  }

  if(is.null(test_color_pal)){
    test_color_pal <- plot_data$test$color_pal
  }



  call_plot_fun <- function(plot_fun, args_list){
    fun_args <- names(formals(plot_fun))
    args_list <- args_list[names(args_list) %in% fun_args]
    do.call(plot_fun, args_list)
  }

  # all possible arguments
  args_list <- list(
    plot_data = plot_data,
    x = vars[1],
    y = vars[2],
    z = vars[3],
    train_color_pal = train_color_pal,
    test_color_pal = test_color_pal,
    comb_title = comb_title,
    alpha = alpha,
    xlim = xlim,
    ylim = ylim,
    position = position,
    point_size = point_size,
    linewidth = linewidth,
    boxpoints = boxpoints,
    width = width
  )

  # Call the plotting function
  call_plot_fun(plot_fun, args_list)

}





#' print method for overimp_plot objects
#' @exportS3Method
print.overimp_plot <- function(x, ...) {
  # Check if it's a gtable/grob (from arrangeGrob)
  if (inherits(x, "gtable") || inherits(x, "grob")) {
    grid::grid.newpage()  # Clear the plot area
    grid::grid.draw(x)    # Draw the plot
  } else if (inherits(x, "ggplot")) {
    # It's a ggplot object - use ggplot's print method
    NextMethod("print")  # Call print.ggplot
  }
  invisible(x)
}

#' plot method for overimp_plot objects
#' @exportS3Method
plot.overimp_plot <- function(x, ...) {
  print(x, ...)
}


