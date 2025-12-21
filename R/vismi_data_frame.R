#' Visualise Multiple Imputations
#' @export
vismi <-function(...){
  UseMethod("vismi")
}


#' Visualise Multiple Imputations for Distributional Characteristics
#' @describeIn vismi Visualise Multiple Imputations for Distributional Characteristics
#' @description This function provides visual diagnostic tools for assessing multiply imputed datasets created with 'mixgb' or other imputers. It supports 1D, 2D, and 3D visualizations for numeric variables.
#' @param data A data frame containing the original data with missing values.
#' @param imp_list A list of imputed data frames.
#' @param x A character string specifying the name of the variable to plot on the x
#' axis. Default is NULL.
#' @param y A character string specifying the name of the variable to plot on the y
#' axis. Default is NULL.
#' @param z A character string specifying the name of the variable to plot on the z
#' axis. Default is NULL.
#' @param interactive A logical value indicating whether to create an interactive plotly plot (TRUE
#' by default) or a static ggplot2 plot (FALSE).
#' @param integerAsFactor A logical value indicating whether to treat integer variables as factors
#' (TRUE) or numeric (FALSE). Default is FALSE.
#' @param color_pal A named vector of colors for different imputation sets. If NULL
#' (default), a default color palette is used.
#' @param marginal_x A character string specifying the type of marginal plot to add
#' for the x variable in 2D plots. Options are "hist", "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = TRUE. Options are "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = FALSE.
#' @param marginal_y A character string specifying the type of marginal plot to add
#' for the y variable in 2D plots. Options are "hist", "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = TRUE. Options are "box", "rug", "box+rug", or NULL
#' (default, no marginal plot) when interactive = FALSE.
#' @param ... Additional arguments passed to the underlying plotting functions, such as point_size, alpha, nbins, width, and boxpoints.
#' @return A plotly or ggplot2 object visualizing the imputed data.
#' @export
vismi.data.frame = function(data, imp_list, x=NULL, y= NULL, z=NULL, interactive= TRUE, integerAsFactor = FALSE, color_pal=NULL, marginal_x=NULL, marginal_y=NULL,...){

  #check input variables
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


  nonexist_vars <- setdiff(vars, names(data))
  if (length(nonexist_vars > 0)) {
    stop("Please check your spelling. Variable(s) not found in data: ",
         paste(nonexist_vars, collapse = ", "))
  }

  #users_params<-list()
  users_params <- list(...)
  if(interactive){
    params <- modifyList(.vismi_interactive_params(), users_params)
  }else{
    params <- modifyList(.vismi_static_params(), users_params)
  }

  point_size <- params$point_size
  alpha <- params$alpha
  nbins <- params$nbins
  width <- params$width
  boxpoints <- params$boxpoints


  #type of variables
  #types <- sapply(data[ , vars, drop=FALSE], class)
  sub_data <- as.data.frame(data[, vars, drop = FALSE])
  types <- sapply(sub_data, function(col) {
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
    if(!is.null(marginal_y)){
      warning("marginal_y is ignored for 1D diganostics plot.")
    }


    plot_fun <- switch(
      types,
      "numeric"     = if(interactive) plotly_1num else ggplot_1num,
      "factor"   = if(interactive) plotly_1fac else ggplot_1fac
    )


  }



  if(D==2){
    type_comb <- paste0(sort(types), collapse = "_")

    plot_fun <- switch(
      type_comb,
      "numeric_numeric"   = if(interactive) plotly_2num else ggplot_2num,
      "factor_numeric"    = if(interactive) plotly_1fac1num else ggplot_1fac1num,
      "factor_factor"     = if(interactive) plotly_2fac else ggplot_2fac
    )
  }


  if(D==3){
    #vars<-names(sort(types))

    type_comb <- paste0(sort(types), collapse = "_")

    if(type_comb =="factor_numeric_numeric"){
      # z: factor
      fac_idx <- which(types=="factor")
      if(fac_idx !=3){
        fac <- vars[fac_idx]
        vars[fac_idx] <- vars[3]
        vars[3] <- fac
      }
    }else if(type_comb=="factor_factor_numeric"){
      # z: factor
      num_idx <- which(types=="numeric")
      if(num_idx ==3){
        num <- vars[num_idx]
        vars[3] <- vars[2]
        vars[2] <- num
      }
    }



    plot_fun <- switch(
      type_comb,
      "numeric_numeric_numeric"   = if(interactive) plotly_3num else stop("3D static plots are not supported yet. Please set interactive = TRUE."),
      "factor_numeric_numeric"    = if(interactive) plotly_1fac2num else ggplot_1fac2num,
      "factor_factor_numeric"     = if(interactive) plotly_2fac1num else ggplot_2fac1num,
      "factor_factor_factor"      = if(interactive) plotly_3fac else ggplot_3fac

      )

  }

  # preprocess data
  pre <- preprocess(data, imp_list, vars = vars, integerAsFactor=integerAsFactor)
  all_dt <- pre$all_dt
  if(is.null(color_pal)){
    color_pal <- pre$color_pal
  }



  call_plot_fun <- function(plot_fun, args_list){
    fun_args <- names(formals(plot_fun))
    args_list <- args_list[names(args_list) %in% fun_args]
    do.call(plot_fun, args_list)
  }

  # all possible arguments
  args_list <- list(
    all_dt = all_dt,
    imp_list = imp_list,
    x = vars[1],
    y = vars[2],
    z = vars[3],
    #integerAsFactor = integerAsFactor,
    marginal_x = marginal_x,
    marginal_y = marginal_y,
    color_pal = color_pal,
    point_size = point_size,
    alpha = alpha,
    nbins = nbins,
    width = width,
    boxpoints = boxpoints
  )

  # Call the plotting function
  call_plot_fun(plot_fun, args_list)



}

