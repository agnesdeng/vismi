# overimputation plot for a single variable

# 1. Define the Generic
vismi2 <- function(obj, ...) {
  UseMethod("vismi2")
}

#'@exportS3Method
vismi2.overimp <- function(obj, x=NULL, y=NULL, z=NULL, num.plot = "qq", int.plot="ridge", cat.plot = "bar") {

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



  #train_data = obj$imputed_train
  #test_data = obj$imputed_test



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
    # Call the function
    overimp1D_qq(obj = obj, x = vars[1])
  }


}

# 3. Manually Register the Method (Crucial step)
registerS3method("vismi2", "overimp", vismi2.overimp)


overimp1D_qq <- function(obj, x, point_size = 1, ylim = NULL) {

  train_data = obj$params$train_data
  test_data = obj$params$test_data

  long_data <- .prepare_imputed_long(obj, x)

  # Train plot
  P1 <- ggplot2::ggplot(long_data$train$long_df, ggplot2::aes(sample = .data[[x]], color = Group)) +
    ggplot2::stat_qq(size = point_size) +
    ggplot2::scale_color_manual(values = long_data$train$colors) +
    ggplot2::labs(title = "Training Data")

  if(!is.null(ylim)) P1 <- P1 + ggplot2::ylim(ylim)

  # Test plot
  if(!is.null(long_data$test)) {
    xrange <- ggplot2::ggplot_build(P1)$layout$panel_params[[1]]$x.range
    P2 <- ggplot2::ggplot(long_data$test$long_df, ggplot2::aes(sample = .data[[x]], color = Group)) +
      ggplot2::stat_qq(size = point_size) +
      ggplot2::coord_cartesian(xlim = xrange) +
      ggplot2::scale_color_manual(values = long_data$test$colors) +
      ggplot2::labs(title = "Test Data")
    if(!is.null(ylim)) P2 <- P2 + ggplot2::ylim(ylim)

    gridExtra::grid.arrange(P1, P2, ncol = 2)
  } else {
    P1
  }
}

