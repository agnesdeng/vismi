ts_overimp <- function(obj, m = NULL, imp_idx = NULL, integerAsFactor = FALSE, num_plot= "cv", fac_plot = "cv",train_color_pal = NULL, test_color_pal = NULL,stack_y = FALSE, diag_color = NULL,seed=2025,...) {

  all_vars <- obj$params$Names

  users_params <- list(...)
  params <- modifyList(.vismi_overimp_params(), users_params)

  plot_map <- list(
    numeric = list(cv = overimp1D_cv_num, ridge = overimp1D_ridge, density = overimp1D_density),
    factor  = list(cv = overimp1D_cv_fac, bar = overimp1D_bar, dodge = overimp1D_dodge)
  )


  # Create a "Control Table" for Trelliscope. This table has one row per variable
 plot_data_frame<- tibble(variable = all_vars) %>%
    group_by(variable) %>%
    mutate(panel = purrr::map(variable, function(v_name) {

      # for each variable
      plot_data_sub <- .overimp_preprocess(obj = obj, vars = v_name, m=m,imp_idx =imp_idx,
                                           integerAsFactor = integerAsFactor)


      col_sample <- obj$imputed_train[[1]][[v_name]]
      v_type <- if(inherits(col_sample, "ordered") || (is.integer(col_sample) && integerAsFactor)) {
        "factor"
      } else if(is.numeric(col_sample)) {
        "numeric"
      } else {
        "factor"
      }


      plot_which <- if(v_type == "numeric") num_plot else fac_plot
      plot_fun <- plot_map[[v_type]][[plot_which]]

      args_list <- list(
        plot_data = plot_data_sub,
        x = v_name,
        comb_title = paste("Masked true vs impute:", v_name),
        train_color_pal = if(is.null(train_color_pal)) plot_data_sub$train$color_pal else train_color_pal,
        test_color_pal = if(is.null(test_color_pal)) plot_data_sub$test$color_pal else test_color_pal,
        point_size = params$point_size,
        xlim = params$xlim,
        ylim = params$ylim,
        stack_y=stack_y,
        diag_color = diag_color,
        seed = seed
      )

      do.call(plot_fun, args_list[names(args_list) %in% names(formals(plot_fun))])

    }))%>%
   ungroup()


    trelliscopejs::trelliscope(plot_data_frame,name = "Overimputation Diagnostic for all Variables", panel_col = "panel")
}
