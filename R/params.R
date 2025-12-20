.vismi_static_params <- function() {
  list(
    point_size = 1,
    alpha = 0.8,
    width=0.9,
    nbins=NULL,
    boxpoints = "outliers"
  )
}

.vismi_interactive_params <- function() {
  list(
    point_size = 4,
    alpha = 0.8,
    width=0.9,
    nbins=NULL,
    boxpoints = "outliers"
  )
}


.vismi_overimp_params <- function() {
  list(
    position = position_dodge(width = 0.9),
    point_size = 1,
    alpha = 0.9,
    linewidth = 1,
    ylim = NULL
  )
}


