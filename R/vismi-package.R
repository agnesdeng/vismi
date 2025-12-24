#' vismi: Visualisation Tools for Multiple Imputation
#'
#' Visual diagnostic tools for assessing multiply imputed datasets created with
#' 'mixgb' or other imputers.
#'
#' @docType package
#' @name vismi-package
#' @keywords internal
#' @importFrom data.table ':=' as.data.table data.table is.data.table melt rbindlist setnames
#' @importFrom plotly plot_ly layout add_histogram add_lines add_trace subplot
#' @importFrom utils modifyList
#' @importFrom stats as.formula setNames density
#' @importFrom dplyr group_by reframe tibble filter ungroup mutate summarise n sym
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density geom_point geom_bar geom_boxplot geom_jitter position_jitter margin unit
#' @importFrom ggplot2 scale_y_continuous scale_y_discrete scale_alpha_discrete scale_color_manual scale_fill_manual ylab labs guides guide_legend theme element_text element_rect element_line
#' @importFrom ggplot2 after_stat facet_grid vars labeller ylim coord_cartesian stat_qq stat_qq_line position_dodge ggplot_build
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom tidyr pivot_longer
#' @importFrom mixgb mixgb impute_new
#' @importFrom rlang .data
#' @importFrom scales hue_pal
#' @importFrom grDevices nclass.Sturges colorRampPalette
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid textGrob gpar
#' @importFrom ggridges geom_density_ridges

#' @references Deng, Y., & Lumley, T. (2026), vismi: Visualisation Tools for Multiple Imputation, arXiv preprint.
"_PACKAGE"


NULL
