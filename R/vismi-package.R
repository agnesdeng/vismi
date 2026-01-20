#' vismi: Visual Diagnostics for Multiple Imputation
#'
#' @keywords internal
#' @importFrom cli cli_h1 cli_alert_info cli_alert_warning cli_inform qty cli_vec
#' @importFrom data.table ':=' as.data.table data.table is.data.table melt rbindlist setnames
#' @importFrom dplyr group_by reframe tibble filter select all_of left_join ungroup mutate summarise n sym row_number
#' @importFrom ggplot2 ggplot aes geom_line geom_histogram geom_density geom_point geom_bar geom_col geom_boxplot geom_jitter position_jitter margin unit geom_rect geom_segment geom_abline
#' @importFrom ggplot2 scale_y_continuous scale_y_discrete scale_alpha_discrete scale_alpha_manual scale_color_manual scale_fill_manual scale_fill_identity scale_x_continuous ylab labs guides guide_legend
#' @importFrom ggplot2 xlim ylim theme theme_minimal element_text element_rect element_line
#' @importFrom ggplot2 after_stat facet_grid vars labeller label_value ylim coord_cartesian stat_qq stat_qq_line position_dodge ggplot_build element_blank
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @importFrom ggtext element_markdown
#' @importFrom GGally ggpairs
#' @importFrom grDevices nclass.Sturges colorRampPalette
#' @importFrom gridExtra arrangeGrob
#' @importFrom grid textGrob gpar
#' @importFrom mixgb mixgb impute_new
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom plotly plot_ly layout add_histogram add_lines add_trace subplot
#' @importFrom purrr map
#' @importFrom rlang .data
#' @importFrom scales hue_pal
#' @importFrom stats as.formula setNames density
#' @importFrom tidyr pivot_longer starts_with
#' @importFrom trelliscopejs trelliscope
#' @importFrom utils combn modifyList askYesNo
#' @references Yongshi Deng, Thomas Lumley. (2026), vismi: Visual Diagnostics for Multiple Imputation,
#' R package version 0.9.3
"_PACKAGE"
