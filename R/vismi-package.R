#' vismi: Visualisation Tools for Multiple Imputation
#'
#' Visual diagnostic tools for assessing multiply imputed datasets created with
#' 'mixgb' or other imputers. The package emphasises side-by-side
#' comparisons of observed, imputed, and (optionally) masked-true values
#' through ggplot2-based displays.
#'
#' @docType package
#' @name vismi-package
#' @aliases vismi
#' @keywords internal
#' @importFrom data.table ':=' as.data.table data.table is.data.table melt rbindlist setnames
#' @importFrom ggplot2 element_blank element_text margin theme
#' @importFrom grid unit
#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict quantile rbinom
#' @references Deng, Y. (2024). Visual diagnostics for multiply imputed data.
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
