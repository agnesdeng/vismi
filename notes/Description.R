#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict quantile rbinom
#' @importFrom grid unit
#' #' @importFrom data.table ':=' as.data.table data.table is.data.table melt rbindlist setnames
Package: vismi
Type: Package
Title: Visualisation Tools for Multiple Imputation
Version: 0.3.0
Authors@R: c(
  person(given = "Yongshi", family ="Deng", email = "yongshi.deng@auckland.ac.nz",
         role = c("aut","cre"),comment =c(ORCID = "0000-0001-5845-859X")),
  person(given = "Thomas", family = "Lumley", email = "t.lumley@auckland.ac.nz",
         role = "ths")
)
Description: A comprehensive suite of visual diagnostic functions for assessing the
quality of multiply-imputed data. 'vismi' provides functions for inspecting
distribution characteristics and overimputation diagnostics. Flexible graphics
functions are powered by 'plotly', 'ggplot2', 'ggridges' and 'trelliscopejs'. Details of
this work can be found Deng (2024) <https://hdl.handle.net/2292/70308>.
URL: https://github.com/agnesdeng/vismi
BugReports: https://github.com/agnesdeng/vismi/issues
License: GPL (>= 3)
Encoding: UTF-8
Language: en-GB
LazyData: true
Depends: R (>= 4.3.0)
Imports:
  data.table,
dplyr,
forcats,
ggplot2 (>= 4.0.1),
ggridges,
gridExtra,
magrittr,
plotly,
rlang,
stats
scales,
tibble,
tidyr,
trelliscopejs,
utils
Suggests:
  knitr,
rmarkdown,
testthat (>= 3.0.0)
RoxygenNote: 7.3.3
