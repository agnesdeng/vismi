# Script to (re)create the precomputed imputed dataset used in the package.
# Run this locally if you want to regenerate `data/imputed_mixgb_nhanes3.rda`.

# Usage: in R console at package root:
#   source('data-raw/create_imputed_data.R')

if (!requireNamespace("mixgb", quietly = TRUE)) {
  stop("Please install the 'mixgb' package to run this script: install.packages('mixgb') or remotes::install_github('...')")
}

#library(mixgb)
devtools::load_all("/Users/agnes/Desktop/mixgb")

data("nhanes3_newborn", package = "mixgb")
set.seed(2025)
withNA.df <- createNA(data = nhanes3_newborn,var.names = c(
  "HSHSIZER", "HSAGEIR", "HSSEX",
  "DMARETHN", "HYD1"),p = 0.1
)
colSums(is.na(withNA.df))
imp_nhanes3_newborn<- mixgb(data = withNA.df, m = 5, pmm.type = "auto")
# Save the resulting object into data/ as an .rda for inclusion in the package
if (!dir.exists('data')) dir.create('data')

na_nhanes3_newborn <- withNA.df
save(na_nhanes3_newborn, file = 'data/na_nhanes3_newborn.rda', compress = 'xz')

save(imp_nhanes3_newborn, file = 'data/imp_nhanes3_newborn.rda', compress = 'xz')
cat('Saved data/imp_nhanes3_newborn.rda\n')





mixgbPMM.data <- mixgb(data = withNA.df, m = 5, pmm.type = "auto")
mixgb.data <- mixgb(data = withNA.df, m = 5, pmm.type = NULL)


data("nhanes3_newborn", package = "mixgb")
set.seed(2025)
params <- list(max_depth = 3, subsample = 0.8, nthread = 2)
# small example m=3 to keep size modest
imp_nhanes3_newborn <- mixgb::mixgb(data = nhanes3_newborn, m = 3, xgb.params = params, nrounds = 30)

# Ensure the saved .rda contains the object named `imp_nhanes3_newborn`

# Save the resulting object into data/ as an .rda for inclusion in the package
if (!dir.exists('data')) dir.create('data')
save(imp_nhanes3_newborn, file = 'data/imp_nhanes3_newborn.rda', compress = 'xz')
cat('Saved data/imp_nhanes3_newborn.rda\n')

# Note: do not run devtools::document() automatically here in data-raw script;
# run locally after creating the data file if you want to update documentation.

colSums(is.na(nhanes3_newborn))
