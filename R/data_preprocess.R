# Convert a factor to an integer variable.
fac2int <- function(vec) {
  suppressWarnings(int.levels <- as.integer(levels(vec)))
  if (any(is.na(int.levels))) {
    as.integer(vec)
  } else {
    int.levels[vec]
  }
}

# Classify the type of each variable in a dataset
feature_type <- function(data) {
  # @param data A data.frame or a data.table
  # @return The type (numeric/integer/binary/multiclass) of each variable in a dataset

  Types <- sapply(data, class)

  if (any(Types == "character")) {
    stop("Data contains variables of character type. Please change them into factor.")
  }

  # ordinal.idx<-grep("ordered",Types)
  ord.fac <- names(Filter(is.ordered, data))
  if (length(ord.fac) > 0) {
    Types[ord.fac] <- "factor"
  }

  factor.vars <- which(Types == "factor")
  for (fac in factor.vars) {
    if (length(levels(data[[fac]])) == 2) {
      Types[fac] <- "binary"
    } else {
      Types[fac] <- "multiclass"
    }
  }

  return(Types)
}

