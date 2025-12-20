if (any(types == "character")) {

  proceed <- utils::askYesNo(
    "Data contains character variables. Treat them as factor?"
  )

  if (isTRUE(proceed) {
    data[, vars] <- lapply(data[, vars, drop = FALSE], function(x) {
      if (is.character(x)) factor(x) else x
    })
  } else {
    stop("Operation aborted: character variables were not converted into factor.")
  }
}


# ordinal.idx<-grep("ordered",Types)
ord.fac <- names(Filter(is.ordered, data))
if (length(ord.fac) > 0) {
  types[ord.fac] <- "factor"
}
