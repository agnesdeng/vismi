% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overimpute_cart.R
\name{overimpute_cart}
\alias{overimpute_cart}
\title{Overimputation using mice cart (can only do with training data)}
\usage{
overimpute_cart(
  train.data,
  test.data = NULL,
  seed = NULL,
  p = 0.2,
  m = 5,
  maxit = 5,
  printFlag = FALSE,
  ...
)
}
\arguments{
\item{train.data}{A data.frame with missing values}

\item{p}{The proportion of extra missing values}

\item{m}{The number of overimputed datasets. Default: 5}

\item{maxit}{The number of imputation iterations. Default: 1}

\item{printFlag}{Verbose setting of the mice package. If TRUE, it will print out information. Default: FALSE.}

\item{...}{Extra arguments to pass to mice}
}
\value{
an overimpute object
}
\description{
Create extra missing values and overimpute them
}
