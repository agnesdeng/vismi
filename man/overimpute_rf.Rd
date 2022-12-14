% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overimpute_rf.R
\name{overimpute_rf}
\alias{overimpute_rf}
\title{Overimputation using mice rf (can only do with training data)}
\usage{
overimpute_rf(
  train.data,
  test.data = NULL,
  p = 0.3,
  m = 5,
  maxit = 5,
  rfPackage = "ranger",
  printFlag = FALSE,
  ...
)
}
\arguments{
\item{train.data}{A data.frame with missing values}

\item{p}{The proportion of extra missing values}

\item{m}{The number of overimputed datasets. Default: 5}

\item{maxit}{The number of imputation iterations. Default: 1}

\item{rfPackage}{The backend of rf. Can be either "ranger" or "randomForest". Default: "ranger".}

\item{printFlag}{Verbose setting of the mice package. If TRUE, it will print out information. Default: FALSE.}

\item{...}{Extra arguments to pass to mice}
}
\value{
an overimpute object
}
\description{
Create extra missing values and overimpute them
}
