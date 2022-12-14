% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_1var.R
\name{plot_hist}
\alias{plot_hist}
\title{Histogram with density plots for multiply imputed values for a single numeric variable}
\usage{
plot_hist(
  imputation.list,
  var.name,
  original.data,
  true.data = NULL,
  color.pal = NULL
)
}
\arguments{
\item{imputation.list}{A list of \code{m} imputed datasets returned by the \code{mixgb} imputer, or other package.}

\item{var.name}{The name of a numeric variable of interest.}

\item{original.data}{The original data with missing values.}

\item{true.data}{The true data without missing values. In general, this is unknown. Only use for simulation studies.}

\item{color.pal}{A vector of hex color codes for the observed and m sets of imputed values panels. The vector should be of length \code{m+1}. Default: NULL (use "gray40" for the observed panel, use ggplot2 default colors for other panels.)}
}
\value{
Histogram with density plots
}
\description{
Plot histograms with density curves of observed values versus m sets of imputed values for a specified numeric variable using \pkg{ggplot2}.
}
\examples{
#obtain m multiply datasets
library(mixgb)
imputed.data <- mixgb(data = nhanes3, m = 3)

#plot the multiply imputed values for variable "BMPHEAD"
plot_hist(imputation.list = imputed.data, var.name = "BMPHEAD",
  original.data = nhanes3)
}
