% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima.R
\name{store.Arima}
\alias{store.Arima}
\title{Store residuals from a model}
\usage{
\method{store}{Arima}(dataset, object, name = "residuals", envir = parent.frame(), ...)
}
\arguments{
\item{dataset}{Dataset to append residuals to}

\item{object}{Return value from a model function}

\item{name}{Variable name(s) assigned to model residuals}

\item{envir}{Environment to evaluate filter expressions in}

\item{...}{Additional arguments}
}
\description{
Store residuals from a model
}
\details{
The store method for objects of class "model". Adds model residuals to the dataset while handling missing values and filters. See \url{https://radiant-rstats.github.io/docs/model/Arima.html} for an example in Radiant
}
\examples{
Arima(diamonds, "price", order = c(1, 0, 1)) \%>\%
 store(diamonds, ., name = "resid") \%>\%
 head()

}
