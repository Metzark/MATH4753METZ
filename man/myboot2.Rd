% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Title myboot2}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{number of iterations}

\item{x}{data vector}

\item{fun}{function to create bootstrap from}

\item{alpha}{confidence interval error}

\item{cx}{padding}

\item{...}{extra arguments for hist}
}
\value{
list with confidence interal, function used, x data vector, xstat, and point estimate
}
\description{
Title myboot2
}
\examples{
myboot2(x=rnorm(25,mean=25,sd=10))
}
