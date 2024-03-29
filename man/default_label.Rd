% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variable_label.R
\name{default_label}
\alias{default_label}
\alias{default_label.default}
\alias{default_label.data.frame}
\title{Set Default Variable Labels from Column Names}
\usage{
default_label(x, ...)

\method{default_label}{default}(x, ...)

\method{default_label}{data.frame}(x, ...)
}
\arguments{
\item{x}{A \code{data.frame}}
}
\value{
Returns a \code{data.frame} with labelled columns. Labels are preserved (if already specified), otherwise generated from column names.
}
\description{
This internal function creates variable labels from the column names of a1
data frame.
}
\keyword{internal}
