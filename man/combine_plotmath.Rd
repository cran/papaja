% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_plot.R
\name{combine_plotmath}
\alias{combine_plotmath}
\title{Combine to Expression}
\usage{
combine_plotmath(x)
}
\arguments{
\item{x}{A \code{list} that contains all elements that are intended to be coerced into one expression.}
}
\value{
An expression
}
\description{
We use this internal function to generate expressions that can be used for plotting. Accepts a list of elements that are coerced,
currently supported elements are \code{character}, \code{expression}, and \code{character} that contain \code{latex} elements.
\emph{This function is not exported.}
}
\keyword{internal}
