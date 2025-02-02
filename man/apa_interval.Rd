% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apa_interval.R
\name{apa_interval}
\alias{apa_interval}
\alias{print_interval}
\alias{apa_interval.numeric}
\alias{apa_interval.matrix}
\alias{apa_interval.data.frame}
\alias{apa_interval.list}
\alias{apa_confint}
\alias{print_confint}
\alias{apa_hdint}
\alias{print_hdint}
\title{Typeset Interval Estimate}
\usage{
apa_interval(x, ...)

print_interval(x, ...)

\method{apa_interval}{numeric}(
  x,
  y = NULL,
  ...,
  conf.int = NULL,
  interval_type = NULL,
  enclose_math = FALSE
)

\method{apa_interval}{matrix}(
  x,
  ...,
  conf.int = NULL,
  interval_type = NULL,
  enclose_math = FALSE
)

\method{apa_interval}{data.frame}(
  x,
  ...,
  conf.int = NULL,
  interval_type = NULL,
  enclose_math = FALSE
)

\method{apa_interval}{list}(
  x,
  ...,
  conf.int = NULL,
  interval_type = NULL,
  enclose_math = FALSE
)

apa_confint(
  x,
  ...,
  conf.int = NULL,
  interval_type = "CI",
  enclose_math = FALSE
)

print_confint(
  x,
  ...,
  conf.int = NULL,
  interval_type = "CI",
  enclose_math = FALSE
)

apa_hdint(x, ..., conf.int = NULL, interval_type = "HDI", enclose_math = FALSE)

print_hdint(
  x,
  ...,
  conf.int = NULL,
  interval_type = "HDI",
  enclose_math = FALSE
)
}
\arguments{
\item{x}{Numeric. A vector (of length 2, unless \code{y} is also specified) with,
a two-column \code{matrix}, or a \code{data.frame}, which can coerced to a \code{matrix}.}

\item{...}{Further arguments passed on to \code{\link[=apa_num]{apa_num()}}.}

\item{y}{Numeric. An optional vector of the same length as \code{x}.}

\item{conf.int}{Numeric. Confidence level of the interval. Ignored if
level can be inferred from attributes of \code{x}, see Details.}

\item{interval_type}{Character. Abbreviation indicating the type of interval
estimate, e.g. \code{CI}.}

\item{enclose_math}{Logical. Indicates whether the interval should be
enclosed in \code{$} (i.e., a math environment).}
}
\value{
A single interval is returned as a \code{character} vector of length 1;
multiple intervals are returned as a named \code{list} of \code{character} vectors
of length 1.
}
\description{
Creates a character string to report interval estimates, such as frequentist
confidence or Bayesian credible intervals.
}
\details{
If possible the confidence level of the interval is inferred from
attributes of \code{x}. For a vector of length 2, the attribute \code{conf.level} is
is consulted; for a \code{matrix} or \code{data.frame} the column names are used,
if they are of the format "2.5 \\%" and "97.5 \\%".

If \code{x} is a \code{matrix} or \code{data.frame} the row names are used as names for
the returned \code{list} of intervals.
}
\examples{
apa_confint(1, 2, conf.int = 0.95)
apa_confint(c(1, 2), conf.int = 0.95)
apa_confint(matrix(c(1, 2), ncol = 2), conf.int = 0.95)
apa_confint(confint(lm(cars)))
apa_confint(confint(lm(cars)), digits = 3)
}
\seealso{
\code{\link[=apa_num]{apa_num()}}
}
