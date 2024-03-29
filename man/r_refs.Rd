% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r_refs.R
\name{r_refs}
\alias{r_refs}
\alias{create_bib}
\title{Create a Reference File for R and R Packages}
\usage{
r_refs(
  file,
  append = TRUE,
  prefix = "R-",
  type_pref = c("Article", "Book"),
  tweak = TRUE
)

create_bib(
  x,
  file,
  append = TRUE,
  prefix = "R-",
  type_pref = c("Article", "Book"),
  tweak = TRUE
)
}
\arguments{
\item{file}{Character. Path and name of the file to be created or
updated.}

\item{append}{Logical. Indicates if existing bibliography should be
complemented or overwritten. See details.}

\item{prefix}{Character. Prefix for all R-package reference handles.}

\item{type_pref}{Character. A vector of BibTeX entry types in the order by
which to prioritize packages \code{CITATION} entries. See details.}

\item{tweak}{Logical. Indicates whether to fix some known problems in
citations (based on \code{\link[knitr]{write_bib}}).}

\item{x}{Character. Names of packages to include in bibliography.}
}
\value{
Invisibly returns the bibliography written to \code{file}.
}
\description{
Creates a \code{.bib}-reference file for the installed R version and R-packages,
so they can be cited in an R Markdown-document.
}
\details{
\code{r_refs} is a wrapper for \code{\link{create_bib}} to create a
bibliography for R and all attached or cached packages.

By default, if a file exists at the specified location, \code{r_refs} reads the
file and appends missing citation information to the end of the file
(\code{\link{create_bib}} always overwrites existing files). It is
recommended to use a bibliography-file dedicated to R-references.

\strong{Beware that chunks loading packages should generally \emph{not} be cached}.
\code{r_refs} will make all packages loaded in cached chunks citable, but it
won't know when you remove a package from a cached chunk. This can result
in unused package references in your bibliography-file that will be cited
when using \code{\link{cite_r}}.

If a package provides citation information in a \code{CITATION} file, a
reference is selected based on the preferred order of reference types
specified in \code{type_pref}. By default, available articles are cited rather
than books. If no reference of the specified types is available, the first
reference is used. If multiple references of the preferred type are given
all of them are cited. Finally, if no \code{CITATION} file exists a reference
is generated from the \code{DESCRIPTION} file by \code{\link[utils]{citation}}.
}
\seealso{
\code{\link[=cite_r]{cite_r()}}, \code{\link[knitr:write_bib]{knitr::write_bib()}}, \code{\link[utils:citation]{utils::citation()}}, \code{\link[utils:toLatex]{utils::toLatex()}}
}
