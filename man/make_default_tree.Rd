% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flight.R
\name{make_default_tree}
\alias{make_default_tree}
\title{Create the default field tree for a Flight object}
\usage{
make_default_tree(flt, exclude_dirs = NULL)
}
\arguments{
\item{flt}{A Flight object}

\item{exclude_dirs}{Optional character vector of directories to exclude.
If NULL, the package default exclusions will be used.}
}
\value{
Updated Flight object with populated field tree
}
\description{
Populates the field tree with default fields, excluding specified directories.
}
