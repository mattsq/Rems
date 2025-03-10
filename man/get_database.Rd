% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flight.R
\name{get_database}
\alias{get_database}
\alias{get_database.Flight}
\title{Get database information}
\usage{
get_database(qry, ...)

\method{get_database}{Flight}(x, ...)
}
\arguments{
\item{...}{Additional arguments passed to methods}

\item{x}{A Flight object}
}
\value{
Database information (implementation depends on the class of x)

A list containing database information including id, name, and node type
}
\description{
Generic function to retrieve database information from different objects.
}
\section{Methods (by class)}{
\itemize{
\item \code{get_database(Flight)}: Get database information from a Flight object

}}
