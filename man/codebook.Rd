% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codebook.R
\name{codebook}
\alias{codebook}
\title{interactive exploratory analysis on data.}
\usage{
codebook(x, desc = NA, MISS_cut = 0.2, mapinfo = NA, ncol = 4, ...)
}
\arguments{
\item{x}{a data.frame or tibble object.}

\item{desc}{column description file ( 2 column data frame which containing column_name and description field)}

\item{MISS_column_cut}{Missing ratio threshold of for data showing. the data below the}
}
\description{
`codebook` opens interactive shiny app for a given data and description data frame
}
\examples{
codebook(starwars,starwars_desc)
}
\author{
Byungju Kim (bjk@inu.ac.kr)
}
