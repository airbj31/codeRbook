% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSidebar.R, R/make_dimension_cards.R
\name{make_dimension_cards}
\alias{make_dimension_cards}
\alias{makePlotSideBar}
\title{make sidebar for plot control}
\usage{
makePlotSideBar(catnames = c(), numnames = c())

make_dimension_cards(
  desc_obj,
  bgcolor = "#ffffff",
  fgcolor = "#696969",
  css_style = "border: 1px dashed #696969;",
  max_height = "300px"
)
}
\description{
this function return a sidebar object for controling plot

this function return a list of two value_box. one for dimension and the other for variables with pie-chart
}
\author{
byungju@bu.edu

bjkim@inu.ac.kr
}
