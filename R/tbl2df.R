#' Conversion of table to data frame
#'
#' the function converts 1D-table into data frame
#' @name tbl2df
#' @author Byungju Kim (bjk@inu.ac.kr)
#' @param x a data.frame or tibble object
#'
#' @import tidyverse
#' @export
tbl2df <- function(x) {
  return(x |> as.data.frame() |> rename("Category"="Var1"))
}

