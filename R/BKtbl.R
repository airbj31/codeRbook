#` Table extension for list.
#'
#' base R's `table` extension which can receive `list` type
#'
#' @author Byungju Kim (byungju@bu.edu)
#' @param x vector, and list objects.
#' @examples
#' BKtbl(starwars$films)
#' @export

BKtbl <- function(x,...)
{
  if(class(x)=="list") {return(table(unlist(x),useNA="ifany"))}
  else {return(table(x,useNA="ifany"))}
}
