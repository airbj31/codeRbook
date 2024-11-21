#' Print method for "df_desc" class
#'
#' @param x An object of class "df_desc"
#' @param ... Other arguments passed to or from other methods
#'
#' @export
print.df_desc <- function(x, ...) {
  cat("\033[1;37m# Rows:",x$dim[1],"(avg missing ratio:",prettyNum(mean(x$obs_fMISS) * 100,digits=2),"±",prettyNum(sd(x$obs_fMISS) * 100,digits=2),"%)\n")
  cat("\033[1;37m# Cols:",x$dim[2],"(avg missing ratig:",prettyNum(mean(x$df_summary$nMISS)/x$dimension[1] * 100,digits=2),"±",prettyNum(sd(x$df_summary$nMISS)/x$dimension[1] * 100,digits=2),"%)\n")
  cat("\033[1;37m# suggesting \033[1;34m$col_type_strings \033[0;37mfor reading data: ",x$col_type_string,"\n")
  cat("\033[1;34m$df_summary\033[1;37m# :\n")
  print(x$df_summary)
  cat("\033[1;37m # \033[1;34m$numeric_data_summary\033[1;37m :\n")
  print(x$numeric_column_summary)
  cat("\033[1;37m# \033[1;34m$category_column_summary\033[1;37m:
       We have ",length(x$category_column_summary)," elements with total", sum(unlist(lapply(x$category_column_summary,nrow))-1), "categories")
}
