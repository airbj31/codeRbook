#' makeValueBox2
#'
#' creat static value boxes for character columns.
#' @author byungju kim (bjk@inu.ac.kr)
#' @import bslib
#' @export
makeValuebox2 <-function(x,max_height="220px") {
  if("icon" %in% names(x) & !is.na(x["icon"])) {iconname <-x["icon"]}  else {iconname<-sample(bsicons:::icon_info$name,1)}
  if("desc" %in% names(x) & !is.na(x["desc"])) { desc<-p(x["desc"])} else {desc<-""}
  if("color" %in% names(x)) { column_color <- x["color"]} else {column_color<-randomRGB(1) }
  return(value_box(
    title = paste("#",x['columns']),
    value = x['n_uniq_Val'],
    style = paste0("background-color: ",column_color,"!important;"),
    showcase = bsicons::bs_icon(iconname),
    showcase_layout = showcase_top_right(),
    p(paste("type", x["col_type"])),
    p(paste(x["nMISS"],"Missing values")),
    min_height=220,
    max_height=max_height,
    full_screen = FALSE
  ))
}
