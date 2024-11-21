#' make sidebar for plot control
#'
#' @description
#' this function return a sidebar object for controling plot
#'
#' @name make_dimension_cards
#' @author byungju@bu.edu
#' @export

makePlotSideBar <- function(catnames=c(),numnames=c()) {
  sidebar(
    accordion(
      accordion_panel("axis & chart type",
                      selectizeInput("axis","axis:",
                                     choice=c(catnames,numnames),
                                     selected=numnames[1:2],multiple=TRUE),
                      selectizeInput("charttype","graph type",c("scatter","box plot","violin","density","bar"),selected="scatter"),
                      uiOutput("donut"), ## pie chart options
                      selectizeInput("group","group:",
                                     choice=c(catnames),
                                     selected=catnames[1]),
                      uiOutput("ChartOption"),
                      icon=icon("chart-simple")),
      accordion_panel("chart",icon=icon("chart-simple")),
      accordion_panel("aesthetic",icon=icon("brush")),
      accordion(
        accordion_panel("color",icon=icon("brush")),
        accordion_panel("shape",icon=icon("shapes")),
        accordion_panel("size",icon=bs_icon("arrows-vertical")),
        accordion_panel("text",icon=icon("font"))
      ),
      accordion_panel("layout",icon=bs_icon("columns"))
    )
  )
}
