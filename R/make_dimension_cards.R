#' Generate dimension value boxes
#'
#' @description
#' this function return a list of two value_box. one for dimension and the other for variables with pie-chart
#'
#' @name make_dimension_cards
#' @author bjkim@inu.ac.kr
#' @import viridis
#' @import bslib
#' @import plotly
#' @export
make_dimension_cards <- function(desc_obj,
                                 bgcolor="#ffffff",
                                 fgcolor="#696969",
                                 css_style="border: 1px dashed #696969;",
                                 max_height="300px") {

  if(!inherits(desc_obj,what="df_desc")) stop("desc_obj should be 'df_desc' object")

    ## column missing rate (box-2)
   sparkpie <- desc_obj$df_summary |> group_by(col_type) |> count() |>
    plot_ly(labels=~col_type,values=~n,textinfo= 'label+percent',span=I(1),color = I("white"),
            marker = list(colors = viridis_pal()(length(unique(desc_obj$df_summary$col_type)))),
            pull=0.05,type="pie",hole=0.6
    ) %>%
    layout(
      xaxis = list(visible=F,showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(visible=F,showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      showlegend =F,
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = "white"),
      uniformtext=list(mode="hide"),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) |>
    config(displayModeBar = F) |>
    htmlwidgets::onRender(
      "function(el) {
                                  var ro = new ResizeObserver(function() {
                                  var visible = el.offsetHeight > 200;
                                  Plotly.relayout(el, {'xaxis.visible': visible});
                                  Plotly.relayout(el, {'showlegend': visible});
                                });
                                ro.observe(el);
                                }"
    )

   ## column missing rate (box-4)
   FMISSTEXT <-paste("type: ",desc_obj$df_summary$col_type,"<br>",desc_obj$df_summary$nMISS,"missing items")
   sparkMissing <- desc_obj$df_summary |> mutate(fMISS=nMISS/desc_obj$dimension[1]) |>
     plot_ly(x=~columns,y=~fMISS * 100,color = ~col_type,colors=viridis_pal()(length(unique(desc_obj$df_summary$col_type))),type="bar",
             hovertext=FMISSTEXT,hoverinfo= 'percent+text',
             hovertemplate="column: %{label}  <br> %{hovertext} (%{value:.2f} %)"
             ) %>%
     layout(
       xaxis = list(visible=F,showgrid = T, zeroline = T, showticklabels = T,
                    categoryorder = "array",
                    categoryarray = desc_obj$df_summary$columns),
       yaxis = list(visible=F,showgrid = T, zeroline = T, showticklabels = T,
                    title="% Missing ratio"),
       showlegend =F,
       hovermode = "x",
       margin = list(t = 0, r = 0, l = 0, b = 0),
       font   = list(color = fgcolor),
       paper_bgcolor = "transparent",
       plot_bgcolor = "transparent"
     ) |>
     config(displayModeBar = F) |>
     htmlwidgets::onRender(
       "function(el) {
                                  var ro = new ResizeObserver(function() {
                                  var visible = el.offsetHeight > 200;
                                  Plotly.relayout(el, {'xaxis.visible': visible});
                                  Plotly.relayout(el, {'yaxis.visible': visible});
                                  Plotly.relayout(el, {'show.legend': visible});
                                });
                                ro.observe(el);
                                }"
     )

    ##hovertext=FMISSTEXT,hoverinfo= 'percent+text',
   ## hovertemplate="column: %{label}  <br> %{hovertext} (%{value:.2f} %)"
    #FMISSTEXT2 <-paste(desc_obj$obs_fMISS,"% missing")
    sparkMissing2 <-  plot_ly(x=1:length(desc_obj$obs_fMISS),
                              y=desc_obj$obs_fMISS * 100,type="bar",
                              name="Observation Missing Ratio",
                              color=I(fgcolor),
                              hovertemplate="observation: %{x}  <br> %{value:.2f} %"
                              ) |>
     layout(
       xaxis = list(visible=F,showgrid = T, zeroline = T, showticklabels = T,
                    categoryorder = "array",
                    categoryarray = desc_obj$df_summary$columns),
       yaxis = list(visible=F,showgrid = T, zeroline = T, showticklabels = T,
                    title="% Missing ratio"),
       showlegend =F,
       hovermode = "x",
       margin = list(t = 0, r = 0, l = 0, b = 0),
       font   = list(color = fgcolor),
       paper_bgcolor = "transparent",
       plot_bgcolor = "transparent"
     ) |>
     config(displayModeBar = F) |>
     htmlwidgets::onRender(
       "function(el) {
                                  var ro = new ResizeObserver(function() {
                                  var visible = el.offsetHeight > 200;
                                  Plotly.relayout(el, {'xaxis.visible': visible});
                                  Plotly.relayout(el, {'yaxis.visible': visible});
                                  Plotly.relayout(el, {'show.legend': visible});
                                });
                                ro.observe(el);
                                }"
     )


  out <- list(
        value_box(
           title = tooltip(
             span("# Observation ", bsicons::bs_icon("question-circle")),
             "Number of observation (=number of rows)",
             placement = "right"
           ),
           value = desc_obj$dimension[1],
           theme = value_box_theme(fg = fgcolor,bg = bgcolor),
           showcase = bsicons::bs_icon("boxes"),
           showcase_layout = "left center",
           style = css_style,
           max_height=max_height),
        value_box(
          title = tooltip(
            span("# Variables ", bsicons::bs_icon("question-circle")),
            "Number of variables (=number of columns)",
            placement = "right"
          ),
          value = desc_obj$dimension[2],
          showcase = sparkpie,
          showcase_layout = "left center",
          theme = value_box_theme(fg = fgcolor,bg = bgcolor),
          style = css_style,
          full_screen= TRUE,
          max_height=max_height),
        value_box(
          title = tooltip(
            span("% Obs Missing Ratio", bsicons::bs_icon("question-circle")),
            "% Average missing ratio. you can check every row's missing ratio by enlarging this card.",
            placement = "right"
          ),
          value = paste0(prettyNum(mean(desc_obj$obs_fMISS)*100,digits=2),"±",
                         prettyNum(sd(desc_obj$obs_fMISS)*100,digits=2), "%"),
          showcase = sparkMissing2,
          showcase_layout = showcase_bottom(), ## max_height = "200px"
          theme = value_box_theme(fg = fgcolor,bg = bgcolor),
          style = css_style,
          full_screen= TRUE,
          max_height=max_height),
        value_box(
          title = tooltip(
            span("% Missing Ratio", bsicons::bs_icon("question-circle")),
            "% Average missing ratio. you can check every column's missing ratio by enlarge this card.",
            placement = "right"
          ),
          value = paste0(prettyNum(mean(desc_obj$df_summary$fMISS)*100,digits=2),"±",
                         prettyNum(sd(desc_obj$df_summary$fMISS)*100,digits=2), "%"),
          showcase = sparkMissing,
          showcase_layout = showcase_bottom(),
          theme = value_box_theme(fg = fgcolor,bg = bgcolor),
          style = css_style,
          full_screen= TRUE,
          max_height=max_height)
  )
return(out)
}


