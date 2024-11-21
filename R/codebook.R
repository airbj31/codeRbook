#' interactive exploratory analysis on data.
#'
#' @description
#' `codebook` opens interactive shiny app for a given data and description data frame
#'
#' @name codebook
#' @author Byungju Kim (bjk@inu.ac.kr)
#' @param x a data.frame or tibble object.
#' @param desc column description file ( 2 column data frame which containing column_name and description field)
#' @param MISS_column_cut Missing ratio threshold of for data showing. the data below the
#' @import dplyr
#' @import tidyverse
#' @import shiny
#' @import bslib
#' @import bsicons
#' @import sparkline
#' @import htmlwidgets
#' @import plotly
#' @import reactable
#' @import viridis
#' @import htmltools
#' @import waffle
#' @import shinyWidgets
#' @examples
#' codebook(starwars,starwars_desc)
#' @export

codebook <- function(x,desc=NA,MISS_cut=0.2,mapinfo=NA,ncol=4, ...) {
  if (interactive()) {
  dfname <- deparse(substitute(x))

  d <- describe_df(x,show_freq=T,keepValues = T,...)

  catnames <- names(d$category_column_summary)
  numnames <- d$numeric_column_summary$column
  charnames <- d$df_summary |> dplyr::filter(col_type=="character") |> pull(columns)
  listnames <- d$df_summary |> dplyr::filter(col_type=="list") |> pull(columns)

  ##plot_ctrl <- makePlotSideBar(catnames,numnames)
  ##message(plot_ctrl)

  if(is.data.frame(desc) &  "columns" %in% colnames(desc)) {
      d$df_summary <- d$df_summary %>% left_join(desc)
  }

  ui<-codebook_ui(title=dfname,
                  df_desc=d,
                  catnames=catnames,
                  numnames=numnames,
                  listnames=listnames,
                  charnames=charnames)
                  ##plot_menu=plot_ctrl)




  server<- function(input,
                    output,
                    session,
                    data=x,
                    desc_obj=d,
                    catns=catnames,
                    nums=numnames,
                    lsts=listnames,
                    charns=charnames) {
                    ##plot_menu=plot_ctrl) {

    rv <- reactiveValues()
    rv$data             <- data
    rv$desc_obj         <- d
    rv$numnames         <- nums
    rv$catnames         <- catns
    rv$listnames        <- nums
    rv$charns           <- charns

    output$debug <- renderPrint({input$charttype})

    observeEvent({
      input$axis
      }, {

      if(length(input$axis) == 1) {
        if(input$axis %in% rv$numnames)  {

          ## 1x numeric one variable plot
          updateSelectizeInput(session,"charttype","graph type:",
                               choice=c("histogram","density"),
                               selected="histogram")
        } else {
          ## 1x category variable plot
          updateSelectizeInput(session,"charttype","graph type:",
                               choice=c("bar","lollipop","waffle","wordcloud",
                                        "pie","treemap","circularpacking"),
                               selected="bar"
                               )
        }
      } else if(length(input$axis) == 2) {
           req(input$charttype)
          ## 2x num x num
          if(input$axis[[1]] %in% rv$numnames && input$axis[[2]] %in% rv$numnames) {
            updateSelectizeInput(session,"charttype","graph type:",
                                 choice=c("scatter","box plot","violin","density2d","bar"),
                                 selected="scatter")
            ## two variables are both numeric
            ## if(input$charttype=="box plot") { }
            ## else if(input$charttype=="histogram") { }
            ## else if(input$charttype=="scatter plot") { }
            ## else if(input$charttype=="violin plot") { }
            ## else if(input$charttype=="scatter plot with marginal point") { }
            ## else if(input$charttype=="2d density plot") { }
            ## else if(input$charttype=="connected scatter plot") { }
            ## else if(input$charttype=="area plot") { }
            ## else if(input$charttype=="line plot") { }

            ## 2x cat x cat
          } else if(input$axis[[1]] %in% rv$catnames && input$axis[[2]] %in% rv$catnames) {
            updateSelectizeInput(session,"charttype","graph type:",
                                 choice=c("Venn diagram","treemap","circular packing",
                                          "sunburst","bar","dendrogram","grouped scatter",
                                          "heatmap","Lollipop","spiderplot","sankey",
                                          "network","chord","arc"),
                                 selected="scatter")
          } else {
            updateSelectizeInput(session,"charttype","graph type:",
                                 choice=c("bar","box plot","lolipop","pie","wordcloud","treemap",
                                          "circular packing","waffle","violin","ridgeline",
                                          "density","histogram","scatter","density2d","violin",
                                          "PCA","correlogram","line","steamed graph","area",
                                          "heatmap","parallel plot","spider plot","sankey"),
                                 selected="bar")


          }
      } else {}

    })


    ## observeEvent(input$charttype,{})


    observeEvent({
      input$axis
      input$charttype},{

            output$ChartOption <- renderUI({
              if(length(input$axis)==1) {
                  if(input$charttype=="histogram") { sliderInput("bins","#bins",min=1,max=100,value=30)
                  } else if(input$charttype=="pie") { sliderInput("diadonut","inner diameter size:",min=0,max=10,step=1,value=2)
                  } else if(input$charttype=="waffle") {
                    tagList(
                      sliderInput("nRow","# rows",min=1,max=10,step=1,value=5),
                      sliderInput("waffle_size","size",min=0.1,max=10,step=0.1,value=0.3))
                  }
              } else if(length(input$axis==2)) {
                if(input$charttype=="violin") {
                    tagList(
                      selectizeInput("violin_scale","scale",c("area","count"),selected="area"),
                      checkboxInput("violin_quantile","quantile",value = F),
                      checkboxInput("violin_trim","trim tails",value = F))
                } else if(input$charttype=="density2d") {
                    tagList(
                      sliderInput("density2d_lwd","line width:",min=0.1,max=10,step=0.1,value=1)

                    )
                } else {}}
            })

            output$AesOption <- renderUI({
              req(input$charttype)
              if(input$charttype=="waffle") {
                selectizeInput("color","color by ",c("NULL",catnames,numnames),selected="NULL")
              } else if(input$charttype=="scatter") {
                tagList(
                  selectizeInput("color","color by ",c("NULL",catnames,numnames),selected="NULL"),
                  selectizeInput("fill","fill by",c("NULL",catnames,numnames),selected="NULL"),
                  selectizeInput("shape","shape by",c("NULL",catnames,numnames),selected="NULL"),
                  selectizeInput("size","shape by",c("NULL",catnames,numnames),selected="NULL"),
                  selectizeInput("alpha","shape by",c("NULL",catnames,numnames),selected="NULL"))
              } else {
                  tagList(
                    selectizeInput("color","color by ",c("NULL",catnames,numnames),selected="NULL"),
                    selectizeInput("fill","fill by",c("NULL",catnames,numnames),selected="NULL")
                  )
                }
            })

    })


    output$plot2d <- renderPlot({

      req(input$axis)
      req(input$charttype)

      if(length(input$axis) == 1) {

        p <- ggplot(rv$data,aes_string(x=input$axis[[1]],color=input$color,fill=input$fill))

        if(as.vector(input$axis) %in% rv$numnames) {

            if(input$charttype == "histogram") {
              p <- p + geom_histogram(bins=input$bins)
            } else {
              p <- p + geom_density(kernel=input$kernel)
            }
        } else if(as.vector(input$axis) %in% rv$catnames) {

           if(input$charttype == "bar") {
              p <- p + geom_bar(stat="count")
           } else if(input$charttype == "lollipop") {
              p <- p + geom_point(stat="count",size=3) + geom_bar(width=0.02)
           } else if(input$charttype == "waffle") {
              req(input$nRow)
              req(input$waffle_size)
              grp <- rlang::sym(input$axis[[1]])
              p <- ggplot(rv$data |> group_by(!!grp) |> count(),
                          aes_string(fill=input$axis[[1]],values="n")) + geom_waffle(n_rows=input$nRow,size=input$waffle_size)

           } else if(input$charttype=="wordcloud") {
             p <- ggplot(rv$data ,aes_string(input$axis[[1]])) + geom_text_wordcloud() + theme_minimal()

           } else if(input$charttype=="pie") {
             req(input$diadonut)
             p <- ggplot(rv$data,aes_string(input$diadonut,fill=input$axis[[1]],color=input$axis[[1]])) + geom_bar(stat="count") + coord_polar(theta="y")
             p <- p + xlim(c(0.2,input$diadonut + 0.5))

           } else if(input$charttype=="treemap") {
           }else if(input$charttype=="circularpacking") { }

        }
      } else if(length(input$axis)==2) {
        req(input$charttype)
        if(input$charttype=="scatter") {
          p<-ggplot(rv$data,aes_string(x=input$axis[[1]],y=input$axis[[2]],color=input$color,fill=input$fill)) + geom_point()
        } else if(input$charttype=="violin") {
          p<-ggplot(rv$data,aes_string(input$axis[[1]],y=input$axis[[2]],color=input$color,fill=input$fill)) + geom_violin(drop = FALSE,draw_quantiles = input$violin_quantile,trim=input$violin_trim,scale = input$violin_scale)
        } else if(input$charttype=="box plot") {
          p<-ggplot(rv$data,aes_string(input$axis[[1]],y=input$axis[[2]],color=input$color,fill=input$fill)) + geom_boxplot()
        } else if(input$charttype=="density2d") {
          p<-ggplot(rv$data,aes_string(input$axis[[1]],y=input$axis[[2]],color=input$color,fill=input$fill)) + geom_density_2d(linewidth = input$density2d_lwd)
        }
      }

      if(input$charttype %in% c("pie","waffle")) {
        p + theme_void()
      } else { p + theme_bw() }
    })
#    summary_srv("summaryData",desc_obj=d)

    ## summary tables
    output$cat.table <- renderReactable({
      info <- getCurrentOutputInfo()
      summary_tbl <- do.call(rbind, lapply(names(desc_obj$category_column_summary), function(name) {
        df <- desc_obj$category_column_summary[[name]]
        df$column <- name
        df
      }))
      CNAMES <- summary_tbl$column |> unique()
      summary_cat  <- lapply(CNAMES,function(name) {summary_tbl |> dplyr::filter(column==name) |> pull(Category) })
      summary_freq <- lapply(CNAMES,function(name) {summary_tbl |> dplyr::filter(column==name) |> pull(Freq) })
      names(summary_cat)  <- CNAMES
      names(summary_freq) <- CNAMES
      freq_cnt <- lapply(CNAMES,function(name) { length(summary_freq[[name]])})
      CATTBL <- tibble(column=CNAMES,nCategory=freq_cnt |> unlist(),category=summary_cat,freq=summary_freq)



      if (info$height() > 600) {
        reactable(CATTBL,
                  columns=list(
                    freq = colDef(cell=function(values) {
                      sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(summary_tbl$Freq))
                    })
                  ))
      } else {
        reactable(CATTBL |> select(-category),
                  columns=list(
                    freq = colDef(cell=function(values) {
                      sparkline(values, type = "bar", chartRangeMin = 0, chartRangeMax = max(summary_tbl$Freq))
                    })
                  ))
      }



    })
    output$num.table <- renderReactable({

       reactable(rv$desc_obj$numeric_column_summary |> dplyr::select(-idx) |> dplyr::select(column,distribution,nMISS,everything()),
                 columns=list(
                   distribution = colDef(cell = function(value, index) {
                     sparklines::sparkline(desc_obj$numeric_column_summary$distribution[[index]], chart_type = "box")
                   }),
                   Min. = colDef(name="Min",format =colFormat(separators= T, digits=2)),
                   QT1  = colDef(format =colFormat(separators= T, digits=2)),
                   Median = colDef(format =colFormat(separators= T, digits=2)),
                   Mean   = colDef(format =colFormat(separators= T, digits=2)),
                   QT3    = colDef(format =colFormat(separators= T, digits=2)),
                   Max.   = colDef(name="Max",format =colFormat(separators= T, digits=2)),
                   nMISS  = colDef(
                     name = "#Missing",
                     format =colFormat(separators= T, digits=0),
                     cell=function(value) {
                       width <- paste0(value/desc_obj$dimension[1] * 100, "%")
                       bar_chart(value, width = width,fill="#969696")
                     }
                     )
                 ),highlight = TRUE)

    })
    ## full table
    output$tbl <- renderReactable({
          reactable(x |> dplyr::select(input$colselection))
      })

    ## summary tables - characters
    output$cha.table <- renderReactable({
        reactable(rv$desc_obj$df_summary |> dplyr::filter(col_type=="character") |> select(-col_type),
                    columns=list(
                      n_uniq_Val =
                        colDef(name="#unique values",
                               align="left",
                               format=colFormat(separators=T,digits=0),
                               cell=function(value) {
                                           width <- paste0(value / max(desc_obj$df_summary |> dplyr::filter(col_type=="character") |> pull(n_uniq_Val)) * 100, "%")
                                           bar_chart(value, width = width)

                                }),
                      nMISS =colDef(name="#Missing",
                                    align="left",
                                    format=colFormat(separators=T,digits=0),
                                    cell=function(value) {
                                      width <- paste0(value/desc_obj$dimension[1] * 100, "%")
                                      bar_chart(value, width = width,fill="#969696")
                                    }
                                    ),
                      fMISS =colDef(name="%Missing",
                                    align="left",
                                    format = colFormat(percent = TRUE, digits = 1)
                                    )

                    ),
                  highlight = TRUE
                  )
    })




        ## not used
        ## output$plot3d <- renderPlotly({})
        ## output$plot1d <- renderPlot({})

  } # end of server

  shinyApp(ui, server)
  }
}

codebook_ui <-function(title,df_desc,catnames,numnames,listnames,charnames,plot_menu,...) {

  ui<-page_navbar(
  title = title,
  theme=bs_theme(preset="shiny"),
  fillable_mobile = TRUE,
    nav_panel("Summary",
              summary_ui("summaryData",desc_obj=df_desc,ncol=4),
              icon=bsicons::bs_icon("postcard-heart-fill")),
    nav_panel("Chart",
              layout_column_wrap(1/2,
                card(
                  card_header("plot"),
                  layout_sidebar(
                    fillable=TRUE,
                    sidebar= sidebar(
                      accordion(
                        accordion_panel("Axis & Chart",
                                        selectizeInput("axis",span("axis ",tooltip(bs_icon("question-circle"),"selected columns are x, y, and z axis respectively.")),
                                                       choice=c(catnames,numnames),
                                                       selected=numnames[1:2],multiple=TRUE),
                                        selectizeInput("charttype",span("chart ",tooltip(bs_icon("question-circle"),"choose the chart type")),
                                                       c("scatter","box plot","violin","density2d","bar"),selected="scatter"),
                                        uiOutput("ChartOption"), ## pie chart options
                                        verbatimTextOutput("debug"),
                                        selectizeInput("group","group:",
                                                       choice=c(catnames),
                                                       selected=catnames[1]),
                                        ##uiOutput("ChartOption"),
                                        icon=icon("chart-simple")),
                        ##accordion_panel("chart",icon=icon("chart-simple")),
                        accordion_panel("aesthetic mapping",
                                        uiOutput("AesOption"),
                                        icon=icon("brush")),
                        accordion_panel("text/label",icon=icon("font")),
                        accordion_panel("layout",icon=bs_icon("columns"))
                      )
                    ),

                      ##makePlotSideBar(catnames,numnames),
                    card_body(plotOutput("plot2d"))
                  )),
                card(
                  card_header("table"),
                  layout_sidebar(
                    fillable=TRUE,
                    sidebar=sidebar(
                      virtualSelectInput(
                        inputId = "colselection",
                        label = "column selection:",
                        choices = list(
                          category  = catnames,
                          numeric   = numnames,
                          character = charnames,
                          list = listnames),
                        selected=c(catnames,numnames,charnames,listnames),
                        multiple = TRUE,
                        width = "100%",
                        dropboxWrapper = "body"
                      )

                      ,position="right"),
                    card_body(reactableOutput('tbl'))
                ))
              ),
              #crosstalk::bscols(
              #  #explorePlot("Explore data",df=x,nums=numnames,cats=catnames),

              ##sidebar("hi"),
              #),#sidebar_ui(d,cats = catnames,nums = numnames,chars = charnames,list=c()),
              icon=bsicons::bs_icon("graph-up"))
  )
  return(ui)
}

accordion_filters<-function(x,...) {

}


makeValuebox <-function(x) {
  return(value_box(
    title = paste("#unique ",x[1]),
    value = x[3],
    style = paste0("background-color: ",randomRGB(1,100:255,100:255,100:255),"!important;"),
    showcase = bsicons::bs_icon(sample(bsicons:::icon_info$name,1)),
    showcase_layout = showcase_top_right(),
    p(paste(x[4],"Missing values")),
    max_height="180px"
  ))
}

summary_ui <- function(id,label="summary",desc_obj,ncol) {

  ns<-NS(id)
  ##cdf <- desc_obj$df_summary %>% dplyr::filter(col_type=="character")
  dimcards<-make_dimension_cards(desc_obj)

  ##if(dim(cdf)[1]>0) {
  ##  vbs <- apply(cdf,1,makeValuebox2)
    ## vbs <- card(card_header("Character columns"),
    ##            card_body(layout_column_wrap(width=1/2,!!!vbs)))
  ##} else {vbs <-list()}
  accordion(
    accordion_panel(
      "Data Dimesion",
       layout_column_wrap(width=1/ncol,!!!dimcards), #,!!!vbs
      icon=bs_icon("rulers")),
    accordion_panel(
      "Data Summary",
      layout_column_wrap(width=1/(ncol-1),
           card(full_screen = TRUE,
                card_header(class="bg-dark",bs_icon("123"),"numberic variables"),
                card_body(reactableOutput("num.table"))),
          card(full_screen=TRUE,
               card_header(class="bg-dark",bs_icon("tags"),"categorical variables"),
               card_body(reactableOutput("cat.table"))),
          card(full_screen=TRUE,
                card_header(class="bg-dark",bs_icon("alphabet"),"charactor variables"),
                card_body(reactableOutput("cha.table"))),

          )
      ,icon=bsicons::bs_icon("card-text")),
    open =TRUE,
    multiple = TRUE,
    )
}


summary_srv <-function(id,input,output,session,desc_obj,data=x) {
                       ## selected_category,
                       ## selected_list,
                       ## selected_character,
                       ## selected_number
                      ## ) {
  moduleServer(id,
    function(input,output,session) {
      ns<-session$ns
      ##if(!is.na(selected_category)) {output$nCategory  <- renderText({length(selected_category)})}
      ##if(!is.na(selected_category)) {output$nList      <- renderText({length(selected_list)})}
      ##if(!is.na(selected_category)) {output$nCharacter <- renderText({length(selected_character)})}
      ##if(!is.na(selected_category)) {output$nNumber    <- renderText({length(selected_number)})}
      #observeEvent()
      ##ndf <- reactive({desc_obj$numeric_column_summary |> select(-idx)})


    }
  )
}


library(plotly)

sparkline <- plot_ly(economics) %>%
  add_lines(
    x = ~date, y = ~psavert,
    color = I("white"), span = I(1),
    fill = 'tozeroy', alpha = 0.2
  ) %>%
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = ""),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "white"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent"
  ) %>%
  config(displayModeBar = F) %>%
  htmlwidgets::onRender(
    "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
  )

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

sidebar_ui <- function(df_desc,cats,nums,lists,chars) {
  i<-1;
  menuitem <-tagList()
  ##
  ## todo: load data selection menu when df is empty.
  ##

  if(length(cats)>0)  { menuitem[[i]]<-selectizeInput("cat_select",'category:',multiple=T,choices=cats,selected=cats);i<-i+1;}
  if(length(nums)>0)  { menuitem[[i]]<-selectizeInput("num_select",'numbers:',multiple=T,choices=nums,selected=nums);i<-i+1;}
  if(length(lists)>0) { menuitem[[i]]<-selectizeInput("list_select",'list:',multiple=T,choices=lists,selected=lists);i<-i+1;}
  if(length(chars)>0) { menuitem[[i]]<-selectizeInput("char_select",'characters:',multiple=T,choices=chars,selected=chars);i<-i+1;}
  sidebar(
  accordion(
    accordion_panel(
      "data filtering:",menuitem,
      icon = bsicons::bs_icon("menu-app")
    )
  )
  )
}
