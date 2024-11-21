#' summary_ui/summary_srv module
#'
#'

SparkyCard_ui  <- function(id,column_name,class="p-0",height=200) {
  ns<-NS(id)
  page_fluid(
    card(
      height = height,
      style = "resize:vertical;",
      full_screen = TRUE,
      card_header(column_name),
      card_body(class = "lead container",
                textOutput(ns("counts")),
                lorem::ipsum(paragraphs = 10, sentences = 5),height=height/2
                ),
      card_body(class=class, plotOutput(ns("cardplot")),height=height/2)
    ))
}

SparkyCard_srv <- function(id,data,chart_type=c("pie","Donut","histogram","box","violin","bar","line"),digits=2, big.mark = ",",height) {

  moduleServer(
    id,
    function(id,output,session,chart_type,...) {
      ns<-session$ns
      data<- reactive({describe_df(starwars)$category_column_summary$sex})

      output$counts <- renderText({prettyNum(nrow(data()),...)})
      output$cardplot <- renderPlot({
        info <- getCurrentOutputInfo()
        if (info$height() > 600) {
          ggplot(data(),aes(Category,Freq)) + geom_col(fill="black") + theme_void()
        } else {
          ggplot(data(),aes(Category,Freq)) + geom_col(fill="black") + theme_bw()
        }

  })
  })
}

##

demo <- function() {

  df <-describe_df(starwars)
  ui <- fluidPage(SparkyCard_ui("x","sex"))
  server <- function(input, output, session) {
    SparkyCard_srv("x")
  }
  shinyApp(ui, server)
}



