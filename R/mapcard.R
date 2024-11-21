#' map card
#'
#' MAP UI/Server module for card
#'
#' @name MapCard
#' @author bjkim@inu.ac.kr
#' @import viridis
#' @import bslib
#' @import shinycssloaders
#' @import plotly
#' @import leaflet
MapCard <- function(mapinfo,
                    bgcolor="#ffffff",
                    fgcolor="#696969",
                    css_style="border: 1px dashed #696969;",
                    max_height="180px") {
  card(
    card_body(
      withSpinner(leafletOutput(ns('mapplot')))
    )

  )

}

MapCard_srv <- function(id,data=mapinfo){
  moduleServer(
    id,
    function(input,output,session ,data) {
      ns<-session$ns

      lat <- x |> pull(data[1])
      lon <- x |> pull(data[2])

      if(length(data) > 2) {
        grp <- x |> pill(data[3])
      }
      output$mapplot <-renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = FALSE)) |>
          addTiles() |>
          setView(127.41972, 35.82139, zoom = 3)
      })

      outputOptions(output, "mapplot", suspendWhenHidden = FALSE)
      output$test <- renderPrint({input$excNA})

      observeEvent({
##        input$tab
##        input$hapgrp
##        input$date
##        input$excNA
      },
      {
        m<-leafletProxy("mapplot", session) %>% clearMarkers() %>% clearMarkerClusters()

        if(isTRUE(input$excNA)) {

           m <-  m %>%  addCircleMarkers(data=data %>% dplyr::filter(grpA=="K",Year >= input$date[1],Year <= input$date[2]),                   lng =~wgs84.lng, lat=~wgs84.lat,popup =~ txt,color="red",radius = 5,weight=1.5,stroke=T,fillColor="red",fillOpacity=0.2)
        m

      }
      })

    })}
