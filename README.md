
library(shiny)
library(shinydashboard)
library(leaflet)
library(rvest)
library(dplyr)


ui <- dashboardPage(
  dashboardHeader(title = "Earthquakes Map"),
  dashboardSidebar(
    dateRangeInput("dates", "Date range:",
                   start = Sys.Date() - 30, end = Sys.Date()),
    numericInput("mag", "Minimum magnitude:", 4, min = 2, max = 10, step = 0.1),
    actionButton("update", "Update Map")
  ),
  dashboardBody(
    leafletOutput("map")
  )
)


server <- function(input, output, session) {
  
  # Function to scrape earthquake data
  scrape_earthquake_data <- function(start_date, end_date, min_mag) {
    url <- sprintf("https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson&starttime=%s&endtime=%s&minmagnitude=%s",
                   start_date, end_date, min_mag)
    webpage <- read_html(url)
    earthquakes <- html_nodes(webpage, "pre") %>%
      html_text() %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    return(earthquakes)
  }
  

  earthquakes <- reactive({
    scrape_earthquake_data(format(input$dates[1]), format(input$dates[2]), input$mag)
  })
  

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -100, lat = 40, zoom = 3)  # Initial map view coordinates
  })
  

  observeEvent(input$update, {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(
        data = earthquakes()$features %>% 
          purrr::map_dfr(~{
            data <- .x$properties
            lng <- .x$geometry$coordinates[1]
            lat <- .x$geometry$coordinates[2]
            data.frame(id = data$mag, popup = paste("Magnitude:", data$mag, "<br>Date:", as.Date(data$time), "<br>Location:", data$place), lng = lng, lat = lat)
          }),
        lng = ~lng,
        lat = ~lat,
        popup = ~popup,
        label = ~id,
        clusterOptions = markerClusterOptions()
      )
  })
}

shinyApp(ui = ui, server = server)
