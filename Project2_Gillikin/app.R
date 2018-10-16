# Project 2

library(shiny)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(readxl)
library(stringr)
library(sp)


ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

types <- sort(ckanUniques("76fda9d0-69be-4dd5-8108-0de7907fc5a4", "REQUEST_TYPE")$REQUEST_TYPE)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("City of Pittsburgh 311 Dashboard"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates",
                     "Select Dates",
                     start = Sys.Date()-30,
                     end = Sys.Date()),
      selectInput("type_select",
                  "Request Type",
                  choices = types,
                  selected = "Potholes")
    ),
    
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Line Plot",
                 plotlyOutput("linePlot")
        ),
        tabPanel("Open/Closed",
                 plotlyOutput("barChart"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  load311 <- reactive({
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$dates[2], "%27%20AND%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
    
    # Load and clean data
    dat311 <- ckanSQL(url) %>%
      mutate(date = as.Date(CREATED_ON),
             STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    
    return(dat311)
  })
  output$linePlot <- renderPlotly({
    dat311 <- load311()
    
    # shape the data for chart
    table <- dat311 %>%
      group_by(date) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(table, aes(x = date, y = count)) +
      geom_point(colour = "#d95f02") +
      geom_line(colour = "#d95f02") +
      geom_smooth()
  })
  output$barChart <- renderPlotly({
    dat311 <- load311()
    
    # shape the data for chart
    table <- dat311 %>%
      group_by(STATUS) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(table, aes(x = STATUS, y = count, fill = STATUS)) +
      geom_bar(stat = "identity")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


leaflet() %>%
  # Set custom view
  setView(-78.4800, 38.03, 13) %>%
  # Base Groups ---
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.HOT", group = "HOT") %>%
  addProviderTiles("OpenTopoMap", group = "TopoMap") %>%
  # Overlay Groups ---
  # Add clusters of markers for affordable housing units
  addCircleMarkers(data = housing.units, lng = ~long, lat = ~lat, radius = 2, color = ~pal(type), clusterOptions = markerClusterOptions(), group = "Affordable housing units") %>%
  # Add legend for circle markers
  addLegend(position = "topright" , pal = palet, values = housing.units$type, title = "Affordable housing units by type") %>%
  # Add path connecting selected points of interest
  addPolylines(data = path.interests, lng = ~long, lat = ~lat, color = "blue", group = "Path of places of interest") %>%
  # Add census tracts
  addPolygons(data = tract, fill = F, weight = 3, color = "#444444", group = "Census tracts") %>%
  # Layers control ---
  addLayersControl(
    baseGroups = c("OSM (default)", "HOT", "TopoMap"),
    overlayGroups = c("Affordable housing units", "Census tracts", "Path of places of interest"),
    options = layersControlOptions(collapsed = FALSE))

