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
library(httr)
library(shinydashboard)

treeTops <- read.csv("trees.csv")

# Load PGH neighborhood shapefile
#neighborhoods <- rgdal::readOGR("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")


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

neighborhood <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "neighborhood")$neighborhood)
height <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "height")$height)
condition <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "condition")$condition)

header <- dashboardHeader(title = "The Trees of Pittsburgh")

sidebar <- dashboardSidebar(
  # bars on the side
  sidebarMenu(
    id = "tabs",
    menuItem("Map", icon = icon("map"), tabName = "map"),
    menuItem("Charts", icon = icon("bar-chart"), tabName = "charts"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    selectInput("name_select",
                "Neighborhood",
                choices = sort(unique(neighborhood)),
                multiple = TRUE, 
                selectize = TRUE,
                selected = c("Greenfield"))
#      selectInput("condition_select",
#                  "Condition",
#                  choices = sort(unique(condition)),
#                  multiple = TRUE, 
#                  selectize = TRUE,
#                  selected = c("Fair")),
#      selectInput("neighborhood_select",
#                  "Condition",
#                  choices = neighborhood,
#                  selected = "Greenfield",
#                  multiple = TRUE,
#                  selectize = TRUE),
#      sliderInput("height_select",
#                  "Height",
#                  min = min(height),
#                  max = max(height),
#                  value = c(min(height), max(height))),
#      actionButton("reset", "Reset Selection", icon = icon("refresh"))
      
    )
)

body <- dashboardBody(tabItems(
  tabItem("map",
          fluidRow(
            leafletOutput("map")
        ),
  tabItem("charts",
          fluidRow(
            tabPanel("Number by Neighborhood", plotlyOutput("barChart1")),
            tabPanel("Number by Common Name", plotlyOutput("barChart2")))
        ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Statistics", DT::dataTableOutput("table"), width = 12))
            )
        )
  ))

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session = session) {
  loadtrees <- reactive({
    # inputs 
    types_filter <- ifelse(length(input$name_select) > 0, 
                           paste0("%20AND%20%22neighborhood%22%20IN%20(%27", paste(input$name_select, collapse = "%27,%27"),"%27)"),
                           "")
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%1515a93c-73e3-4425-9b35-1cd11b2196da%22%20WHERE%20%22height%22%20=%20%27", input$height_select, "%27")

    
    loadtrees <- ckanSQL(url)
    return(loadtrees)
  })
  output$map <- renderLeaflet({
    trees <- loadtrees()
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) #%>%
 #     addPolygons(data = neighborhoods) %>%
      addCircleMarkers(data = loadtrees(), lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE, fillOpacity = .75)
  })  
  
  output$barChart1 <- renderPlotly({
    dat <- loadtrees()
    ggplotly(
      ggplot(data = dat, aes(x = neighborhood, fill = neighborhood)) + 
        geom_bar() +
        labs(title = "Number of Trees by Neighborhood", x="", y = "") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      , tooltip = "")
  })
  output$barChart2 <- renderPlotly({
    dat <- loadtrees()
    ggplotly(
      ggplot(data = dat, aes(x = common_name, fill = common_name)) + 
        geom_bar() +
        labs(title = "Name of Trees by Neighborhood", x="", y = "") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      , tooltip = "")
  })
  # Datatable
  output$table <- DT::renderDataTable({
    subset(loadtrees(), select = c(neighborhood))
  })
  # Reset Selection of Data
  observeEvent(input$reset, {
    updateSelectInput(session, "name_select", selected = c("Greenfield"))
    showNotification("Loading...", type = "message")
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("trees-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(loadtrees(), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

