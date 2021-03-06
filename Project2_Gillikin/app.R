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

# Load PGH neighborhood shapefiles
neighborhoods <- rgdal::readOGR("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")
# Set pallette for map
# This has to be AFTER you've created the common_names type this is breaking your app... It's also named wrong
palet <- colorFactor(topo.colors(5), common_names)

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
common_names <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "common_name")$common_name)
condition <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "condition")$condition)
height <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "height")$height)

#Dashboard sections
header <- dashboardHeader(title = "Pittsburgh's Trees")

sidebar <- dashboardSidebar(

  sidebarMenu(
    id = "tabs",
    menuItem("Map", icon = icon("map"), tabName = "maps"),
    menuItem("Charts", icon = icon("bar-chart"), tabName = "charts"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    selectInput("neighborhood_select",
                "Neighborhood",
                choices = sort(unique(neighborhood)),
                multiple = TRUE, 
                selectize = TRUE,
                selected = c("Greenfield")),
    selectInput("name_select",
                "Common Name",
                choices = sort(unique(common_name)),
                selected = c("Maple: Red"),
                multiple = TRUE,
                selectize = TRUE),
    selectInput("condition_select",
                "Tree Condition",
                choices = sort(unique(condition)),
                multiple = TRUE, 
                selectize = TRUE,
                selected = c("Fair")),
    sliderInput("height_select",
                "Height",
                min = 0,
                max = max(na.omit(height)),
                value = c(0, max(na.omit(height)))),
    hr(),
    actionButton("reset", "Reset Selection", icon = icon("refresh"))
  )
)

body <- dashboardBody(tabItems(
  tabItem("maps",
          fluidPage(
            leafletOutput("map1")
          )
  ),
    tabItem("charts",
          fluidPage(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Number by Neighborhood", plotlyOutput("barChart1")),
                   tabPanel("Number by Common Name", plotlyOutput("barChart2")))
          )
  ),
  tabItem("table",
          fluidPage(
            inputPanel(
              downloadButton("downloaddata","Download Data")
            ),
            box("Selected Statistics", DT::dataTableOutput("table"), width = 12))
        )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Note before going into the reactive part of the server: When I run it with the .csv of the data, everything runs. 
# When I try to make any single part reactive, nothing works. Obviously, when I try to make everything reactive and pulled from the API, nothing works. 

# Define server logic
server <- function(input, output, session = session) {
  treeTops <- reactive({
   # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%1515a93c-73e3-4425-9b35-1cd11b2196da%22%20WHERE%20%22height%22%20%3E=%20%27", input$height_select[1], "%27%20AND%20%22height%22%20%3C=%20%27", input$height_select[2], "%27%20AND%20%22neighborhood%22%20=%20%27", input$neighborhood_select, "%27%20AND%20%22common_name%22%20=%20%27", input$name_select, "%27%20AND%20%22condition%22%20=%20%27", input$condition_select, "%27") 

    treeTops <- ckanSQL(url)
    return(treeTops)
  })
  # Map of trees by neighborhood, colored by name
  output$map1 <- renderLeaflet({
    dat <- treeTops()
    leaflet() %>%
      # Once reactive, no need for setView()
      setView(lng = -79.995888, lat = 40.440624, 12.25) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = neighborhoods, color = "gray", weight = 1, 
                  highlight = highlightOptions(
                    weight = 4, 
                    color = "#545455",
                    bringToFront = TRUE)) %>%
      addCircleMarkers(data = dat, lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE, fillOpacity = .75, color = ~palet(common_name), label = ~common_name)
  })  
  # Chart of number of trees in each neighborhood
  output$barChart1 <- renderPlotly({
    dat <- treeTops()
    ggplotly(
      ggplot(data = treeTops, aes(x = neighborhood, fill = neighborhood)) + 
        geom_bar() +
        labs(x = "", y = "") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      )
  })
  # Chart of number of each tree name
  output$barChart2 <- renderPlotly({
    dat <- treeTops()
    ggplotly(
      ggplot(data = treeTops, aes(x = common_name, fill = common_name)) + 
        geom_bar() +
        labs(x = "", y = "") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      )
  })
  # Reset Selection of Data
  observeEvent(input$reset, {
    updateSelectInput(session, "neighborhood_select", selected = "Greenfield")
    updateSelectInput(session, "name_select", selected = "Maple: Red")
    updateSelectInput(session, "condition_select", selected = "Fair")
    updateSliderInput(session, "height_select", value = c(0, max(na.omit(height))))
    showNotification("Loading...", type = "message")
  })
  # Datatable
  output$table <- DT::renderDataTable({
    subset(treeTops, select = c("neighborhood", "common_name", "condition"))
  })
#   Download data in the datatable
  output$downloaddata<-downloadHandler(
    filename = function(){
      paste("PGHTrees",Sys.Date(),".csv",sep="")
  },
    content=function(file){
      write.csv(treeTops,file)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
