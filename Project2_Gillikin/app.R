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
neighborhoods <- rgdal::readOGR("http://pghgis-pittsburghpa.opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson")
palet <- colorFactor(topo.colors(5), treeTops$common_names)

#ckanSQL <- function(url) {
#  # Make the Request
#  r <- RETRY("GET", URLencode(url))
#  # Extract Content
#  c <- content(r, "text")
#  # Basic gsub to make NA's consistent with R
#  json <- gsub('NaN', 'NA', c, perl = TRUE)
#  # Create Dataframe
#  data.frame(jsonlite::fromJSON(json)$result$records)
#}

# Unique values for Resource Field
#ckanUniques <- function(id, field) {
#  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
#  c(ckanSQL(URLencode(url)))
#}

#neighborhood <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "neighborhood")$neighborhood)
#condition <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "condition")$condition)
#height <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "height")$height)

header <- dashboardHeader(title = "Pittsburgh's Trees")

sidebar <- dashboardSidebar(
  # bars on the side
  sidebarMenu(
    id = "tabs",
    menuItem("Map", icon = icon("map"), tabName = "maps"),
    menuItem("Charts", icon = icon("bar-chart"), tabName = "charts"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    selectInput("neighborhood_select",
                "Neighborhood",
                choices = sort(unique(treeTops$neighborhood)),
                multiple = TRUE, 
                selectize = TRUE,
                selected = c("Greenfield")),
    selectInput("condition_select",
                  "Tree Condition",
                choices = sort(unique(treeTops$condition)),
                multiple = TRUE, 
                selectize = TRUE,
                selected = c("Fair")),
    selectInput("name_select",
                "Common Name",
                choices = sort(unique(treeTops$common_name)),
                selected = "Maple: Red",
                multiple = TRUE,
                selectize = TRUE),
    sliderInput("height_select",
                "Height",
                min = 0,
                max = max(na.omit(treeTops$height)),
                value = c(0, max(na.omit(treeTops$height)))),
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

# Define server logic
server <- function(input, output, session = session) {
 # loadtrees <- reactive({
#    if (length(input$name_select) > 0) {
#      treeTops <- subset(treeTops, treeTops$neighborhood %in% input$name_select)
#    }
    # inputs 
    #types_filter <- ifelse(length(input$name_select) > 0, 
    #                       paste0("%20AND%20%22neighborhood%22%20IN%20(%27", paste(input$name_select, collapse = "%27,%27"),"%27)"),
    #                       "")
    # Build API Query with proper encodes
  #  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%1515a93c-73e3-4425-9b35-1cd11b2196da%22%20WHERE%20%22height%22%20=%20%27", input$height_select, "%27")

    
 #   loadtrees <- ckanSQL(url)
 #   return(loadtrees)
 # })
  output$map1 <- renderLeaflet({
 #   trees <- loadtrees()
    leaflet() %>%
      setView(lng = -79.995888, lat = 40.440624, 12.25) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = neighborhoods, color = "gray", weight = 1, 
                  highlight = highlightOptions(
                    weight = 4, 
                    color = "#545455",
                    fill = "violet",
                    bringToFront = TRUE)) %>%
      addCircleMarkers(data = treeTops, lng = ~longitude, lat = ~latitude, radius = 2, stroke = FALSE, fillOpacity = .75, color = ~palet(common_name), label = ~common_name)
  })  
  
  output$barChart1 <- renderPlotly({
#    dat <- loadtrees()
    ggplotly(
      ggplot(data = treeTops, aes(x = neighborhood, fill = neighborhood)) + 
        geom_bar() +
        labs(x = "", y = "") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      )
  })
  output$barChart2 <- renderPlotly({
 #   dat <- loadtrees()
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
    updateSelectInput(session, "condition_select", selected = "Fair")
    updateSelectInput(session, "name_select", selected = "Maple: Red")
    updateSliderInput(session, "height_select", value = c(0, max(na.omit(treeTops$height))))
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

