

library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(reshape2)
library(data.table)
library(DT)
library(httr)
library(jsonlite)
library(plotly)
library(htmltools)
library(leaflet)
library(rgdal)
library(leaflet.extras)
library(shinydashboard)


## Creation of ckan function for csv data (see Homework 4)
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


## Unique values for Resource Field
ckanUnique <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

##Create choices for Neighborhood and type of fire and wards
neighborhood_choices <- sort(ckanUnique("8d76ac6b-5ae8-4428-82a4-043130d17b02", "neighborhood")$neighborhood)
type_description_choices <- sort(ckanUnique("8d76ac6b-5ae8-4428-82a4-043130d17b02", "type_description")$type_description)
ward_choices <- sort(ckanUnique("8d76ac6b-5ae8-4428-82a4-043130d17b02", "ward")$ward)

##UI for Shiny App
header <- dashboardHeader(title = "Fires in Pittsburgh")
sidebar <- dashboardSidebar(
  
  sidebarMenu(
    id = "tabs",
    menuItem("Map", icon = icon("map"), tabName = "map"),
    menuItem("Charts", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Data Table", icon = icon("table"), tabName = "datatable"),
    dateRangeInput(inputId = "dates", label = "Select Dates", start = "2018-05-03", end = Sys.Date()),
    selectizeInput(inputId = "neighborhood", label = "Pick a neighborhood", selected = "Bloomfield", multiple = TRUE, choices = neighborhood_choices, options = list(maxItems = 4)),
    downloadButton("new.download", label = "Download File"),
    actionButton("click", "Refresh")
  )
)
#Create Body Dashboard
body <- dashboardBody(
  tabItems(
    tabItem("map",
            fluidRow(
              box(title = "Map",leafletOutput("mymap"), width = 12 
              ))),
    tabItem("plot",
            fluidRow(
              box(
                selectizeInput(inputId = "type", label = "Type of Fire", 
                               choices = type_description_choices, multiple = TRUE, selected = "Building fire", options = list(maxItems = 6))),
              box(
                selectizeInput(inputId = "ward", label = "Wards", choices = ward_choices, multiple = TRUE, selected = 8, options = list(maxItems = 6))
              )),
            fluidRow(
              tabBox(title = "Plot",
                     width = 12,
                     tabPanel("Fire Type", plotOutput("plot1")),
                     tabPanel("Ward", plotOutput("plot2"))
              ))),
    tabItem("datatable",
            fluidPage(
              box(title = "Fire Incidents", DT::dataTableOutput("datatable"), width = 12))
    )))

# Define UI for shiny dashboard
ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session = session) {
  
  #Reactive Element for Map AND Charts
  df.filter2 <- reactive ({
    types_filter <- ifelse(length(input$neighborhood) > 0, 
                           paste0("%20AND%20%22neighborhood%22%20IN%20(%27", paste(input$neighborhood, collapse = "%27,%27"),"%27)"),
                           "")
    url4 <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%228d76ac6b-5ae8-4428-82a4-043130d17b02%22%20WHERE%20%22alarm_time%22%20%3E=%20%27", input$dates[1], "T00:00:00%27%20AND%20%22alarm_time%22%20%3C=%20%27",input$dates[2] , "T23:59:59%27", types_filter)
    # use ckan on url
    clean.data <- ckanSQL(url4)
    print(colnames(clean.data))
    return(clean.data)
  })
  #Create Interactive Map: THIS WORKS
  output$mymap <- renderLeaflet({
    trial <- readOGR("2010_Census_Tracts.geojson") %>% na.omit
    fires <- df.filter2()
    #Create map
    leaflet() %>%
      addProviderTiles("OpenStreetMap.France", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Esri.DeLorme", options = providerTileOptions(noWrap = TRUE), group = "Topographical") %>%
      
      #  Layers Control
      addLayersControl(
        baseGroups = c("Default", "Topographical"),
        options = layersControlOptions(collapsed = FALSE)
      )  %>%
      addPolygons(data = trial, fillOpacity = 0, color = "orange", label = ~hood) %>%
      addCircleMarkers(data = fires, lng = ~longitude, lat = ~latitude, radius = 1.5)# %>%
  })
  # Create a Data Table: THIS WORKS
  output$datatable <- DT::renderDataTable({
    subset(df.filter2(), select = c(alarm_time, neighborhood, ward, type_description, longitude))
  })
  
  #Create Chart 1 on Fire Types 
  output$plot1 <- renderPlot({
    df2 <- df.filter2() %>% filter(type_description %in% input$type)
    ggplot(df2, aes(x = type_description, fill = type_description)) +
      geom_bar() +
      ggtitle("Counts of Fire by Type") +
      ylab("Count") +
      xlab("Fire Type") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # #Create Chart 2 on Wards where Fires Occur
  output$plot2 <-  renderPlot({
    df3 <- df.filter2() %>% filter(ward %in% input$ward)
    ggplot(df3, aes(x = ward, fill = ward)) +
      geom_bar() +
      ggtitle("Counts of Fire by Ward") +
      ylab("Count") +
      xlab("Ward") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  #Download Button
  output$new.download <- downloadHandler(
    filename = function(){ 
      paste("new.download", Sys.Date(), ".csv", sep = "" )},
    content = function(file) {
      write.csv(df.filter2(), file, row.names = FALSE)
    })
  #Refresh Button
  observeEvent(input$click, {
    updateSelectizeInput(session, "ward", selected = 8)
    updateSelectizeInput(session, "type", selected = "Building fire")
    updateSelectizeInput(session, "neighborhood", selected = "Bloomfield")
    showNotification("You have successfully reset the filters for type of fire, ward, and neighborhood!", type = "message")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
