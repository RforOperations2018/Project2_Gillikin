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

trees <- sort(ckanUniques("1515a93c-73e3-4425-9b35-1cd11b2196da", "common_name")$common_name)
neighborhood <- sort(ckanUnique("8d76ac6b-5ae8-4428-82a4-043130d17b02", "neighborhood")$neighborhood)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("The Trees of Pittsburgh"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      selectInput("name_select",
                  "Common Name",
                  choices = common_name,
                  selected = "Maple: Red",
                  multiple = TRUE),
      selectInput("condition_select",
                  "Condition",
                  choices = condition_select,
                  selected = "Fair",
                  multiple = TRUE),
      selectInput("neighborhood_select",
                  "Condition",
                  choices = neighborhood_select,
                  selected = "Greenfield",
                  multiple = TRUE),
      sliderInput("height_select",
                  "Height",
                  choices = height_select,
                  selected = "Fair",
                  multiple = TRUE),
      actionButton("click", "Refresh")
      
    ),
    
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("map")),
        tabPanel("Neighborhood Number",
                 plotlyOutput("barChart1")),
        tabPanel("Common_name number",
                 plotlyOutput("barChart2")),
        tabPanel("Table",
                 inputPanel(
                   downloadButton("downloadData","Download Tree Data")
                 ),
                 fluidPage(DT::dataTableOutput("table"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  loadtrees <- reactive({
    # inputs 
    types_filter <- ifelse(length(input$neighborhood) > 0, 
                           paste0("%20AND%20%22neighborhood%22%20IN%20(%27", paste(input$neighborhood, collapse = "%27,%27"),"%27)"),
                           "")
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$dates[2], "%27%20AND%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
    
    # Load and clean data
    loadtrees <- ckanSQL(url)
    return(loadtrees)
    
    trees <- loadtrees()
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = neighborhood, color = "gray", weight = 2, 
                  highlight = highlightOptions(
                    weight = 6, 
                    color = "#b9aa7e", 
                    bringToFront = TRUE)) %>%
      addCircleMarkers(data = trees, lng = ~longitude, lat = ~latitude, radius = ~ifelse(type == "single", 4, 8), stroke = FALSE, fillOpacity = .75)
  })  
  
  output$barPlot <- renderPlotly({
    dat <- cvl.house()
    ggplotly(
      ggplot(data = dat, aes(x = neighborhood, y = subsidizedUnits)) + 
        geom_col(fill = "#8E5572") +
        labs(title="Six neighborhoods account for 83% of subsidized units", x = "", y="Number of Units") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      , tooltip = "subsidizedUnits")
  })
  output$lineChart <- renderPlotly({
    dat311 <- load311()
    
    # shape the data for chart
    table <- dat311 %>%
      group_by(STATUS) %>%
      summarise(count = n())
    
    # draw plot
    ggplot(table, aes(x = STATUS, y = count, fill = STATUS)) +
      geom_bar(stat = "identity")
  })
  # Datatable
  output$table <- DT::renderDataTable({
    subset(loadAccount(), select = c(department_name, general_ledger_date, object_account_description, amount))
  })
  # Reset Selection of Data
  observeEvent(input$reset, {
    updateSelectInput(session, "department_select", selected = c("DPW-Operations"))
    showNotification("Loading...", type = "message")
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("revenue.expenses-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(loadAccount(), file)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

