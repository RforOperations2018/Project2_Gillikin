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

violations <- sort(ckanUniques("1a1329e2-418c-4bd3-af2c-cc334e7559af", "REQUEST_TYPE")$REQUEST_TYPE)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("The Trees of Pittsburgh"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date",
                     "Inspection Date Range",
                     start = Sys.Date()-30,
                     end = Sys.Date()),

      checkboxGroupInput("type", "Violation Type", 
                         choices = list(" Low risk violation" = 1, " Medium risk violation" = 2, " High risk violation" = 3),
                         selected = 1)
      
      common_name
      height
      condition
      neighborhood
      
    ),
    
    # Tabset Main Panel
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("map")),
        tabPanel("Neighborhood Number",
                 plotlyOutput("barChart")),
        tabPanel("Common_name number",
                 plotlyOutput("barChart")),
        tabPanel("Table",
                 inputPanel(
                   downloadButton("downloadData","Download Revenue/Expense Data")
                 ),
                 fluidPage(DT::dataTableOutput("table"))
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session = session) {
  load311 <- reactive({
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%2276fda9d0-69be-4dd5-8108-0de7907fc5a4%22%20WHERE%20%22CREATED_ON%22%20%3E=%20%27", input$dates[1], "%27%20AND%20%22CREATED_ON%22%20%3C=%20%27", input$dates[2], "%27%20AND%20%22REQUEST_TYPE%22%20=%20%27", input$type_select, "%27")
    
    # Load and clean data
    dat311 <- ckanSQL(url) %>%
      mutate(date = as.Date(CREATED_ON),
             STATUS = ifelse(STATUS == 1, "Closed", "Open"))
    
    return(dat311)
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
      addCircleMarkers(data = housing.units, lng = ~long, lat = ~lat, radius = ~ifelse(type == "single", 4, 8), color = ~palet(type), stroke = FALSE, fillOpacity = .75, label = ~address)
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

