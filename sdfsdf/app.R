
library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)
library(shinydashboard)
library(readr)
library(grid)
library(scales)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(shinyjs)
library(httr)
library(jsonlite)
library(plyr)
library(htmltools)

# Load map data
zipcodes <- rgdal::readOGR("http://openac-alcogis.opendata.arcgis.com/datasets/df8e66efc3dd4f2aadae81b55b2b65e7_0.geojson")

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub("NaN|''", 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(field, id) {
  url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

# Unique names
category <- sort(ckanUniques("SaleType", "4af05575-052d-40ff-9311-d578319e810a")$SaleType)
taxes <- sort(ckanUniques("CostsTaxes", "4af05575-052d-40ff-9311-d578319e810a")$CostsTaxes)
services <- sort(ckanUniques("ServiceCompleted", "4af05575-052d-40ff-9311-d578319e810a")$ServiceCompleted)

#pdf(NULL)

header <- dashboardHeader(title = "Pittsburgh Properties",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Mayor Bill Peduto",
                                         message = HTML("We need to increase the tax base!"),
                                         icon = icon("exclamation-circle"))
                          ))

sidebar <- dashboardSidebar(
  # bars on the side
  sidebarMenu(
    id = "tabs",
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("View Map", icon = icon("map"), tabName = "map"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),
    
    # Category Select
    selectInput("categorySelect",
                "Types of Sheriff Sale's:",
                choices = category,
                multiple = TRUE,
                selectize = TRUE,
                selected = c( "Municipal Lien", "Other Real Estate")),
    
    # Date Select
    dateRangeInput("dateSelect",
                   "Sheriff Sale Auction Date:",
                   start = Sys.Date()-30,
                   end = Sys.Date(),
                   format = "mm/dd/yyyy"),
    
    
    # Select Amount Owed
    sliderInput("taxesSelect",
                "Outstanding Taxes Owed:",
                min = min(taxes),
                max = max(taxes),
                value = c(min(taxes), max(taxes)),
                step = 5000),
    
    # Select Defendent knows
    selectizeInput("servicesSelect",
                   "Does the defendant know the property is available for auction?",
                   choices = c("0", "1"),
                   multiple = FALSE,
                   selected = "1"),
    
    # Reset button
    actionButton("reset", "Reset Filters", icon = icon("refresh")),
    actionButton("button", "Click to View Changes")
  ))

body <- dashboardBody(tabItems(
  # names of boxes
  tabItem("plot",
          fluidRow(
            infoBoxOutput("attorney"),
            infoBoxOutput("avgtaxes"),
            infoBoxOutput("zipcode")),
          
          # names of the plot tabs
          fluidRow(
            tabBox(title = "Plot", width = 12,
                   tabPanel("Types of Sheriff Sales", plotlyOutput("plot_types")),
                   tabPanel("Sum of taxes owed by Zip Code", plotlyOutput("plot_taxes"))))),
  # View Map
  tabItem("map",
          fluidRow(
            leafletOutput("map"),
            p())),
  
  # Download Data
  tabItem("table",
          inputPanel(
            downloadButton("downloadData","Download Sheriff Sale Data") # add button to download table as csv
          ),
          fluidPage(
            box(title = "Pittsburgh Sheriff Sales Properties", DT::dataTableOutput("table"), width = 12)))
))

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session=session) {
  
  # Creating filtered sheriff sale data
  propInput <- eventReactive(input$button, {
    # API Stuff
    
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%224af05575-052d-40ff-9311-d578319e810a%22%20WHERE%20%22CostsTaxes%22%20%3E%3D%27", 
                  input$taxesSelect[1],"%27%20AND%20%22CostsTaxes%22%20%3C%3d%27", input$taxesSelect[2], "%27%20AND%20%22SaleType%22%20IN%20%28%27",
                  gsub(" ", "%20", input$categorySelect[1]), "%27%2C%20%27",
                  gsub(" ", "%20", input$categorySelect[2]), "%27%2C%20%27",
                  gsub(" ", "%20", input$categorySelect[3]), "%27%2C%20%27",
                  gsub(" ", "%20", input$categorySelect[4]), "%27%29%20AND%20%22City%22%20%3D%20%27PITTSBURGH%27%20AND%20%22ServiceCompleted%22%20%3d%27", input$servicesSelect[1],  "%27")
    
    # dates do not work
    # "%20AND%20%22SaleDate%22%20%3E%3D%27", 
    #  input$dateSelect[1], "T00:00:00Z%27%20AND%20%22SaleDate%22%20%3C%3D%27",input$dateSelect[2] , "T23:59:59Z%27")
    
    # originally tried using ReadyForSale field, but couldn't get it to work. These are MULTIPLE attempts at trying to make it work
    #"https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%2C%20CASE%20WHEN%20ReadyForSale%20IN%20c%28%27no%27%2C%20%27no.no%27%2C%20%27FALSE%27%29%20THEN%20%27No%27%20else%20ReadyForSale%20FROM"
    
    # https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT *, CASE WHEN ReadyForSale = 'no' then 'WHEN ReadyForSale = 'no.no' THEN 'No' WHEN ReadyForSale =  'FALSE' THEN 'No' No' else ReadyForSale FROM 4af05575-052d-40ff-9311-d578319e810a
    
    print(url)
    
    sale.load <- ckanSQL(url) %>%
      mutate(
        AttorneyName = str_replace_all(AttorneyName, '"', ""),
        SaleDate = as.POSIXct(SaleDate),
        ZIPCode = str_replace_all(ZIPCode, '"', ""),
        ReadyForSale = case_when(
          ReadyForSale %in% c("yes", "yes.no", TRUE) ~ "Yes",
          ReadyForSale %in% c("no", "no.no", FALSE) ~ "No")
      )
    
    #sale.load <- na.omit(sale.load)
    return(sale.load)
  })
  
  # map
  output$map <- renderLeaflet({
    # Plot map
    leaflet() %>%
      
      # Add Basemaps
      addProviderTiles(providers$OpenMapSurfer.Grayscale, options = providerTileOptions(noWrap = TRUE)) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE), group = "Default") %>%
      addProviderTiles("Esri.WorldImagery", options = providerTileOptions(noWrap = TRUE), group = "Imagery") %>%
      
      # Set View
      setView(lat = 40.44, lng = -79.95, zoom = 11.8) %>%
      
      # Add Pittsburgh Zip Codes
      addPolygons(data = zipcodes, color = "#000000", label = ~ZIP, fillOpacity = 0.00) %>%
      
      # Add Layers control
      addLayersControl(
        baseGroups = c("Default", "Imagery"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      # Add Sheriff Sale Points
      addAwesomeMarkers(data = propInput(),
                        lat = ~latitude,
                        lng = ~longitude,
                        label = ~SaleType,
                        clusterOptions = markerClusterOptions())
  })
  
  # Plot 1-  Counts of Properties by Sale Types
  output$plot_types <- renderPlotly({
    property <- propInput()
    ggplotly( ggplot(data = property,
                     aes(x = CostsTaxes, fill = SaleType))  +
                geom_area(stat = "bin", na.rm = T) +
                guides(fill = FALSE) +
                scale_y_continuous(name = "Count of Properties") +
                scale_x_continuous(labels = dollar_format(prefix ="$"), name = "Taxes Owed") +
                theme(axis.text.x = element_text(angle = 15,
                                                 vjust = 1,
                                                 hjust = 1)))
  })
  
  # Plot 2- Plot showing taxes owed by zip code
  output$plot_taxes <- renderPlotly({
    property <- propInput()
    ggplotly( ggplot (data = property,
                      aes (x = ZIPCode,
                           y = round(CostsTaxes, 0), fill = SaleType)) +
                geom_bar(stat = "identity", na.rm = T) +
                guides (fill = FALSE) +
                theme(axis.text.x = element_text(angle = 30,
                                                 hjust = 1),
                      axis.text = element_text(size = rel(0.5))) +
                scale_y_continuous(labels = dollar_format(prefix ="$"), name = "Sum of Taxes Owed") +
                scale_x_discrete (name = "Zip Code"))
  })
  
  # Data table of Assessment
  output$table <- DT::renderDataTable({
    df <- propInput()
    subset(df, select = c(DocketNumber, SaleType, AttorneyName, Plaintiff, Defendant, SaleDate, Address, CostsTaxes))
  },
  options = list(
    autoWidth = TRUE,
    scrollX = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all"))
  ))
  
  # Common Attorney infobox
  output$attorney <- renderInfoBox({
    proper <- propInput()
    name <- names(sort(table(proper$AttorneyName), decreasing = TRUE))
    valueBox(subtitle = "Is the most common attorney", value = name, icon = icon("briefcase"),  color = "green")
  })
  
  # Average Taxes Owed infobox
  output$avgtaxes <- renderValueBox({
    proper <- propInput()
    nums <- round(mean(proper$CostsTaxes, na.rm = T), 0)
    valueBox(subtitle = "Average taxes owed", value = paste0("$", nums), icon = icon("usd"), color = "red")
  })
  
  # Most in a zipcode infobox
  output$zipcode <- renderValueBox({
    proper <- propInput()
    name <- names(sort(table(proper$ZIPCode), decreasing = TRUE))
    valueBox(subtitle = "Is the zipcode with the most sheriff-sales", value = name, icon("home"), color = "blue")
  })
  
  # Make data downloadable and set default download name
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("sheriff-sale-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(propInput(), file)
    })
  
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "categorySelect", selected = "Mortgage Foreclosure")
    # updateDateRangeInput(session, "dateSelect", start = NULL, end = ys.Date()-7)
    updateSliderInput(session, "taxesSelect", value = c(min(taxes), max(taxes)))
    updateSelectizeInput(session, "readySelect", selected = c("no"))
    showNotification("You have successfully reset the filters! Make sure to hit the Submit button again!", type = "message")
  })
}

# Run the application
shinyApp(ui = ui, server = server)