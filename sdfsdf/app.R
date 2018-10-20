


library(leaflet)
library(leaflet.extras)
library(rgdal)
library(httr)
library(readr)
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)


#Kaggle Api
kaggle.api <- "https://www.kaggle.com/api/v1/datasets/download/crawford/boston-public-schools/Public_Schools.csv"
kaggle.auth <- function() {
  source("credentials2.R")
  httr::authenticate(username, key)
}
response <- httr::GET(kaggle.api, kaggle.auth())
Bostonschools<- read_csv(response$content) 
bostonschoolsmap<-readOGR("Public_Schools.geojson")
schooldistrict<-readOGR("Boston_Neighborhoods.shp")

# Define UI for application
ui <- navbarPage("Public Schools in Boston",
                 theme = shinytheme("cosmo"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("nameselect",
                                          "Name of School",
                                          choices = sort(unique(bostonschoolsmap$SCH_NAME)),
                                          selected = c("Adams Elementary"),
                                          selectize = T,
                                          multiple = T)
                            ),
                            mainPanel(
                              # Map Output
                              leafletOutput("leaflet")
                            )
                          )
                 ),
                 #Plots panel
                 tabPanel("Schools Plots",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("cityselect",
                                          "City",
                                          choices = sort(unique(Bostonschools$CITY)),
                                          selected = c(""),
                                          selectize = T,
                                          multiple = T),
                              selectInput("schooltypeselect",
                                          "School Type",
                                          choices = sort(unique(Bostonschools$SCH_TYPE)),
                                          selectize = T,
                                          multiple = T),
                              selectInput("zipcodeselect",
                                          "Zip Code",
                                          choices = sort(unique(Bostonschools$ZIPCODE)),
                                          selected = c("ES"),
                                          selectize = T,
                                          multiple = T)
                            ),
                            fluidRow(
                              column(5, plotlyOutput("Plot1")),
                              column(8, plotlyOutput("Plot2"))
                            )
                          )    
                 ),
                 #Data table panel
                 tabPanel("Table",
                          fluidPage(
                            inputPanel(
                              downloadButton("downloaddata","Download Data")
                            ),
                            wellPanel(DT::dataTableOutput("table"))
                          )
                 )
)

# Define server logic 
server <- function(input, output) {
  
  #Defining a reactive function for the map
  bostonmapsInputs <- reactive({
    if (length(input$nameselect) > 0) {
      bostonschoolsmap <- subset(bostonschoolsmap, SCH_NAME %in% input$nameselect)
    }
    return(bostonschoolsmap)
  })
  #Defining a reactive function for the plots and table
  bostonInputs <- reactive({
    if (length(input$cityselect) > 0) {
      Bostonschools <- subset(Bostonschools, CITY %in% input$cityselect)
    }
    if (length(input$schooltypeselect) > 0) {
      Bostonschools <- subset(Bostonschools, SCH_TYPE %in% input$schooltypeselect)
    }
    if (length(input$zipcodeselect) > 0) {
      Bostonschools <- subset(Bostonschools, ZIPCODE %in% input$zipcodeselect)
    }
    
    return(Bostonschools)
  })
  #Leaflet output
  output$leaflet <- renderLeaflet({
    bostonschoolsmap <- bostonmapsInputs()
    # Build Map
    leaflet()%>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      addPolygons(data=schooldistrict,
                  color="red")%>%
      addMarkers(data=bostonschoolsmap, popup = ~paste0(SCH_NAME))
  }) 
  
  #I am going to create the plots
  #The first pplot is going to show the number of schools per type of school
  output$Plot1 <- renderPlotly({
    Bostonschools<- bostonInputs()
    ggplot(data =  Bostonschools, aes(x =SCH_TYPE)) + 
      geom_histogram(stat="count") + 
      labs(title= "Number of schools per type",
           x= "Number of schools", y= "school type")
  })
  # The second plot is going to show the number of schools per principle's name
  output$Plot2 <- renderPlotly({
    Bostonschools<- bostonInputs()
    ggplot(data =  Bostonschools, aes(x =PL)) + 
      geom_histogram(stat="count") + 
      labs(title= "Number of schools per Principal´s name",
           x= "Number of schools", y= "Principal's name")
  })
  #Table and download button
  output$table <- DT::renderDataTable({
    subset(bostonInputs(), select = c("SCH_TYPE","SCH_NAME","ZIPCODE","ADDRESS","CITY"))
  })
  output$downloaddata<-downloadHandler(
    filename = function(){
      paste("Bostonschools",Sys.Date(),".csv",sep="")
    },
    content=function(file){
      write.csv(bostonInputs(),file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
