library(shiny)
library(tidyverse)
library(htmltools)
library(httr)
library(xml2)

# API call setup and definitions

# api key
key <- readChar("api_key.txt", file.info("api_key.txt")$size)

# base url for the api calls
base_url <- "http://realtime.portauthority.org/bustime/api/v3/getvehicles"

# function to query bus data from api
# following from: https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
getBusData <- function(route) {
    url <- paste0(base_url, "?key=", key, "&rt=", route)
    request <- RETRY("GET", URLencode(url), repeated=TRUE)
    content <- content(request, "text")
    
    # parse xml
    xml <- as_list(read_xml(content))
    
    bustime_df <- as_tibble(xml) %>%
        unnest_wider(`bustime-response`) %>%
        unnest(cols = names(.)) %>%
        readr::type_convert() 
}

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    titlePanel("Pittsburgh Bus Tracker"),
    
    # top bar menu
    tabPanel("Data Table",
             fluidPage(
                 wellPanel(DT::dataTableOutput("table"))
                 )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # updating vehicle location data
    bus.data <- reactive({
        getBusData(2)
    })

    # raw data table for display
    output$table <- DT::renderDataTable(bus.data())
}

# Run the application 
shinyApp(ui = ui, server = server)
