library(shiny)
library(tidyverse)
library(htmltools)
library(httr)
library(xml2)

# API call setup and definitions

# api key
key <- readChar("api_key.txt", file.info("api_key.txt")$size)

# base url for the api calls
base_url <- "http://realtime.portauthority.org/bustime/api/v3"

# function to query bus data from api
# following from: https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
getBusData <- function(value, type) {
    # add key to request
    url <- paste0(base_url, "/getvehicles?key=", key)
    
    # add either route or vehicle id to request
    if (type == "route") {
        url <- paste0(url, "&rt=", value)
    } else if (type == "vid") {
        url <- paste0(url, "&vid=", value)
    } else {
        stop("Error, request type not recognized.")
    }
    request <- RETRY("GET", URLencode(url), repeated=TRUE)
    content <- content(request, "text")
    
    # parse xml
    xml <- as_list(read_xml(content))
    
    bustime_df <- as_tibble(xml) %>%
        unnest_wider(`bustime-response`) %>%
        unnest(cols = names(.)) %>%
        readr::type_convert() 
}

# get the route information when the app launches
getRouteData <- function() {
    # add key to request
    url <- paste0(base_url, "/getroutes?key=", key)
    
    request <- RETRY("GET", URLencode(url), repeated=TRUE)
    content <- content(request, "text")
    
    # parse xml
    xml <- as_list(read_xml(content))
    
    bustime_df <- as_tibble(xml) %>%
        unnest_wider(`bustime-response`) %>%
        unnest(cols = names(.)) %>%
        readr::type_convert() %>%
        # remove non-bus info
        filter(rtpidatafeed == "Port Authority Bus")
}
route.data <- getRouteData()

# define custom icons for the map
icons <- awesomeIconList(
    bus.simple = makeAwesomeIcon(icon="bus-simple", library="fa")
)

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    "Pittsburgh Bus Tracker",
    
    # bus map
    tabPanel("Live Map",
             # input
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId="route.select",
                                 label="Route Number",
                                 choices=unique(route.data$rtdd),
                                 selected=unique(route.data$rtdd)[1])
                 ),
             # map
                 mainPanel(
                     shinyjs::useShinyjs(),
                     tags$style(type = "text/css", 
                                ".leaflet {height: calc(100vh - 90px) !important;}
                                body {background-color: #D4EFDF;}"),
                     # Map Output
                     leafletOutput("leaflet")
                    )
                 )
             ),
    
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
        req(input$route.select)
        getBusData(value=input$route.select, type="route")
    })

    # raw data table for display
    output$table <- DT::renderDataTable(bus.data())
    
    # leaflet map base
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.HOT") %>%
            setView(-79.9959, 40.4406, 10)
    })
    
    # bus coordinate view
    # only update when bus data updates
    observe({
        # if no buses, clear the markers and then exit
        if (!("lat" %in% colnames(bus.data()))) {
            leafletProxy("leaflet") %>%
                clearGroup(group="busPosition")
        } else {
            # convert lat and lon for plotting
            data <- bus.data() %>%
                mutate(lat = as.numeric(lat),
                       lon = as.numeric(lon))
            
            
            
            leafletProxy("leaflet", data=data) %>%
                clearGroup(group="busPosition") %>%
                addAwesomeMarkers(lng=~lon,
                                  lat=~lat,
                                  icon=makeAwesomeIcon(icon="bus", library="fa"),
                                  popup=~vid,
                                  group="busPosition",
                                  layerId=~vid)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
