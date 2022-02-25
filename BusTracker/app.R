library(shiny)
library(tidyverse)
library(htmltools)
library(httr)
library(xml2)
library(jsonlite)
library(leaflet)

# API call setup and definitions

# api key
key <- readChar("api_key.txt", file.info("api_key.txt")$size)

# base url for the api calls
base_url <- "http://realtime.portauthority.org/bustime/api/v3"

# function to query bus data from api
# following from: https://urbandatapalette.com/post/2021-03-xml-dataframe-r/
getBusData <- function(value, type) {
    # don't send request if more than 10 values because of api limit
    if (length(value) > 10) {
        stop("Error, too many values to search.")
    }
    
    # add key to request
    url <- paste0(base_url, "/getvehicles?key=", key)
    
    # add either route or vehicle id to request
    if (type == "route") {
        url <- paste0(url, "&rt=", paste0(value, collapse=","))
    } else if (type == "vid") {
        url <- paste0(url, "&vid=", paste0(value, collapse=","))
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
        readr::type_convert() %>%
        if ("vid" %in% names(.)) filter(., !is.null(vid)) else .
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

# color info
color.list <- c('red', 'darkred', 'orange', 'green', 'darkgreen', 'blue', 
                'purple', 'darkpurple', 'cadetblue')

# get bus stop info (from static file to avoid excessive API calls)
#bus.stop.df <- st_read("data/bus_stops.shp")

# get route patterns
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

# Define UI for application that draws a histogram
ui <- navbarPage(

    # Application title
    "Pittsburgh Bus Tracker",
    
    # bus map
    tabPanel("Live Map",
             # input
             sidebarLayout(
                 sidebarPanel(
                     selectizeInput(inputId="route.select",
                                    label="Route Number",
                                    choices=unique(route.data$rtdd),
                                    selected=unique(route.data$rtdd)[1],
                                    multiple=TRUE,
                                    options=list(maxItems=9))
                 ),
             # map
                 mainPanel(
                     fluidRow(
                         width=8,
                         shinyjs::useShinyjs(),
                         tags$style(type = "text/css", 
                                    ".leaflet {height: calc(50vh - 90px) !important;}
                            body {background-color: #D4EFDF;}"),
                         # Map Output
                         leafletOutput("leaflet", height=500)
                        )
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
    
    # get pattern data for routes
    pattern.data <- reactive({
        req(input$route.select)
        route <- input$route.select[1]
        pattern <- getPatternData(route)
        pattern$rt <- route
        
        leftover.routes <- input$route.select[-1]
        
        for (route in leftover.routes) {
            next.pattern <- getPatternData(route)
            next.pattern$rt <- route

            pattern <- rbind(pattern, next.pattern)
        }
        
        output$table <- DT::renderDataTable(pattern)
        
        
        return(pattern)
    })
    
    # leaflet map base + bus stops
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.HOT") %>%
            setView(-79.9959, 40.4406, 10) %>%
            addLayersControl(overlayGroups=c("Stops", "Buses", "Routes"))
    })
    
    # update route patterns
    observe({
        # get all routes in data for colors
        route.table <- unique(pattern.data()$rt)
        color.df <- tibble(rt=route.table, color=color.list[1:length(rt)])

        # convert lat and lon for plotting
        plot.data <- pattern.data() %>%
            inner_join(color.df, by="rt")

        # clear old routes
        leafletProxy("leaflet") %>%
            clearGroup(group="Routes") 

        # add new routes
        for (route in route.table) {
            data.i <- filter(plot.data, rt == route)
            leafletProxy("leaflet") %>%
                addPolylines(data=data.i,
                             group="Routes",
                             color=~color,
                             lat=~lat,
                             lng=~lon)
        }
    })
    
    # bus coordinate view
    # only update when bus data updates
    observe({
        # if no buses, clear the markers and then exit
        if (!("lat" %in% colnames(bus.data()))) {
            leafletProxy("leaflet") %>%
                clearGroup(group="busPosition")
        } else {
            # get all routes in data for colors
            route.table <- unique(bus.data()$rt)
            color.df <- tibble(rt=route.table, color=color.list[1:length(rt)])
            
            # convert lat and lon for plotting
            plot.data <- bus.data() %>%
                mutate(lat = as.numeric(lat),
                       lon = as.numeric(lon)) %>%
                inner_join(color.df, by="rt")
            
            
            # define custom icons for the map
            icons <- awesomeIcons(
                icon="bus", 
                iconColor="black",
                library="fa",
                markerColor=plot.data$color
            )
            
            # clear old markers and add new ones
            leafletProxy("leaflet", data=plot.data) %>%
                clearGroup(group="Buses") %>%
                addAwesomeMarkers(lng=~lon,
                                  lat=~lat,
                                  icon=icons,
                                  popup=~vid,
                                  group="Buses",
                                  layerId=~vid)
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
