library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(htmltools)
library(httr)
library(xml2)
library(jsonlite)
library(leaflet)
library(plotly)


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
        if ("msg" %in% names(.)) filter(., msg != "No data found for parameter") else .
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
getPatternData <- function(route) {
    # add key to request
    url <- paste0(base_url, "/getpatterns?key=", key)
    
    # request patterns for the route
    url <- paste0(url, "&rt=", route)
    url <- paste0(url, "&rtpidatafeed=Port%20Authority%20Bus&format=json")
    
    
    request <- RETRY("GET", URLencode(url), repeated=TRUE)
    content <- content(request, "text")
    results <- fromJSON(content)
    
    return(results$`bustime-response`$ptr$pt[[1]])
}

# get bus predictions
getPredictionData <- function(vid) {
    # add key to request
    url <- paste0(base_url, "/getpredictions?key=", key)
    
    # request patterns for the route
    url <- paste0(url, "&vid=", vid)
    url <- paste0(url, "&rtpidatafeed=Port%20Authority%20Bus&format=json")
    
    
    request <- RETRY("GET", URLencode(url), repeated=TRUE)
    content <- content(request, "text")
    results <- fromJSON(content)
    
    return(results$`bustime-response`$prd)
}

# Define UI for application that draws a histogram
ui <- navbarPage(
    # Application title
    "Pittsburgh Bus Tracker",
    
    # bring in some nice shiny dashboard elements like box
    # https://stackoverflow.com/a/59527927
    header = tagList(
        useShinydashboard()
    ),
    
    
    # bus map
    tabPanel("Live Map",
             # input
             sidebarLayout(
                 sidebarPanel(id="sidebar",
                     selectizeInput(inputId="route.select",
                                    label="Route Number",
                                    choices=unique(route.data$rtdd),
                                    selected=unique(route.data$rtdd)[1],
                                    multiple=TRUE,
                                    options=list(maxItems=9))
                 ),
             # map
                 mainPanel(id="mainPanel",
                     fluidRow(
                         width=8,
                         shinyjs::useShinyjs(),
                         tags$style(type = "text/css", 
                                    ".leaflet {height: calc(50vh - 90px) !important;}
                            body {background-color: #8899A6;}"),
                         # Map Output
                         leafletOutput("leaflet", height=500)
                        ),
                     fluidRow(
                         tabsetPanel(type="pills",
                             tabPanel("Bus Information",
                                      box(width=12,
                                          background="navy",
                                          fluidRow(infoBoxOutput("bus.status.box")),
                                          fluidRow(
                                              column(4,
                                                     uiOutput("display.bus.click")),
                                              column(8,
                                                     plotlyOutput("prediction.plot"))
                                              )
                                          )
                                     )
                            )
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
    output$table <- DT::renderDataTable(bus.pred())
    
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
                       lon = as.numeric(lon),
                       status = ifelse(dly == "false",
                                       "On-Time",
                                       "Delayed")) %>%
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
                removeControl(layerId="legend") %>%
                clearGroup(group="Buses") %>%
                addAwesomeMarkers(lng=~lon,
                                  lat=~lat,
                                  icon=icons,
                                  popup=~paste0(
                                      "<h3>Route: ", rt, "</h3>",
                                      "<h4>Bus ID: ", vid, "<br>",
                                      "Destination: ", des, "<br>",
                                      "Status: ", status, "<br>",
                                      "Speed: ", spd, "</h4>"
                                  ),
                                  group="Buses",
                                  layerId=~vid) %>%
                addLegend("bottomright", colors=color.df$color, labels=color.df$rt,
                          title="Route", layerId="legend", opacity=2)
        }
    })
    
    # show bus info when selected
    bus.click <- reactiveVal(NULL)
    bus.pred <- reactiveVal(NULL)
    
    # observe clicks
    # reference: http://rstudio.github.io/leaflet/shiny.html#inputsevents
    observeEvent(input$leaflet_marker_click, {
        user.click <- input$leaflet_marker_click
        if (is.null(user.click)) {
            return()
        }
        
        bus.click(user.click)
        bus.pred(getPredictionData(user.click$id))
    })
    
    # format the bus display graphic
    output$display.bus.click <- renderUI({
        if (is.null(bus.click())) {
            return(tagList(
                h2(paste0("Route: ")),
                h3(paste0("Bus ID: ")),
                h3(paste0("Destination: ")),
                h3(paste0("Status: ")),
                h3(paste0("Current speed: "))
            ))
        }
        display.data <- bus.data() %>%
            filter(vid == bus.click()$id) %>%
            mutate(status = ifelse(dly == "false",
                                   "On-Time",
                                   "Delayed"))
        
        tagList(
            h2(paste0("Route: ", display.data$rt)),
            h3(paste0("Bus ID: ", display.data$vid)),
            h3(paste0("Destination: ", display.data$des)),
            h3(paste0("Status: ", display.data$status)),
            h3(paste0("Current speed: ", display.data$spd, " mph"))
        )
    })
    
    # makes a prediction plot for bus arrivals
    output$prediction.plot <- renderPlotly({
        if (is.null(bus.click()) | is.null(bus.pred())) {
            return()
        }
        
        plot <- bus.pred() %>%
            mutate(`Stop Name` = as.factor(stpnm),
                   prdtm = parse_date_time(prdtm, "%Y%m%d %h:%M"),
                   `Arrival Time` = sprintf("%02d:%02d", hour(prdtm), minute(prdtm))) %>%
            head(5) %>%
            arrange(desc(prdtm)) %>%
            mutate(`Stop Name` = factor(`Stop Name`, unique(`Stop Name`))) %>%
            ggplot(aes(y=`Stop Name`,
                       x=vid)) +
            geom_point(aes(color=`Arrival Time`), size=2, shape="square") +
            geom_text(aes(label=`Arrival Time`), nudge_x=0.25, size=3, hjust=0) +
            #scale_y_continuous(trans = rev_date) +
            theme_minimal() +
            theme(panel.grid.major.x = element_line(linetype="dashed", color="black"),
                  axis.ticks.x=element_blank(),
                  axis.text.x=element_blank(),
                  legend.position="none") +
            ylab("") +
            xlab("") +
            ggtitle("Predicted Arrival Times")
        
        ggplotly(plot, tooltip=c("Arrival Time", "Stop Name"))
    })
    
    # makes an info box indicating if the bus is delayed or on-time
    output$bus.status.box <- renderInfoBox({
        if (is.null(bus.click())) {
            return(infoBox("Bus Status",
                           value="Not Selected",
                           icon=icon("minus"),
                           color="black"))
        }

        display.data <- bus.data() %>%
            filter(vid == bus.click()$id) %>%
            mutate(status = ifelse(dly == "false",
                                   "On-Time",
                                   "Delayed"))
        
        if (display.data$status=="On-Time") {
            icon.type <- "check"
            color.type <- "green"
        } else {
            icon.type <- "exclamation"
            color.type <- "red"
        }
        
        infoBox(title="Bus Status",
                value=paste0(display.data$status),
                icon=icon(icon.type, lib="font-awesome"),
                color=color.type,
                fill=TRUE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
