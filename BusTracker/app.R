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
library(lubridate)
library(sf)
library(sfheaders)
library(data.table)


# API call setup and definitions ----

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
        url <- paste0(url, "&rt=", paste0(value, collapse = ","))
    } else if (type == "vid") {
        url <- paste0(url, "&vid=", paste0(value, collapse = ","))
    } else {
        stop("Error, request type not recognized.")
    }
    request <- RETRY("GET", URLencode(url), repeated = TRUE)
    content <- content(request, "text")
    
    # parse xml
    xml <- as_list(read_xml(content))
    
    bustime_df <- as_tibble(xml) %>%
        unnest_wider(`bustime-response`) %>%
        unnest(cols = names(.)) %>%
        readr::type_convert() %>%
        if ("msg" %in% names(.))
            filter(., msg != "No data found for parameter")
    else
        .
}

# get the route information when the app launches
getRouteData <- function() {
    # add key to request
    url <- paste0(base_url, "/getroutes?key=", key)
    
    request <- RETRY("GET", URLencode(url), repeated = TRUE)
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
color.list <-
    c(
        'red',
        'darkred',
        'orange',
        'green',
        'darkgreen',
        'blue',
        'purple',
        'cadetblue',
        'pink',
        'black'
    )

# get bus stop info (from static file to avoid excessive API calls)
bus.stop.df <- st_read("data/bus_stops.shp")
bus.stop.df <- bus.stop.df %>%
    filter(Mode == "Bus") %>%
    arrange(CleverID) %>%
    mutate(has.shelter = ifelse(Shelter == "No Shelter",
                                "No", "Yes"),
           human.readable = paste0("ID: ", CleverID, " | ", Stop_name))

stop.values <- bus.stop.df$CleverID
stop.names <- bus.stop.df$human.readable
stop.list <- setNames(stop.values, stop.names)

# get route patterns
getPatternData <- function(route) {
    # add key to request
    url <- paste0(base_url, "/getpatterns?key=", key)
    
    # request patterns for the route
    url <- paste0(url, "&rt=", route)
    url <-
        paste0(url, "&rtpidatafeed=Port%20Authority%20Bus&format=json")
    
    
    request <- RETRY("GET", URLencode(url), repeated = TRUE)
    content <- content(request, "text")
    results <- fromJSON(content)
    
    return(results$`bustime-response`$ptr$pt[[1]])
}

# get bus predictions
getPredictionData <- function(value, type) {
    # add key to request
    url <- paste0(base_url, "/getpredictions?key=", key)
    
    # vid or stop prediction
    if (type == "vid") {
        url <- paste0(url, "&vid=")
    } else if (type == "stpid") {
        url <- paste0(url, "&stpid=")
    } else {
        return()
    }
    
    # request patterns for the route
    url <- paste0(url, value)
    url <-
        paste0(url, "&rtpidatafeed=Port%20Authority%20Bus&format=json")
    
    
    request <- RETRY("GET", URLencode(url), repeated = TRUE)
    content <- content(request, "text")
    results <- fromJSON(content)
    
    return(results$`bustime-response`$prd)
}

# make update interval list
interval.values <- c(30, 60, 300, 600)
interval.names <- c("30 seconds", "1 minute", "5 minutes", "10 minutes")
interval.list <- setNames(interval.values, interval.names)

# Define UI for application ----
ui <- navbarPage(
    # Application title
    "Pittsburgh Bus Tracker",
    
    # bring in some nice shiny dashboard elements like box
    # https://stackoverflow.com/a/59527927
    header = tagList(useShinydashboard()),
    
    # bus map tab
    tabPanel("Live Map",
             fluidRow(
                 id = "mainRow",
                 column(
                     id = "sidebar",
                     width = 4,
                     
                     # info block
                     box(id="info",
                         width=12,
                         title="Welcome to Pittsburgh Bus Tracker!",
                         p("Stay informed about your favorite bus lines."),
                         p("Enter up to 10 bus lines to track all currently operating
                           buses on each line. The position of each bus and their routes
                           will be updated on the map. Click on the bus for more information!
                           A pop-up will display specifics about the bus and the area under the map
                           will populate with the bus's next stops and expected arrival times"),
                         p("Enter a bus stop ID to get more information about that stop. It will
                           appear on the map and populate a schedule of the next arriving buses 
                           (see the 'Stop Information' tab).")
                         ),

                     # input
                     box(id="input",
                         width=12,
                         
                     selectizeInput(
                         inputId = "route.select",
                         label = "Route Number",
                         choices = unique(route.data$rtdd),
                         selected = unique(route.data$rtdd)[1],
                         multiple = TRUE,
                         options = list(maxItems = 10)
                     ),
                     selectizeInput(
                         inputId = "stop.select",
                         label = "Bus Stop ID",
                         choices = stop.list,
                         selected = stop.list[1],
                         multiple = FALSE
                     ),
                     selectizeInput(
                         inputId = "update.interval",
                         label = "Update Interval",
                         choices = interval.list,
                         selected = interval.list[1]
                     ),
                     uiOutput("last.update.interval")),
                     
                     # map info box
                     box(id="map.info",
                         width=12,
                         title="Map Information",
                         p("The map has three layers, all of which can be toggled 
                           in the top-right corner of the map."),
                         p("The first layer shows the selected bus stop - indicated by the blue home icon."),
                         p("The next layer tracks the current location of all buses on the
                           selected bus-lines. These are colored by route and have arrows 
                           indicating their current direction of travel."),
                         p("The final layer displays the routes of all the selected
                           bus-lines. Their colors match the bus-markers in the 
                           previous layer.")
                     ),
                 ),
                 # map and plots
                 column(
                     id = "mainPanel",
                     width = 8,
                     fluidRow(
                         id = "maprow",
                         style = "margin-right: 10px;",
                         shinyjs::useShinyjs(),
                         tags$style(
                             type = "text/css",
                             ".leaflet {height: calc(50vh - 90px) !important;}
                                body {background-color: #8899A6;}"
                         ),
                         # Map Output
                         leafletOutput("leaflet", height =
                                           500)
                     ),
                     br(),
                     fluidRow(
                         id = "inforow",
                         style = "background-color:#f8f8f8;margin-right:10px;",
                         tabsetPanel(
                             type = "pills",
                             tabPanel("Bus Information",
                                      box(
                                          width = 12,
                                          fluidRow(column(4,
                                                          uiOutput(
                                                              "display.bus.click"
                                                          )),
                                                   column(8,
                                                          plotlyOutput("prediction.plot"))),
                                          fluidRow(infoBoxOutput("bus.status.box"))
                                      )),
                             tabPanel("Stop Information",
                                      box(
                                          width = 12,
                                          fluidRow(column(4,
                                                          uiOutput("display.stop")),
                                                   column(8,
                                                          plotlyOutput(
                                                              "stop.prediction.plot"
                                                          ))),
                                          fluidRow(infoBoxOutput("next.bus.box"))
                                      ))
                         )
                     ),
                     hr(),
                     p("Author: Bobby Nelson"),
                     p("Updated: 3/1/2022"),
                     p("Data provided by Port Authority TrueTime")
                 )
             )),
    
    # top bar menu
    tabPanel("Data Table",
             fluidPage(
                 wellPanel(
                     h1("Bus Location Data"),
                     DT::dataTableOutput("bus.table"),
                     downloadButton("bus.download.button")
                 ),
                 wellPanel(
                     h1("Bus Prediction at Bus Stop"),
                     DT::dataTableOutput("stop.table"),
                     downloadButton("stop.download.button")
                 )
             ))
)

# Define server logic ----
server <- function(input, output, session) {
    ## handle API update frequency ----
    
    # set up a reactive for the input interval
    update.interval <- reactive({
        return(as.numeric(input$update.interval) * 1000)
    })
    
    # track when the last update occurred
    # inspired from: https://github.com/rstudio/shiny-examples/blob/main/086-bus-dashboard/server.R
    last.update <- reactive({
        bus.data()
        Sys.time()
    })
    
    # display for update interval
    # taken from: https://github.com/rstudio/shiny-examples/blob/main/086-bus-dashboard/server.R
    output$last.update.interval <- renderUI({
        invalidateLater(5000, session)
        p(class = "text-muted",
          "Data refreshed ",
          round(difftime(
              Sys.time(), last.update(), units = "secs"
          )),
          " seconds ago.")
    })
    
    ## pull bus-specific data ----
    
    # updating vehicle location data
    bus.data <- reactive({
        req(input$route.select)
        
        invalidateLater(update.interval())
        getBusData(value = input$route.select, type = "route")
    })
    
    # raw data table for display
    output$bus.table <-
        DT::renderDataTable(bus.data(), options = list(scrollX = TRUE))
    
    # download button for bus location data
    output$bus.download.button <- downloadHandler(
        # from documentation
        filename = function() {
            fname <-
                paste0("bus_location_routes_",
                       input$route.select,
                       collapse = "_")
            fname <-
                paste(fname,
                      "_",
                      str_replace_all(Sys.time(), ":|\ ", "_"),
                      ".csv",
                      sep = "")
        },
        content = function(file) {
            fwrite(bus.data(), file)
        }
    )
    
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
    
    # get bus stop info
    stop.data <- reactive({
        req(input$stop.select)
        
        bus.stop.df %>%
            filter(CleverID == input$stop.select)
    })
    
    # get bus stop prediction data
    stop.pred <- reactive({
        req(input$stop.select)
        invalidateLater(update.interval())
        
        getPredictionData(input$stop.select, type = "stpid")
    })
    
    # raw data table for bus stop display
    output$stop.table <-
        DT::renderDataTable(stop.pred(), options = list(scrollX = TRUE))
    
    # download button for bus stop data
    output$stop.download.button <- downloadHandler(
        # from documentation
        filename = function() {
            paste(
                "bus_stop_",
                input$stop.select,
                "_",
                str_replace_all(Sys.time(), ":|\ ", "_"),
                ".csv",
                sep = ""
            )
        },
        content = function(file) {
            write.csv(stop.pred(), file)
        }
    )
    
    
    ## base leaflet ----
    # leaflet map base + bus stops
    output$leaflet <- renderLeaflet({
        leaflet() %>%
            addProviderTiles("OpenStreetMap.HOT") %>%
            setView(-79.9959, 40.4406, 10) %>%
            addLayersControl(overlayGroups = c("Stops", "Buses", "Routes"))
    })
    
    ## update leaflet through observables ----
    
    # update route patterns
    observe({
        # get all routes in data for colors
        route.table <- unique(pattern.data()$rt)
        color.df <-
            tibble(rt = route.table, color = color.list[1:length(rt)])
        
        # convert lat and lon for plotting
        plot.data <- pattern.data() %>%
            inner_join(color.df, by = "rt")
        
        # clear old routes
        leafletProxy("leaflet") %>%
            clearGroup(group = "Routes") %>%
            removeControl(layerId = "legend") %>%
            addLegend(
                "bottomright",
                colors = color.df$color,
                labels = color.df$rt,
                title = "Route",
                layerId = "legend",
                opacity = 5
            )
        
        # add new routes
        for (route in route.table) {
            # simplify the route points into a multi-linestring
            # this GREATLY increases the leaflet responsiveness
            data.i <- plot.data %>%
                filter(rt == route) %>%
                st_as_sf(coords = c("lon", "lat")) %>%
                summarise(geometry = st_combine(geometry)) %>%
                st_cast("MULTILINESTRING")
            route.color <- color.df %>%
                filter(rt == route) %>%
                pull(color)
            leafletProxy("leaflet") %>%
                addPolylines(
                    data = data.i,
                    color = route.color,
                    opacity = 5,
                    group = "Routes"
                )
        }
    })
    
    # bus coordinate view
    # only update when bus data updates
    observe({
        # if no buses, clear the markers and then exit
        if (!("lat" %in% colnames(bus.data()))) {
            leafletProxy("leaflet") %>%
                clearGroup(group = "busPosition")
        } else {
            # get all routes in data for colors
            route.table <- unique(bus.data()$rt)
            color.df <-
                tibble(rt = route.table, color = color.list[1:length(rt)])
            
            # convert lat and lon for plotting
            plot.data <- bus.data() %>%
                mutate(
                    lat = as.numeric(lat),
                    lon = as.numeric(lon),
                    status = ifelse(dly == "false",
                                    "On-Time",
                                    "Delayed")
                ) %>%
                inner_join(color.df, by = "rt")
            
            # define custom icons for the map
            icons <- awesomeIcons(
                icon = "arrow-up",
                iconColor = "black",
                library = "fa",
                markerColor = plot.data$color,
                iconRotate = plot.data$hdg
            )
            
            # clear old markers and add new ones
            leafletProxy("leaflet", data = plot.data) %>%
                clearGroup(group = "Buses") %>%
                addAwesomeMarkers(
                    lng =  ~ lon,
                    lat =  ~ lat,
                    icon = icons,
                    popup =  ~ paste0(
                        "<h3>Route: ",
                        rt,
                        "</h3>",
                        "<h4>Bus ID: ",
                        vid,
                        "<br>",
                        "Destination: ",
                        des,
                        "<br>",
                        "Status: ",
                        status,
                        "<br>",
                        "Speed: ",
                        spd,
                        "</h4>"
                    ),
                    group = "Buses",
                    layerId =  ~ vid
                )
        }
    })
    
    # observe stop selection
    observe({
        display.data <- stop.data()
        
        # clear old markers and add new ones
        leafletProxy("leaflet", data = display.data) %>%
            clearGroup(group = "Stops") %>%
            addAwesomeMarkers(
                lng =  ~ as.numeric(Longitude),
                lat =  ~ as.numeric(Latitude),
                icon = icon("home"),
                popup =  ~ paste0(
                    "<h4>Stop Name: ",
                    Stop_name,
                    "<br><br>",
                    "Direction: ",
                    Direction,
                    "<br>",
                    "Routes: ",
                    Routes_ser,
                    "<br>",
                    "Sheltered: ",
                    has.shelter,
                    "</h4>"
                ),
                group = "Stops",
                layerId =  ~ CleverID
            )
    })
    
    ## handle user clicks on bus icons ----
    
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
        
        # don't process if clicked on bus stop
        if (user.click$id == input$stop.select) {
            return()
        }
        
        bus.click(user.click)
        bus.pred(getPredictionData(user.click$id, type = "vid"))
    })
    
    ## create displays (plots, UI, boxes, etc) ----
    
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
        
        tagList(h2(paste0("Route: ", display.data$rt)),
                h3(paste0("Bus ID: ", display.data$vid)),
                h3(paste0(
                    "Destination: ", display.data$des
                )),
                h3(paste0("Status: ", display.data$status)),
                h3(paste0(
                    "Current speed: ", display.data$spd, " mph"
                )))
    })
    
    # makes a prediction plot for bus arrivals
    output$prediction.plot <- renderPlotly({
        if (is.null(bus.click()) | is.null(bus.pred())) {
            return()
        }
        
        plot <- bus.pred() %>%
            mutate(
                `Stop Name` = as.factor(stpnm),
                prdtm = parse_date_time(prdtm, "%Y%m%d %h:%M"),
                `Arrival Time` = sprintf("%02d:%02d", hour(prdtm), minute(prdtm))
            ) %>%
            head(5) %>%
            arrange(desc(prdtm)) %>%
            mutate(`Stop Name` = factor(`Stop Name`, unique(`Stop Name`))) %>%
            ggplot(aes(y = `Stop Name`,
                       x = vid)) +
            geom_point(aes(color = `Arrival Time`),
                       size = 2,
                       shape = "square") +
            #geom_point(size=2, shape="square") +
            geom_text(
                aes(label = `Arrival Time`),
                nudge_x = 0.2,
                nudge_y = 0.1,
                size = 3,
                hjust = 0
            ) +
            #scale_y_continuous(trans = rev_date) +
            theme_minimal() +
            # want to add an arrow on the major.x, but plotly will not display it
            theme(
                panel.grid.major.x = element_line(linetype = "dashed", color = "black"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                legend.position = "none"
            ) +
            scale_color_hue() +
            ylab("") +
            xlab("") +
            ggtitle("Predicted Arrival Times")
        
        ggplotly(plot, tooltip = c("Arrival Time", "Stop Name"))
    })
    
    # makes an info box indicating if the bus is delayed or on-time
    output$bus.status.box <- renderInfoBox({
        empty.box <- infoBox(
            "Bus Status",
            value = "Not Selected",
            icon = icon("minus"),
            color = "black",
            fill = TRUE
        )
        if (is.null(bus.click())) {
            return(empty.box)
        }
        
        display.data <- bus.data() %>%
            filter(vid == bus.click()$id) %>%
            mutate(status = ifelse(dly == "false",
                                   "On-Time",
                                   "Delayed"))
        
        if (NROW(display.data) < 1) {
            return(empty.box)
        }
        
        if (display.data$status == "On-Time") {
            icon.type <- "check"
            color.type <- "green"
        } else {
            icon.type <- "exclamation"
            color.type <- "red"
        }
        
        infoBox(
            title = "Bus Status",
            value = paste0(display.data$status),
            icon = icon(icon.type, lib = "font-awesome"),
            color = color.type,
            fill = TRUE
        )
    })
    
    # format stop display graphic
    output$display.stop <- renderUI({
        display.data <- stop.data()
        
        tagList(h3(paste0(
            "Stop Name: ", display.data$Stop_name
        )),
        h3(paste0(
            "Direction: ", display.data$Direction
        )),
        h3(paste0(
            "Routes: ", display.data$Routes_ser
        )),
        h3(paste0(
            "Sheltered: ", display.data$has.shelter
        )))
    })
    
    # make prediction plot for bus arrivals at a specific bus stop
    output$stop.prediction.plot <- renderPlotly({
        if (is.null(stop.pred())) {
            return()
        }
        
        plot <- stop.pred() %>%
            mutate(
                bus.id = paste0("Bus: ", vid, ", Route: ", rt),
                prdtm = parse_date_time(prdtm, "%Y%m%d %h:%M"),
                `Arrival Time` = sprintf("%02d:%02d", hour(prdtm), minute(prdtm))
            ) %>%
            arrange(desc(prdtm)) %>%
            mutate(bus.id = factor(bus.id, unique(bus.id))) %>%
            ggplot(aes(y = bus.id,
                       x = as.factor(rt))) +
            geom_point(aes(color = rt), size = 2, shape = "square") +
            geom_text(
                aes(label = `Arrival Time`),
                nudge_x = 0.25,
                nudge_y = 0.1,
                size = 3,
                hjust = 0
            ) +
            theme_minimal() +
            # want to add an arrow on the major.x, but plotly will not display it
            theme(
                panel.grid.major.x = element_line(linetype = "solid", color = "black"),
                panel.grid.minor.x = element_line(linetype = "solid", color =
                                                      "black"),
                legend.position = "none"
            ) +
            scale_color_hue() +
            ylab("") +
            xlab("Route") +
            ggtitle("Predicted Arrival Times")
        
        ggplotly(plot, tooltip = c("Arrival Time", "bus.id", "rt"))
    })
    
    # info box with next bus arrival
    output$next.bus.box <- renderInfoBox({
        if (is.null(stop.pred())) {
            return(
                infoBox(
                    "Next Bus Arrives in",
                    value = " Data not found",
                    icon = icon("minus"),
                    color = "black",
                    fill = TRUE
                )
            )
        }
        
        arrival.time <- stop.pred()[1, "prdctdn"]
        
        if (arrival.time == "DUE") {
            arrival.time <- 0
        }
        
        infoBox(
            title = "Next Bus Arrives in",
            value = paste0(arrival.time, " minutes"),
            icon = icon("clock"),
            color = "green",
            fill = TRUE
        )
    })
}

# Run the application ----
shinyApp(ui = ui, server = server)
