## Title: Pittsburgh Bus Tracking Application
### Author: Bobby Nelson
### Date: 3/4/2022

## About

This tool is an interactive Shiny application for tracking Pittsburgh buses in 
real time. The data is polled from the [Port Authority TrueTime API](http://truetime.portauthority.org/).

## Features

The application has the following features:

* User input section:
    1. Users may search for up to 10 bus lines at a time.
    2. Users may search for a single bus stop.
    3. Users may change the update interval for the map (default is 30 seconds).
* A live-updating map of buses in Pittsburgh:
    * There are three layers:
        1. The currently selected bus stop.
        2. Current positions of all buses on the selected bus routes (up to 10 routes).
        3. All routes for the selected bus lines (up to 10).
    * Bus positions include a directional arrow showing their current heading.
    * All markers on the map can be clicked-on to display more information.
* An information dashboard on the currently selected bus.
    * Shows text information on the bus number, destination, route, and speed.
    * Indicates whether the bus is on-time or delayed.
    * Displays the predicted arrival times for the next 5 stops.
* An information dashboard on the currently selected bus stop.
    * Shows text information on the stop name, direction, routes, and whether it is sheltered.
    * Indicates when the next bus will arrive.
    * Displays the predicted arrival times for the next buses (all routes).

## Access

View the application at: https://rgnelson.shinyapps.io/BusTracker/.
