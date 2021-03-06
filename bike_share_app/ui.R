#ui
library(shiny)
library(tidyverse)
library(leaflet)
library(shinyjs)

tagList(useShinyjs(),
        tags$head(
          tags$meta(charset="utf-8"),
          #tags$meta(http-equiv="X-UA-Compatible", content="IE=edge"),
          tags$meta(name="viewport", content="initial-scale=1,user-scalable=no,maximum-scale=1,width=device-width"),
          tags$meta(name="mobile-web-app-capable", content="yes"),
          tags$meta(name="apple-mobile-web-app-capable", content="yes"),
          tags$meta(name="theme-color", content="#000000"),
          tags$meta(name="description", content=""),
          tags$meta(name="author", content="")
        ),
        navbarPage("US Bike Shares",
                   id = "nav",
                   
                   tabPanel(
                     "Map",
                     absolutePanel(id = 'well',
                                 class = "panel panel-default",
                                 fixed = TRUE,
                                 draggable = TRUE,
                                 top = 'auto',
                                 left = "auto",
                                 right = 'auto',
                                 bottom = "auto",
                                 width = '60%',
                                 height = "auto",
                                 style = "z-index:1000;",
                       div(id = 'well2',
                         h2("Welcome to a Bikeshare Mapping app", style = "margin: 10px;"),
                         p("The North American Bikeshare Association has created the General Bikeshare Feed Specification,
                           an open data standard for bikeshare", style = "margin: 10px;"),
                         p("The NABSA publishes a known list of bikeshare systems who publish GBFS data feeds.
                           All the information in this site comes from those feeds which are mantained by the GBFS community.
                           This application only contains the subset of these feeds that are in the United States.", style = "margin: 10px;"),
                         a("Here is a link to NABSA's github.",href="https://github.com/NABSA/gbfs", target = "_blank", style = "margin: 10px;"),
                         br(),
                         p("Choose a city, click Go, and enjoy!", style = "margin: 10px;")
                       )
                     ),
                     div(
                       class = "outer",
                       tags$head(includeCSS("styles.css")),
                       leafletOutput("map", width = '100%', height = '100%'),
                       uiOutput("city")
                     )
                   )))