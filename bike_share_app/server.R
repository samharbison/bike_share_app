#server
library(shiny)
library(tidyverse)
library(jsonlite)
library(shinyWidgets)
library(rvest)

function(input, output, session) {
  #jsondata = read_csv("stationJSON_urls.csv")
  jsondata = as.data.frame(read_html(('https://github.com/NABSA/gbfs/blob/master/systems.csv')
  ) %>%
    html_table(fill = TRUE)) %>%
    setNames(.[1, ]) %>%
    slice(-1) %>%
    select(-1) %>%
    filter(`Country Code` == 'US') %>%
    filter(!grepl("Lime|Spin", Name))
  
  output$city = renderUI({
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = TRUE,
      top = 60,
      left = "auto",
      right = 20,
      bottom = "auto",
      width = 330,
      height = "auto",
      style = "z-index:1000;",
      div(id = 'inst',
          h5("How it works"),
          p("1. Choose a city!"),
          p("2. Click Go!"),
          p("3. Once the map appears, click on a marker. If a marker is pink, 
          that means a bike is available. If a marker is Red, that means 
          the dock currently does not have any bike available."),
          p('When you click on a marker, you will find the bikeshare company name,
            the name of the statione, and availability information')
      ),
      pickerInput(
        "city_of_choice",
        "Choose a city!",
        unique(jsondata$Location) %>% sort(),
        options = list(title = "Pick a City")
      ),
      # shinyjs::hidden(pickerInput('docked', 'Docked or Dockless?', choices = list("Docked" = "docked",
      #                                                                             "Dockless" = "dockless"))),
      actionButton("go", "GO"), br(), br(),
      shinyjs::hidden(div(id = 'inst2',
          p("Pink = Available Bikes"),
          p("Red = No Available Bikes"))
    ))
    
  })
  tablemaker <- reactive({
    withProgress(message = "Loading Data", value = .2, {
      ind = jsondata[which(jsondata$Location == input$city_of_choice), ]
      l = NULL
      l = lapply(1:nrow(ind), function(x) {
        d = try(fromJSON(ind[x, 'Auto-Discovery URL'])$data$en$feeds$url, TRUE)
        if (class(d) == 'try-error') {
          d=NULL
        } else {
          d = d[grep(c('station_status|station_information'), d)]
        }
        
        l = try(lapply(1:length(d), function(i) {
          fromJSON(d[i])$data$stations
        }), TRUE)
        if (class(l) == 'try-error') {
          l = NULL
        } else if (class(l[[1]]) == 'list' & is.null(nrow(l[[1]]))){
          l = NULL
        }
        if (is.null(l)) {
          tab = NULL
        } else {
          tab = Reduce(function(x, y) {
            inner_join(x, y, by = 'station_id')
          }, l) %>%
            select(-grep("num_bikes_available_types", names(.))) %>%
            mutate(PName = rep(ind[x, 'Name'], nrow(.))) %>%
            select(
              station_id,
              name,
              lon,
              lat,
              num_bikes_available,
              num_docks_available,
              PName
            )
        }
        tab
      })
      incProgress(.7)
      # d = fromJSON(ind)$data$en$feeds$url
      # d = d[grep(c('station_status|station_information'),d)]
      # #tab = inner_join(fromJSON(d$station_info)$data$stations, fromJSON(d$station_status)$data$stations, by = "station_id")
      if (length(l) == 1 & is.null(l[[1]])) {
        tab = NULL
      } else {
        tab = do.call('bind_rows', l)
      }
      return(tab)
    })
  })
  
  
  mapper = eventReactive(input$go, {
    shinyjs::hide('well')
    shinyjs::hide('inst')
    shinyjs::show('inst2')
    if (is.null(tablemaker())) {
      showModal(modalDialog(tagList(
        h2("Sorry, something isn't working properly!"),
        p(
          "There is a kink in our data chain and we don't have the
          information you are looking for at the moment."
        )
        )))
    } else {
      leaflet(data = tablemaker()) %>%
        addTiles() %>%
        addAwesomeMarkers(
          ~ lon,
          ~ lat,
          popup = paste(
            sep = "<br/>",
            paste("<b>", tablemaker()$PName, "</b>"),
            paste("<b>", tablemaker()$name, "</b>"),
            paste(
              "Number of Bikes Available:",
              tablemaker()$num_bikes_available,
              sep = " "
            ),
            paste(
              "Number of Docks Available:",
              tablemaker()$num_docks_available,
              sep = " "
            )
          ),
          icon = awesomeIcons(
            icon = "bicycle",
            iconColor = "black",
            library = "fa",
            markerColor = sapply(tablemaker()$num_bikes_available, function(x) {
              if (x == 0) {
                "red"
              } else {
                "pink"
              }
            })
          )
        )
    }
  })
  
  
  output$map = renderLeaflet({
    mapper()
  })
}