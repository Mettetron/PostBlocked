# silence warnings
oldw <- getOption("warn")
options(warn = -1)

# load packages
suppressMessages(library(shiny))  
suppressMessages(library(rvest))  # website scrape
suppressMessages(library(rgdal))  # dealing with shapefiles
suppressMessages(library(leaflet))  # fancy interactive map


# read in the map shapefile using the rgdal package. 
world <- readOGR( 
  dsn= paste0(getwd(),"/data/ne_50m_admin_0_countries_namenew/") , 
  layer="ne_50m_admin_0_countries_namenew",
  verbose=FALSE
)

# load postcrossing data
pc.data <- read.csv("data/postInfoScrape.csv")

# Check for updates - and update if relevant
pc.page <- read_html("https://www.postcrossing.com/postal-monitor")
# get date of update
updated <- pc.page %>% html_nodes(".last:nth-child(5)") %>% html_text(trim = TRUE)
if (updated != pc.data$updated[1]) {
  source("postInfoScrape.R")
  # reload updated data
  pc.data <- read.csv("data/postInfoScrape.csv")
}

## prepare for initial map
# prepare variables to be used for coloring 
world@data$covid_blocked <- pc.data$covid.blocked[match(world@data$NAME_NEW, pc.data$send.country)]
covid.blocked.countries <- world@data$NAME_NEW[which(world@data$covid_blocked == "blocked")]
non.info.countries <- world@data$NAME_NEW[which(is.na(world@data$covid_blocked))]
info.countries <- world@data$NAME_NEW[which(world@data$covid_blocked == "sending")]

# map color function
mapColPrep <- function(my.country) {
  # find blocked countries 
  now.blocked.string <- as.character(pc.data[as.character(pc.data$send.country) == as.character(my.country), "blocked.list"])
  now.blocked.vec <- unlist(strsplit(now.blocked.string, split="_"))
  # make color palette
  if (my.country %in% info.countries) {
    my.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", 
                        ifelse(world@data$NAME_NEW %in% now.blocked.vec, "red", 
                               ifelse(world@data$NAME_NEW == my.country, "yellow", "green")))
  } else if (my.country %in% non.info.countries) {
    my.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00",
                        ifelse(world@data$NAME_NEW == my.country, "yellow", "#cccccc"))
  } else if (my.country %in% covid.blocked.countries) {
    my.colors <- ifelse(world@data$NAME_NEW == my.country, "yellow",
                        ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", "red"))
  }
  
  return(my.colors)
}


# output table function
dfPrep <- function(my.country) {
  # get my country can't send to
  if (my.country %in% covid.blocked.countries) {
    no.send.to <- "All"
  } else if (my.country %in% non.info.countries) {
    no.send.to <- "No information available"
  } else if (my.country %in% info.countries) {
    now.blocked.string <- as.character(pc.data[as.character(pc.data$send.country) == as.character(my.country), "blocked.list"])
    no.send.to <- c(as.character(covid.blocked.countries), unlist(strsplit(now.blocked.string, split="_")))
  }
  # get aren't sending to my country
  no.receive.from <- as.character(covid.blocked.countries)
  for (n in 1:nrow(pc.data)) {
    b <-  unlist(strsplit(as.character(pc.data[n, "blocked.list"]), split="_"))
    if (my.country %in% b) {
      no.receive.from <- c(no.receive.from, as.character(pc.data[n, "send.country"]))
    }
  }
  # unique and sort
  no.send.to <- sort(unique(no.send.to))
  no.receive.from <- sort(unique(no.receive.from))
  # make df
  table.rows <- max(length(no.send.to), length(no.receive.from))
  length(no.send.to) <- table.rows
  length(no.receive.from) <- table.rows
  my.country.df <- data.frame(no.send.to, no.receive.from)
  colnames(my.country.df) <- c(paste0("Countries that ", my.country, " does not send mail to"),
                               paste0("Countries that do not send mail to ", my.country))
  return(my.country.df)
}

# prepare mouseover text
world@data$covid_blocked_text <- ifelse(is.na(world@data$covid_blocked), paste0("No sending info<br/>for ", world@data$NAME_NEW), 
                                        ifelse(world@data$covid_blocked == "sending", paste0("Click here to see <br/>where ", world@data$NAME_NEW,  "<br/>does not send mail"),
                                               paste0("Because of COVID, <br/>", world@data$NAME_NEW, " neither sends <br/>nor receives international mail")))
mytext <- world@data$covid_blocked_text %>%
  lapply(htmltools::HTML)

# make sure projection is uniform
proj4string(world) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# to use for countries in searchbar
all.countries <- c(sort(as.character(world@data$NAME_NEW)), "none searched")

ui <- fluidPage(
  
  title = "Where can't I send a postcard?",
  
  h2(textOutput("country")),
  h4("Click the map or use the seach bar to find information about your country of interest"),
  fluidRow(
    column(8,
           leafletOutput('map', height=600),
           h6("Data collected by", align = "center"),
           HTML('<div style="text-align: center;"><p><a href="https://www.postcrossing.com/postal-monitor">
                  <img src="PClogo.png" width="282" height="36" style="text-align:center" /></a></p></div>'),
           h6(as.character(pc.data$updated[1]), align = "center")
           
           
    ),
    column(4, 
           selectizeInput("searched.country",  # selectizeInput makes writable and searchable dropdown menu
                          "Search country",
                          choices = all.countries, 
                          selected = "none searched"), 
           downloadButton("downloadData", "Download Table"),
           tableOutput('info_table')
    )
  ),
  
  # for user geolocation (with prompt) https://github.com/AugustT/shiny_geolocation
  tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              ')
)

server <- function(input, output, session) {
  
  # basic start map
  output$map <- renderLeaflet({
    
    # if user has allowed geolocation, use that to select start country
    if (!is.null(input$lat)){
      lat <- input$lat
      lon <- input$long
      coords <- as.data.frame(cbind(lon, lat))
      point <- SpatialPoints(coords)
      proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      selected.country <- world[point, ]@data$NAME_NEW
      print(paste(selected.country, "geolocated"))
      output$country <- renderText({ 
        paste0("Where can't I send a postcard from ", selected.country, "?")
      })
    } else {
      selected.country <- "Germany"  # else use Germany as start country
      output$country <- renderText({ 
        paste0("Where can't I send a postcard from ", selected.country, "?")
      })
    }
    
    # table
    output$info_table <- renderTable(dfPrep(selected.country)) 
    output$downloadData <- downloadHandler(
      filename = paste0("PostBlocked_", selected.country, ".csv"),
      content = function(file) {
        write.csv(dfPrep(selected.country), file, row.names = FALSE)
      }
    )
    
    # plot
    leaflet(world) %>% 
      addTiles() %>% 
      setView(lat=20, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = mapColPrep(selected.country), 
        stroke = F, 
        group = 'initialColors',
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>% addLegend(colors=c("yellow", "green", "red", "#cccccc", "#5b0f00"), 
                      labels=c("Selected country", "Receives mail from selected country", "Blocked by selected country", "Information lacking", "COVID blocked"), 
                      opacity=0.3, position = "bottomleft")
    
  })
  
  ## add different map on click
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    
    if(is.null(click))
      return()   
    
    #pulls lat and lon from shiny click event
    lat <- click$lat
    lon <- click$lng
    
    #puts lat and lon for click point into its own data frame
    coords <- as.data.frame(cbind(lon, lat))
    
    #converts click point coordinate data frame into SP object, sets CRS
    point <- SpatialPoints(coords)
    proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    selected.country <- world[point, ]@data$NAME_NEW
    print(paste(selected.country, "clicked"))
    
    # title
    output$country <- renderText({ 
      paste0("Where can't I send a postcard from ", selected.country, "?")
    })
    
    # table
    output$info_table <- renderTable(dfPrep(selected.country)) 
    output$downloadData <- downloadHandler(
      filename = paste0("PostBlocked_", selected.country, ".csv"),
      content = function(file) {
        write.csv(dfPrep(selected.country), file, row.names = FALSE)
      }
    )
    
    # plot
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup('selectedColors') %>%
      clearGroup('initialColors') %>%
      setView(lat=20, lng=0 , zoom=2) %>%
      addPolygons(data = world, 
                  fillColor = mapColPrep(selected.country),
                  color = "red",
                  weight = 3, 
                  stroke = F,
                  group = 'selectedColors',
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  )
      )
  })
  
  ## add different map on search
  observeEvent(input$searched.country, {
    selected.country <- input$searched.country
    print(paste(selected.country, "searched"))
    
    if(selected.country == "none searched") {
      return()
    } else if (!selected.country %in% all.countries) {
      return ()
    }
    
    # title
    output$country <- renderText({ 
      paste0("Where can't I send a postcard from ", selected.country, "?")
    })
    
    # table
    output$info_table <- renderTable(dfPrep(selected.country)) 
    output$downloadData <- downloadHandler(
      filename = paste0("PostBlocked_", selected.country, ".csv"),
      content = function(file) {
        write.csv(dfPrep(selected.country), file, row.names = FALSE)
      }
    )
    
    # plot
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup('selectedColors') %>%
      clearGroup('initialColors') %>%
      setView(lat=20, lng=0 , zoom=2) %>%
      addPolygons(data = world, 
                  fillColor = mapColPrep(selected.country),
                  stroke = F,
                  group = 'selectedColors',
                  label = mytext,
                  labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                  )
      )
  })
}

# turn warnings back on
options(warn = oldw)

shinyApp(ui, server)



