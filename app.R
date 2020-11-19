# load packages
library(shiny)
library(rgdal)
library(leaflet)

# read in the map shapefile using the rgdal package. 
world <- readOGR( 
  dsn= paste0(getwd(),"/data_publish/ne_50m_admin_0_countries_namenew/") , 
  layer="ne_50m_admin_0_countries_namenew",
  verbose=FALSE
)

# load postcrossing data
pc.data <- read.csv("data_publish/postInfoScrape.csv")

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
  } else if (clicked.country %in% covid.blocked.countries) {
    my.colors <- ifelse(world@data$NAME_NEW == my.country, "yellow",
                        ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", "red"))
  }
  
  return(my.colors)
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
all.countries <- c(as.character(world@data$NAME_NEW), "none searched")
  
  ui <- fluidPage(
    
    title = "Where can't I send a postcard?",
    
    h2(textOutput("country")),
    h4("Click the map or use the seach bar to find information about your country of interest"),
    fluidRow(
      column(9,
             leafletOutput('map', height=600)
             ),
      column(3,
             selectizeInput("searched.country",  # selectizeInput makes writable and searchable dropdown menu
                            "Search country",
                            choices = all.countries, 
                            selected = "none searched"
             )
             )
    ),
    fluidRow(column(9, align = "center", h6("Data collected by"))
    ),
    fluidRow(column(9,align = "center", HTML('<p><a href="https://www.postcrossing.com/postal-monitor">
                  <img src="PClogo.png" width="282" height="36" /></a></p>'))
    ),
    fluidRow(column(9,align = "center", h6(as.character(pc.data$updated[1])))
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
        output$country <- renderText({ 
          paste0("Where can't I send a postcard from ", selected.country, "?")
        })
      } else {
        selected.country <- "Germany"  # else use Germany as start country
        output$country <- renderText({ 
          paste0("Where can't I send a postcard from ", selected.country, "?")
        })
      }
      
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
      
      output$country <- renderText({ 
        paste0("Where can't I send a postcard from ", selected.country, "?")
      })
      
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
      
      if(selected.country == "none searched") {
        return()
      } else if (!selected.country %in% all.countries) {
        return ()
      }
      
      output$country <- renderText({ 
        paste0("Where can't I send a postcard from ", selected.country, "?")
      })
      
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

shinyApp(ui, server)

