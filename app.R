# load packages
library(shiny)
library(rgdal)
library(leaflet)

# read in the map shapefile using the rgdal package. 
world <- readOGR( 
  dsn= paste0(getwd(),"/data/ne_50m_admin_0_countries_namenew/") , 
  layer="ne_50m_admin_0_countries_namenew",
  verbose=FALSE
)

# load postcrossing data
pc.data <- read.csv("data/postInfoScrape.csv")

## prepare for initial map
# make color based on covid blocking and wether or not there is sending info
world@data$covid_blocked <- pc.data$covid.blocked[match(world@data$NAME_NEW, pc.data$send.country)]
# prepare variables to be used for coloring later
covid.blocked.countries <- world@data$NAME_NEW[which(world@data$covid_blocked == "blocked")]
non.info.countries <- world@data$NAME_NEW[which(is.na(world@data$covid_blocked))]
info.countries <- world@data$NAME_NEW[which(world@data$covid_blocked == "sending")]

# fix colors on start map to show Germany selected
selected.country <- "Germany"
now.blocked.string <- as.character(pc.data[as.character(pc.data$send.country) == as.character(selected.country), "blocked.list"])
now.blocked.vec <- unlist(strsplit(now.blocked.string, split="_"))
# make color palette
if (selected.country %in% info.countries) {
  map.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", 
                           ifelse(world@data$NAME_NEW %in% now.blocked.vec, "red", 
                                  ifelse(world@data$NAME_NEW == selected.country, "yellow", "green")))
} else if (selected.country %in% non.info.countries) {
  map.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00",
                           ifelse(world@data$NAME_NEW == selected.country, "yellow", "#cccccc"))
} else if (clicked.country %in% covid.blocked.countries) {
  map.colors <- ifelse(world@data$NAME_NEW == selected.country, "yellow",
                           ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", "red"))
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

## preparing for map title
# this way of adding a map title is a little hacky - it's really only covering up the old title, and it gets messy if very long names are selected.
current.country <- selected.country
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 100px; 
    padding-right: 100px; 
    background: rgba(255,255,255,1);
    font-weight: bold;
    font-size: 16px;
  }
"))
  
  ui <- fluidPage(
    title = "Where can't I send a postcard?",
    
    h2("Where can't I send a postcard?"),
    h4("Click the map or use the seach bar below it to find information about your country of interest"),
    leafletOutput('map', height=600, width=1000),
    fluidRow(
      column(9,
             selectizeInput("searched.country",  # selectizeInput makes writable and searchable dropdown menu
                   "Search country",
                   choices = all.countries, 
                   selected = "none searched"
                   )
             ),
      column(3,
             a("Data via Postcrossing", href="https://www.postcrossing.com/postal-monitor"),
             h6(as.character(pc.data$updated[1]))
      )
    )
    )
  
  server <- function(input, output, session) {
    
    # map title depending on selected country
    map.title <- paste0("Selected: ", current.country)

    title <- tags$div(
      tag.map.title, HTML(map.title)
    ) 
    
    # basic start map
    output$map <- renderLeaflet({
      leaflet(world) %>% 
        addTiles() %>% 
        addControl(title, position = "topleft", className="map-title") %>%
        setView(lat=20, lng=0 , zoom=2) %>%
        addPolygons( 
          fillColor = map.colors, 
          stroke=TRUE, 
          #fillOpacity = 0.9, 
          color="white", 
          weight=0.3,
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
    
    # add different map on click
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      
      if(is.null(click))
        return()   
      
      #pulls lat and lon from shiny click event
      lat <- click$lat
      lon <- click$lng
      #print(lat)
      #print(lon)
      #lat <- 55.08815
      #lon <-  9.190774
      
      #puts lat and lon for click point into its own data frame
      coords <- as.data.frame(cbind(lon, lat))
      
      #converts click point coordinate data frame into SP object, sets CRS
      point <- SpatialPoints(coords)
      proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      # find blocked countries based on clicked country
      clicked.country <- world[point, ]@data$NAME_NEW
      now.blocked.string <- as.character(pc.data[as.character(pc.data$send.country) == as.character(clicked.country), "blocked.list"])
      now.blocked.vec <- unlist(strsplit(now.blocked.string, split="_"))
      # make color palette
      if (clicked.country %in% info.countries) {
        clicked.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", 
                                 ifelse(world@data$NAME_NEW %in% now.blocked.vec, "red", 
                                        ifelse(world@data$NAME_NEW == clicked.country, "yellow", "green")))
      } else if (clicked.country %in% non.info.countries) {
        clicked.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00",
                                 ifelse(world@data$NAME_NEW == clicked.country, "yellow", "#cccccc"))
      } else if (clicked.country %in% covid.blocked.countries) {
        clicked.colors <- ifelse(world@data$NAME_NEW == clicked.country, "yellow",
                                 ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", "red"))
      }
      # prepare title
      current.country <- clicked.country
      map.title <- paste0("Selected: ", current.country)
      
      title <- tags$div(
        tag.map.title, HTML(map.title)
      ) 
      
      proxy <- leafletProxy("map")
        proxy %>% 
          clearGroup('selectedColors') %>%
          clearGroup('initialColors') %>%
          addControl(title, position = "topleft", className="map-title") %>%
          addPolygons(data = world, 
                              fillColor = clicked.colors,
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
    
    # add different map on search
    observeEvent(input$searched.country, {
      searched.country <- input$searched.country
      
      if(searched.country == "none searched") {
        return()
      } else if (!searched.country %in% all.countries) {
        return ()
      }
           
      current.country <- searched.country
      # generate the new colors
      now.blocked.string <- as.character(pc.data[as.character(pc.data$send.country) == as.character(searched.country), "blocked.list"])
      now.blocked.vec <- unlist(strsplit(now.blocked.string, split="_"))
      # make color palette
      if (searched.country %in% info.countries) {
        clicked.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", 
                                 ifelse(world@data$NAME_NEW %in% now.blocked.vec, "red", 
                                        ifelse(world@data$NAME_NEW == searched.country, "yellow", "green")))
      } else if (searched.country %in% non.info.countries) {
        clicked.colors <- ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00",
                                        ifelse(world@data$NAME_NEW == searched.country, "yellow", "#cccccc"))
      } else if (searched.country %in% covid.blocked.countries) {
        clicked.colors <- ifelse(world@data$NAME_NEW == searched.country, "yellow",
                                 ifelse(world@data$NAME_NEW %in% covid.blocked.countries, "#5b0f00", "red"))
      }
      current.country <- searched.country
      map.title <- paste0("Selected: ", current.country)
      
      title <- tags$div(
        tag.map.title, HTML(map.title)
      ) 

      proxy <- leafletProxy("map")
      proxy %>% 
        clearGroup('selectedColors') %>%
        clearGroup('initialColors') %>%
        addControl(title, position = "topleft", className="map-title") %>%
        addPolygons(data = world, 
                    fillColor = clicked.colors,
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
    
    
  }

shinyApp(ui, server)

