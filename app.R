# app test
library(shiny)
library(rgdal)
library(leaflet)

# Read this shape file with the rgdal library. 
world <- readOGR( 
  dsn= paste0(getwd(),"/data/ne_50m_admin_0_countries_namenew/") , 
  layer="ne_50m_admin_0_countries_namenew",
  verbose=FALSE
)

# load postcrossing data
pc.data <- read.csv("data/postInfoScrape.csv")

# make color based on 
world@data$covid_blocked <- pc.data$covid.blocked[match(world@data$NAME_NEW, pc.data$send.country)]
world@data$covid_blocked_color <- ifelse(is.na(world@data$covid_blocked), "white", 
                                         ifelse(world@data$covid_blocked == "sending", "green", "red"))


# prepare mouseover text
world@data$covid_blocked_text <- ifelse(is.na(world@data$covid_blocked), paste0("No sending info<br/>for ", world@data$NAME_NEW), 
                                        ifelse(world@data$covid_blocked == "sending", paste0("Click here to see <br/>where ", world@data$NAME_NEW,  "<br/>does not send mail"),
                                               paste0("Because of COVID, <br/>", world@data$NAME_NEW, " neither sends <br/>nor receives mail")))
mytext <- world@data$covid_blocked_text %>%
  lapply(htmltools::HTML)

# make sure projection is uniform
proj4string(world) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


  
  ui <- fluidPage(
    title = "Where can't I send a postcard?",
    
    h2("Where can't I send a postcard?"),
    a("Data via Postcrossing", href="https://www.postcrossing.com/postal-monitor"),
    leafletOutput('map', height=600, width=900)
    )
  
  server <- function(input, output, session) {
    
    output$map <- renderLeaflet({
      leaflet(world) %>% 
        addTiles() %>% 
        setView(lat=20, lng=0 , zoom=2) %>%
        addPolygons( 
          fillColor = world@data$covid_blocked_color, 
          stroke=TRUE, 
          #fillOpacity = 0.9, 
          color="white", 
          weight=0.3,
          group = 'B',
          label = mytext,
          labelOptions = labelOptions( 
            style = list("font-weight" = "normal", padding = "3px 8px"), 
            textsize = "13px", 
            direction = "auto"
          )
        )
    })
    
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
      clicked.colors <- ifelse(world@data$NAME_NEW %in% now.blocked.vec, "red", ifelse(world@data$NAME_NEW == clicked.country, "yellow", "green"))
      #retrieves country in which the click point resides, set CRS for country
      selected <- world[point, ]
      proj4string(selected) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      proxy <- leafletProxy("map")
        proxy %>% 
          clearGroup('A') %>%
          clearGroup('B') %>%
          addPolygons(data = world, 
                              fillColor = clicked.colors,
                              color = "red",
                              weight = 3, 
                              stroke = F,
                              group = 'A',
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

