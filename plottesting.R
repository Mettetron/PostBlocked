# plotting on map test
# leaflet guide: https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html

library(leaflet)  # for interactive map plotting
library(rgdal)  # for working with shapefiles

# Read this shape file with the rgdal library. 
library(rgdal)
world <- readOGR( 
  dsn= paste0(getwd(),"/data/ne_50m_admin_0_countries_namenew/") , 
  layer="ne_50m_admin_0_countries_namenew",
  verbose=FALSE
)

# load postcrossing data
pc.data <- read.csv("data/postInfoScrape.csv")

# make color based on 
world@data$covid_blocked <- pc.data$covid.blocked[match(world@data$NAME_NEW, pc.data$send.country)]
world@data$covid_blocked_color <- ifelse(is.na(world@data$covid_blocked), "grey", 
                                         ifelse(world@data$covid_blocked == "sending", "green", "red"))


# # # Create a color palette for the map:
# mypalette <- colorNumeric(palette="viridis", domain=world@data$covid_blocked, na.color="transparent")
# mypalette(c(1,2))

# starting map with covid blocked countries red, and countries with sending info green
m <- leaflet(world) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons(fillColor = world@data$covid_blocked_color, stroke=FALSE)
m

# prepare mouseover text
world@data$covid_blocked_text <- ifelse(is.na(world@data$covid_blocked), "No sending info", 
                                         ifelse(world@data$covid_blocked == "sending", "Click this country <br/>to see where it <br/>does not send mail",
                                                "Because of COVID, <br/>this country neither sends <br/>nor receives mail"))
mytext <- world@data$covid_blocked_text %>%
  lapply(htmltools::HTML)

# mytext <- paste(
#   "Country: ", world_spdf@data$NAME,"<br/>", 
#   "Area: ", world_spdf@data$AREA, "<br/>", 
#   "Population: ", round(world_spdf@data$POP2005, 2), 
#   sep="") %>%
#   lapply(htmltools::HTML)

leaflet(world) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( 
    fillColor = world@data$covid_blocked_color, 
    stroke=TRUE, 
    #fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) 

m 
