## need to match up names in map to names used on Postcrossing
# map from: https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/

library(rgdal)  # for working with shapefiles

# load data from postcrossing
pc.data <- read.csv("data/postInfoScrape.csv")
# pack out the blocked countries
bc <- c()
for (n in 1:nrow(pc.data)){
  b <- unlist(strsplit(as.character(pc.data$blocked.list[n]), split="_"))
  bc <- c(bc, b)
}

all.countries <- sort(unique(c(as.character(pc.data$send.country), bc)))

# read shapefile
world <- readOGR( 
  dsn= paste0(getwd(),"/data/ne_50m_admin_0_countries/") , 
  layer="ne_50m_admin_0_countries",
  verbose=FALSE
)

# compare with the ones in postcrossing data
only.in.PC <- all.countries[which(!all.countries %in% world$NAME_LONG)]
only.on.map <- sort(world$NAME_LONG[which(!world$NAME_LONG %in% all.countries)])
# several do not match, many for spelling reasons. I will change the names in the map
hej <- cbind(only.in.PC, c(as.character(only.on.map), rep("NA", 7)))
#write.csv(hej, "data/matchingCountryNames.csv", fileEncoding = "UTF-8")

# manual checking of the county names that don't match up

# changing names in the map
name.changes <- read.csv("data/nameChanges.csv")
# prepare vectors of all names - current and correct
in.both <- as.character(sort(world$NAME_LONG[which(world$NAME_LONG %in% all.countries)]))
correct.names <- c(name.changes$pc, in.both)
current.names <- c(name.changes$map, in.both)
# match them up to get corect names in map
world@data$NAME_NEW <- correct.names[match(world$NAME_LONG, current.names)]

# save new shapefile
writeOGR(world, driver="ESRI Shapefile", 
         dsn = paste0(getwd(),"/data/ne_50m_admin_0_countries_namenew/") , 
         layer = "ne_50m_admin_0_countries_namenew",
         layer_options = "ENCODING=UTF-8")


