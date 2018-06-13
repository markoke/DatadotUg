# Libraries
library(dplyr)
library(ggmap)
library(ggplot2)

# First plotting the map of mubende which is our first Gold location place
mubende <- c(lat="0.5773",lon= "31.5370")

#Extracting the map from google map 
mubendeMap <- get_map(location = c(lat=0.5773, lon =31.5370),
                      zoom = 10,
                      maptype = "roadmap")
#Visualizing the map
mubende_map <- ggmap(mubendeMap)
mubende_map

# Importing the dataset containing the map data
artisanal_data <- read.csv("data/artisanal-mining-locations.csv")

#checking for what the data contains
str(artisanal_data)

head(artisanal_data,5)

# subsetting to importance columns only
artisanal_data <- artisanal_data[ , 2:11]

# Focusing analysis to mubende only, reason being it has population engaged but we shall do analysis for other
Mubende_only <-artisanal_data%>%
  filter(Location=="Mubende")

# Visualizing the mining locations
mubende_map+ geom_point(data = Mubende_only,
                    aes(x = Longitude, y = Latitude),
                    color ="red")+
  ggtitle("Gold Mines Sites in Mubende")

# Visualizing the mining locations based on the number of people mining
mubende_map+ geom_point(data = Mubende_only,
                        aes(x = Longitude, y = Latitude,size = Total),
                        color ="red")+
  ggtitle("Gold Mine sites in Mubende with Respect to People Mining")

# general plotting of all the mines
# First the map of Uganda, just taking Mubende as a central place but Nakasongola would be perfect
ugandaMap <- get_map(location = c(lat=0.5773, lon =31.5370),
                      zoom = 7,
                      maptype = "toner-2011")

uganda_map <- ggmap(ugandaMap)
uganda_map
#Visualizing the mines
uganda_map+ geom_point(data = artisanal_data,
                        aes(x = Longitude, y = Latitude, color = Commodity))+
  ggtitle("Artisanal Mines areas in Uganda")