# Libraries
library(dplyr)
library(ggmap)
library(ggplot2)

# Drawing the map of uganda
nakasongolo = c(lat = "1.3490", lon= "32.4467")

# getting the map
central_map <- get_map(location = c(lat = 1.3490, lon = 32.4467), 
                       zoom = 7,
                       maptype = "roadmap")

# Visualizing the map 
ugandaMap <- ggmap(central_map)
ugandaMap

# Reading the metallic mineral dataset
metmin <- read.csv("data/metmin.csv")

# Checking the structure of the data
str(metmin)

# glimpse of the data
View(metmin)

# Subsetting to more important
metmin <- metmin[ , 4:15]

# visualizing the metallic minerals locations
ugandaMap+geom_point(data = metmin,
                 aes(x=Longitude_, y= Latitude_Y, color = Status))+
  ggtitle(paste("Metallic Mineral Deposits in Uganda"))+
  labs(caption = "Source of Data: Data.ug")

#
  
