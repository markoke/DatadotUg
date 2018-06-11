# Libraries
library(tidyr)
library(magrittr)

# Importing the dataset
rev_collections <- read.csv("revenue-collections.csv")

# checking the structure of the dataset
str(rev_collections)

# seems there is a problem with the header. Viewing the datasets to see whats really the problem
View(rev_collections)
# there is a problem with the way the headers were made. lets remove the first row
# Deleting the title manually from excel. its the easiest
rev_collections <- read.csv("revenue-collections.csv", 
                            na.strings = "NA",
                            stringsAsFactors = F)

str(rev_collections)

# Making the data tidy using the gather method
rev_collections <- rev_collections %>% gather('X1991.92':'X2016.17', 
                           key = "Year", 
                           value = "Amount_Collected",
                           na.rm = T)


