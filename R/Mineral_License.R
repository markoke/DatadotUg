# Libraries 
library(dplyr)
library(magrittr)

#  Reading the data
mineral_License <- read.csv("data/known-issues-with-mineral-licences-in-uganda---2012---1.csv")

# Checking for the
str(mineral_License)

# Checking for the first 5 row of the data
head(mineral_License, 5)
# Its to altered, using the View method to see how the data is
View(mineral_License)

# subsetting others columns without the EL because its just an id and no interesting analysis in it
mineral_License <- mineral_License[ , 2:5]

# Changing column names
colnames(mineral_License) <- c("License_Name","Application_Date","Grant_Date","Comment")

# creating a column containing a number of days for each license to be granted
mineral_License %>%
  mutate(Days_to_Grant = as.Date(mineral_License$Grant_Date,
                                 format = "%d/%m/%y")-
                         as.Date(mineral_License$Application_Date,
                                 format = "%d/%m/%y"))