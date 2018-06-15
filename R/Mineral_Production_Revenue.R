# Libraries
library(magrittr)
library(ggplot2)
library(dplyr)

# Loading the data
mineral_revenue <- read.csv("data/mineral-production--revenue.csv")

# Checking the structure of the data
str(mineral_revenue)

# Looking at the first 5 rows of the dataset
head(mineral_revenue, 5)

# Alternatively Viewing the dataset
View(mineral_revenue)

# Renaming the columns
# first retreiving the current column names
colnames(mineral_revenue)

colnames(mineral_revenue) <- c("Year", "Mineral_Name",
                              "Average_Price_Per_Tonne_in_UGX1000s",
                              "Quantity_in_Tonnes",
                              "Value_in_UGX1000s")

# Focusing on gold only
Gold_only <- mineral_revenue%>%
  filter(Mineral_Name == "Gold"|
           Mineral_Name == "Gold (kg)"|
           Mineral_Name == "Gold (Kg)*"
           )
# if you get the datase from data.ug, the above stript Retrieves only data between 1999 to 2012, Whats  wrong with other because they contain the word gold too
# It took me time to got it, Gold had on space after d but made all uniform to Gold
Gold_only <- mineral_revenue%>%
  filter(Mineral_Name == "Gold")

# A bar chart of revenue got from gold over years
ggplot(data = Gold_only, aes(x = Year, y = Value_in_UGX1000s))+
  geom_bar(stat = "identity",
           color = "red",
           fill = "gold")+
  ggtitle(paste("Amount of Revenue Collected from Gold Over Years"))+
  labs(caption = "Source of Data: Data.ug")
  
# A bar graph of Quantity of Gold produce
ggplot(data = Gold_only, aes(x = Year, y = Quantity_in_Tonnes))+
  geom_bar(stat = "identity",
           color = "red",
           fill = "gold")+
  ggtitle(paste("Quantity of Gold Mined over Years"))+
  labs(caption = "Source of Data: Data.ug")

# Line graph showing Quantity against revenue collected
ggplot(data = Gold_only, aes(x = Quantity_in_Tonnes, y = Value_in_UGX1000s))+
  geom_point()+
  geom_line()

# Nothing much in the line graph but confirms the two bars graphs
