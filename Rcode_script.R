# Libraries
library(tidyr)
library(magrittr)
library(dplyr)
library(ggplot2)

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

# Selecting only gross revenue over the years
Gross_Revenue <-rev_collections %>% 
  select(Revenue.Collections,Year,Amount_Collected) %>%
  filter(Revenue.Collections == "Gross Revenues")

# Visualizing the revenue collected
 ggplot(data = Gross_Revenue, aes(x=Year, y= Amount_Collected)
      )+
   geom_bar(colour = "orange",
            stat ="identity",
            fill = "red")+
   coord_flip() +
   ggtitle(paste("Gross income Amounts collected over Years"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 
 # Selecting and visualizing value added taxes
 VAT <- rev_collections %>%
   select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Revenue.Collections == " -Cigarettes"|
            Revenue.Collections == " -Beer"|
            Revenue.Collections == " -Spirit/Waragi"|
            Revenue.Collections == " -Soft Drinks"|
            Revenue.Collections == " -Sugar"|
            Revenue.Collections == " -Bottled Water"|
            Revenue.Collections == " -Cement"|
            Revenue.Collections == " -Electricity"|
            Revenue.Collections == " -Water"|
            Revenue.Collections == " -Other Goods and services"|
            Revenue.Collections == " -Phone Talk time"
            )
 # Visualizing VAT using a line graph
 ggplot(data = VAT, aes(x=Year,
                        y= Amount_Collected, 
                        fill = Revenue.Collections)
 )+
   geom_bar(stat ="identity",
            position = position_dodge()
             )+
   coord_flip()+
   ggtitle(paste("VAT Amounts collected over Years"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 
 # dividing the dataset into 2 for easy visualizations and visualizing only btn 2005 to 2017
 VAT_2005_17 <-VAT %>% select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Year == "X2005.06"|
          Year == "X2006.07"|
          Year == "X2007.08"|
          Year == "X2008.09"|
          Year == "X2009.10"|
          Year == "X2010.11"|
          Year == "X2011.12"|
          Year == "X2012.13"|
          Year == "X2013.14"|
          Year == "X2014.15"|
          Year == "X2015.16"|
          Year == "X2016.17"
          )
 # Visualizing VAT using a line graph
 ggplot(data = VAT_2005_17, aes(x=Year,
                        y= Amount_Collected, 
                        fill = Revenue.Collections)
 )+
   geom_bar(stat ="identity",
            position = position_dodge()
   )+
   coord_flip()+
   ggtitle(paste("Variations in VAT Amounts collected Between 2005-2017"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )