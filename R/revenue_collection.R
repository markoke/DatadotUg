# Libraries
library(tidyr)
library(magrittr)
library(dplyr)
library(ggplot2)

# Importing the dataset
rev_collections <- read.csv("data/revenue-collections.csv")

# checking the structure of the dataset
str(rev_collections)

# seems there is a problem with the header. Viewing the datasets to see whats really the problem
View(rev_collections)
# there is a problem with the way the headers were made. lets remove the first row
# Deleting the title manually from excel. its the easiest
rev_collections <- read.csv("data/revenue-collections.csv", 
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

 # .. Excise Duty
 Excise_Duty <- rev_collections%>%
   select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Revenue.Collections =="Excise duty:")
#Visualizing Excise Duty results
 ggplot(data = Excise_Duty, aes(x=Year,
                                y= Amount_Collected 
                                )
 )+
   geom_bar(stat ="identity",
            position = position_dodge(),
            fill = "Red",
            colour = "orange"
   )+
   coord_flip()+
   ggtitle(paste("Excise Duty Amounts Collected Over Years"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 
 # Deeper into excise duty categories
 Excise_Duty_Categories <- rev_collections %>%
   select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Revenue.Collections == "e-Cigarettes"|
            Revenue.Collections == "e-Beer"|
            Revenue.Collections == "e-Spirit/Waragi"|
            Revenue.Collections == "e-Soft Drinks"|
            Revenue.Collections == "e-Sugar"|
            Revenue.Collections == "e-Bottled Water"|
            Revenue.Collections == "e-Cement"|
            Revenue.Collections == "e-Other Goods and services"|
            Revenue.Collections == "e-Phone Talk time"
   )
 
 #Visualizing Excise Year Categories 
 ggplot(data = Excise_Duty_Categories, aes(x=Year,
                        y= Amount_Collected, 
                        fill = Revenue.Collections)
 )+
   geom_bar(stat ="identity",
            position = position_dodge()
   )+
   coord_flip()+
   ggtitle(paste("Exicse Duty Amounts collected over Years"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 # High excise duty is got from beer and phone talk time respectively
 
  # .............Fees and licences
 # General Fees and Licences over Years
 General_Fees_and_L <- rev_collections %>% select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Revenue.Collections == "Fees and Licenses")
 # Visualizing general fees and licences
 ggplot(data = General_Fees_and_L, aes(x=Year,
                        y= Amount_Collected, 
                        fill = Revenue.Collections)
 )+
   geom_bar(stat ="identity",
            position = position_dodge()
   )+
   coord_flip()+
   ggtitle(paste("General Amount for Fees and Licenses collected over Years"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 # What happened in 2011 that there are many offences 
 
 # Deeper into the categories of fees
 General_fees_categories <- rev_collections%>%
   select(Revenue.Collections,
          Year,
          Amount_Collected)%>%
   filter(Revenue.Collections ==" -Fees & Licenses (Traffic Act)"|
            Revenue.Collections ==" -Drivers Permits"|
            Revenue.Collections ==" -Stamp duty & Embossing Fees"|
            Revenue.Collections =="Road User Charges"|
            Revenue.Collections =="Airport Service Tax"|
            Revenue.Collections =="Airport Service Tax"
            )
 # Visualizing the categories
 # Visualizing general fees and licences
 ggplot(data = General_fees_categories, aes(x=Year,
                                       y= Amount_Collected, 
                                       fill = Revenue.Collections)
 )+
   geom_bar(stat ="identity",
            position = position_dodge()
   )+
   coord_flip()+
   ggtitle(paste("General Amount for Fees and Licenses with Categories"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 # We clearly see now that 2011-2012 had a high ammount of Stamp duty &Embosing Fees
 # Possibly that could be the reason it does not follow the increasing trends over years
 # Currently Drivers Permits is promising to have a high increase
 # Generally more fees are collected from Traffic Act over years though in 2011-12 and 2016-17 is where Stamp duty & Embossing Fees were higher
 
 #............ Domestic Taxes
 
 Domestic_Taxes <- rev_collections %>%
   select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Revenue.Collections == "Domestic Taxes")
 
 # Visualizing the Domestic Tax collected
 ggplot(data = Domestic_Taxes, aes(x=Year, y= Amount_Collected)
 )+
   geom_bar(colour = "orange",
            stat ="identity",
            fill = "red")+
   coord_flip() +
   ggtitle(paste("Gross Domestic Taxes Amounts collected over Years"))+
   labs(
     caption = "Source of Data: Data.Ug"
   )
 # There has been higher increase in revenue from one year to next starting from 2003-04 apart from 2012-13 where the increase was low
 
 #.. Direct Domestic Taxes
 D_Domestic_Taxes <- rev_collections %>%
   select(Revenue.Collections,Year,Amount_Collected)%>%
   filter(Revenue.Collections == "Direct Domestic Taxes")
 
 ggplot( data = D_Domestic_Taxes, 
         aes(x = Year, 
             y= Amount_Collected
            ))+
   geom_bar(colour = "orange",
            stat = "identity",
            fill = "red")+
   coord_flip()+
   ggtitle("Direct Domestic Taxes Collected Over Years")+
   labs(caption = "Source of Data: Data.ug")
 # Follows the same trend as that of general Domestic Taxes
 
 # Breaking Down the Direct Domestic Taxes into Categories
 D_Domestic_Categories <- rev_collections%>%
   select(Revenue.Collections,Year, Amount_Collected)%>%
   filter(Revenue.Collections == " -PAYE"|
            Revenue.Collections == " -Corporation Tax"|
            Revenue.Collections == " -Other Income Tax"|
            Revenue.Collections == " -Withholding Tax"|
            Revenue.Collections == " -Tax on Bank Interest"|
            Revenue.Collections == " -Casino and Lottery Tax"|
            Revenue.Collections == " -Agricultural products"|
            Revenue.Collections == "Un-allocated Revenue")
 
 # Visualizing the Direct Domestic Taxes 
 # Visualizing general fees and licences
 ggplot(data = D_Domestic_Categories, aes(x=Year,
                                            y= Amount_Collected, 
                                            fill = Revenue.Collections)
 )+
   geom_bar(stat ="identity",
            position = position_dodge()
   )+
   coord_flip()+
   ggtitle(paste("Direct Domestic Taxes Collected over Years with Categories"))+
   labs(
     caption = "Source of Data: Data.Ug"
     )
 # More Taxes is collected through PAYE, there is a sharp increase of PAYE as years increases from 2003-04
 # Either many people are getting jobs as years increase or government is increasing Taxes
 
 #.............International Trade
 International_trade <- rev_collections %>%
      select(Revenue.Collections,Year,Amount_Collected)%>%
      filter(Revenue.Collections == "Taxes on International Trade")
 # Visualizing the International Trade Tax collected
    ggplot(data = International_trade, 
           aes(x=Year, y= Amount_Collected)
              )+
      geom_bar(colour = "orange",
               stat ="identity",
               fill = "red")+
      coord_flip() +
      ggtitle(paste("Internation Trade Taxes Amounts collected over Years"))+
      labs(
          caption = "Source of Data: Data.Ug"
        )
    
    #Internal trade categories
  International_Trade_Categories <- rev_collections%>%
       select(Revenue.Collections,Year, Amount_Collected)%>%
       filter(Revenue.Collections == " -Petroleum duty"|
                 Revenue.Collections == " -Import duty"|
                 Revenue.Collections == " -Excise duty"|
                 Revenue.Collections == " -Surcharge on Used Imports"|
                 Revenue.Collections == " -VAT on Imports"|
                 Revenue.Collections == " -Withholding Taxes"|
                 Revenue.Collections == " -Temporary Road Licenses"|
                 Revenue.Collections == " -Commission on Imports"|
                 Revenue.Collections == " -Re-exports levy"|
                 Revenue.Collections == " -Hides & Skins levy/Exports Levy")
 # Visualizing general fees and licences
    ggplot(data = International_Trade_Categories, 
           aes(x=Year,
               y= Amount_Collected,                                           
               fill = Revenue.Collections)
              )
      geom_bar(stat ="identity",
               position = position_dodge()
               )
      coord_flip()+
      ggtitle(
        paste("International trade Taxes Collected over Years with Categories"))+
      labs(
           caption = "Source of Data: Data.Ug"
          )
   
