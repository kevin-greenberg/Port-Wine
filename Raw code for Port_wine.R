#Raw code for Port_wine.rmd
  


library(tidyverse)
library(dplyr)
library(DT)
library(knitr)
library(ggmap)
library(ggalt)
library(leaflet)
library(shiny)
library(ggpubr)



wine <- read.csv(url("https://raw.githubusercontent.com/JackStat/PracticalDataScience/master/data/winemag-data-130k-v2.csv"))




#We have a dataset of nearly 130,000 different wines with their ratings, price, tasters, country of origin, variety and much more. That is a lot of wine to look at so we are going to narrow it down to wines from one of my favorite places "Portugal."


WP <- filter(wine, country == 'Portugal')
dim(WP)


# Using `dim(df)` we can see that there are 5691 different wines, and 92 varieties. If we put all 92 into a bar graph it's a little messy, so we are going to ignore the varieties that have a less than 20 wines in them. Now let's take a look at the different varieties of wine Portugal has to offer. 


PV <- WP %>%
  group_by(variety) %>%
  summarise(count = n()) %>%
  filter(count > 20)

ggplot(PV, aes(x=factor(variety), y=count, fill = variety)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(label=count, y = count), size=3) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank() ,
        axis.ticks.x=element_blank()) +
  theme(legend.key.size = unit(0.2, "cm")) +
  guides(fill=guide_legend(ncol=2,byrow=TRUE)) +
  ggtitle("Varities of Wine in Portugal")


# There's a lot of wines there. I could just show you a table if I used `kable(WP)`, but what's the fun in that, and it'd be a lot of scrolling. With `DT::datable(WP)` you can search anything in the table.


SumP <- WP %>%
  group_by(variety) %>%
  summarise(
    count = n(),
    Average_Price = mean(price), na.rm =TRUE,
    Average_Points = mean(points, na.rm = TRUE)
  )

SumP <- SumP[,-4]
SumP <- rename(SumP, c(`Variety of wine` = variety, `Count of Wineries that produce the variety` = count, `Average Price (US $)` = Average_Price, `Average Points` = Average_Points))


DT::datatable((SumP), 
              class = 'cell-border stripe', 
              caption = 'Table 1: This is a table of all the of wine in Portugal, with the average price and points by variety.')



# Now lets look at the best one varieties...Port. From the Varities of Wine in Portugal graph  above we can tell that we have 607 different ports.

Port <- filter(WP, variety == 'Port') 
Port <- Port [, c(3,4,5,6,14)]
DT::datatable((Port), options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '500px', targets = 3)),
  pageLength = 5),
  class = 'cell-border stripe',
caption = 'Table 2: This is a table of all the Port wine.')



 
#Here we have a chart of their price by points. 


ggplot(Port, aes(x = points, y = price)) +
geom_point(position = 'jitter', shape=42, size=10) +
  geom_smooth(method='lm',formula=y~x, se = FALSE) +
  theme_bw()+
  ggtitle("Price by Points of Port")
 
#Below you can see the correlation of price by points.

cor(Port$price, Port$points, use = "complete.obs")
  
 

# If we look at the Port by the winery we can tell there are 97 different wineries, by using `wine <- filter(wine, variety == 'Port') %>% group_by(winery) %>% summarise(count = n())`
 

# Now it be great to be able to taste wines from all these different places, but let's narrow the Ports down to some of winery I've tasted.




selectPorts <- filter(Port,grepl('Broad|Church|Dow|Ferr|Fonseca|do Noval|Warre|Taylor|Kopke|Sand', winery)
             )
ggplot(selectPorts, aes(x=winery, fill=winery))+
  geom_bar(width = 1) +
  coord_polar(theta = "x") +
  theme_dark() 
 

 
# As you can see Taylor Flagate and Kopke have the most Ports in this dataset.
# Now Lets move away from the data and go look where these Ports come from.

leaflet() %>%
  setView(lng= -7.5441, lat = 41.17, zoom = 10) %>%
  addTiles() %>%
  addMarkers(lng =-7.554869499999995, lat = 41.1681615, popup = "Sandeman") %>%
addMarkers(lng =-7.5434, lat = 41.1873, popup = "Churchill's") %>%
  addMarkers(lng =-7.580222999999933, lat = 41.17443799999999, popup = "Taylor Fladgate") %>%      
  addMarkers(lng =-7.5957965999999715, lat = 41.15083540000001, popup = "Broadbent") 



 





