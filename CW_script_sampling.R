set.seed(123)

library("ggplot2")
library(dplyr)
library(tidyverse)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(leaflet)
library(plotly)

NYAirbnb <- read.csv("AB_NYC_2019.csv", encoding = "UTF-8", stringsAsFactors = F, na.strings = c("")) # Reads the dataset

# Why use NA Ref: https://www.statmethods.net/input/missingdata.html
#NYAirbnb[, 16][NYAirbnb[, 16] == 0] <- NA # Sets all the 0 values in latitude to NA

#NYAirbnb <- NYAirbnb[complete.cases(NYAirbnb[,16]),] # Gets rid of the rows with latitude = NA

names_to_delete <- c("id","host_id","number_of_reviews","last_review","reviews_per_month")
NYAirbnb[names_to_delete] <- NULL 
NYAirbnb<-NYAirbnb %>% filter(availability_365>0)
NYAirbnb<-NYAirbnb %>% filter(price>0)

names_to_factor<-c("host_name","neighbourhood_group","neighbourhood","room_type")
NYAirbnb[names_to_factor]<-map(NYAirbnb[names_to_factor], as.factor)

#-------------Getting rid of outliers--------------------#
lq = quantile(NYAirbnb$price)[2] #lower quartile
uq = quantile(NYAirbnb$price)[4] #upper quartile

iqr = uq - lq #interquartile range

upperlim = uq + (iqr*1.5)
lowerlim = lq - (iqr*1.5)

NYAirbnb<-NYAirbnb[!(NYAirbnb$price < lowerlim),]
NYAirbnb<-NYAirbnb[!(NYAirbnb$price > upperlim),]

NYAirbnb <- NYAirbnb[sample(nrow(NYAirbnb)),] # Randomise order of dataframe
NYA_rand <- NYAirbnb[1:14000,]

NYA_rand<-NYA_rand %>% mutate(id = row_number())

training_data<-NYA_rand %>% sample_frac(.7) %>% filter(price > 0)
testing_data<-anti_join(NYA_rand, training_data
                        , by = 'id') %>% filter(price > 0)


# Generates subsets based on room_type
Entire_home <- training_data[training_data$room_type == 'Entire home/apt',] # Subset for Entire home/apt
Private_room <- training_data[training_data$room_type == 'Private room',] # Subset for Private Room
Shared_room <- training_data[training_data$room_type == 'Shared room',] # Subset for Shared Room

# ----------------------Plot by room type---------------#

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude
room_type<-training_data$room_type

plot(longitude,latitude, col=c(c(1,2,3),c(training_data$room_type)), main = "Room Type")
legend("topleft", legend=c("Entire Home","Private Rooms","Shared Rooms") , pch= 1, col=c("Red","Black","Green"), cex = 0.5, pt.cex = 1.5)


#------------------- Plots for Entire Home -------------------------#

price<-Entire_home$price
latitude<-Entire_home$latitude
longitude<-Entire_home$longitude

plot(longitude,latitude,col=Entire_home$neighbourhood_group, main = "Entire Homes")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"), cex = 0.5, pt.cex = 1.5)


#------------------- Plots for Private Rooms -------------------------#

price<-Private_room$price
latitude<-Private_room$latitude
longitude<-Private_room$longitude

plot(longitude,latitude,col=Private_room$neighbourhood_group, main = "Shared Rooms")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"), cex = 0.5, pt.cex = 1.5)


#------------------- Plots for Shared Rooms -------------------------#

price<-Shared_room$price
latitude<-Shared_room$latitude
longitude<-Shared_room$longitude

plot(longitude,latitude,col=Shared_room$neighbourhood_group, main = "Shared Rooms")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"), cex = 0.5, pt.cex = 1.5)

#------------------- Plots for Price -------------------------#

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

plot(longitude,latitude,col=heat.colors(price), main = "Price")

#-----------------------------Distribution Code----------------------#

airbnb_nh <- NYA_rand %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(mean(price), 2))

p <- ggplot(NYA_rand, aes(price)) + 
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.3, fill = "purple")  + 
  ggtitle("Distribution of price by neighbourhood groups") +
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  facet_wrap(~neighbourhood_group)

print(p)

ps <- ggplot(NYA_rand, aes(price)) + 
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.3, fill = "purple")  + 
  ggtitle("Distribution of price by neighbourhood groups",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = airbnb_nh,y = 2, aes(x = price + 90 , label = paste("Mean  = £",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group) + scale_x_log10() 



print(ps)
