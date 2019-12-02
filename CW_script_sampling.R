set.seed(123)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(leaflet)
library(plotly)
library(class)
library(scatterplot3d)

source("plot_functions.R")

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

M <- NYAirbnb[,sapply(NYAirbnb, is.numeric)]
M <- M[complete.cases(M),]
corr_matr <- cor(M,method = "pearson")
corrplot(corr_matr, method = "color")




lon <- NYAirbnb$longitude
lat <- NYAirbnb$latitude
price <- NYAirbnb$price
plot_3d(lon,lat,price)

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


lon <- NYA_rand$longitude
lat <- NYA_rand$latitude
price <- NYA_rand$price
plot_3d(lon,lat,price)


# Generates subsets based on room_type
Entire_home <- training_data[training_data$room_type == 'Entire home/apt',] # Subset for Entire home/apt
Private_room <- training_data[training_data$room_type == 'Private room',] # Subset for Private Room
Shared_room <- training_data[training_data$room_type == 'Shared room',] # Subset for Shared Room

# ----------------------Plot by room type---------------#

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude
room_type<-training_data$room_type

scatter(longitude,latitude,training_data$room_type,"Room Type", "Room")

#------------------- Plots for Entire Home -------------------------#

price<-Entire_home$price
latitude<-Entire_home$latitude
longitude<-Entire_home$longitude

scatter(longitude, latitude, Entire_home$neighbourhood_group, "Entire Home", "Neighbourhood")
#------------------- Plots for Private Rooms -------------------------#

price<-Private_room$price
latitude<-Private_room$latitude
longitude<-Private_room$longitude

scatter(longitude,latitude,Private_room$neighbourhood_group,"Private Rooms", "Neighbourhood")

#------------------- Plots for Shared Rooms -------------------------#

price<-Shared_room$price
latitude<-Shared_room$latitude
longitude<-Shared_room$longitude

plot(longitude,latitude,col=Shared_room$neighbourhood_group, main = "Shared Rooms")
neighbourhood_leg()
scatter(longitude,latitude,Shared_room$neighbourhood_group,"Shared Rooms", "Neighbourhood")
#------------------- Plots for Price -------------------------#

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

scatter(longitude,latitude,heat.colors(price),"Price", "")

#-----------------------------Distribution Code----------------------#
mean_ng <- NYA_rand %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(mean(price), 2))

p <- dist(NYA_rand,mean_ng,~neighbourhood_group)
print(p)

ng_mean <- log_dist(NYA_rand, mean_ng, "Mean= £",~neighbourhood_group)
print(ng_mean)

median_ng <- NYA_rand %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(median.default(price),2))

ng_median <- log_dist(NYA_rand, median_ng, "Median= £",~neighbourhood_group)
print(ng_median)


mean_rt <- NYA_rand %>%
  group_by(room_type) %>%
  summarise(price = round(mean(price),2))

rt_mean <- log_dist(NYA_rand, mean_rt, "Mean= £", ~room_type)
print(rt_mean)

median_rt <- NYA_rand %>%
  group_by(room_type) %>%
  summarise(price = round(median.default(price),2))


rt_median <- log_dist(NYA_rand, median_rt, "Median= £", ~room_type)
print(rt_median)



