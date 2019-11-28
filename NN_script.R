# clears all objects in "global environment"
rm(list=ls())
# clears the console area
cat("\014")

set.seed(123)

#install.packages("keras")
library(tidyverse)
library(ggthemes)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(corrplot)
library(leaflet)
library(kableExtra)
library(RColorBrewer)
library(plotly)
library(keras)
library(data.table)
library(mltools)
#install_keras()

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
NYA_dataset<-NYA_rand
#Reducing number of columns we need to process
names_to_delete<-c("name", "host_name", "neighbourhood_group", "neighbourhood", "calculated_host_listings_count")
NYA_dataset[names_to_delete] <- NULL
glimpse(NYA_dataset) #should be 6 columns at this point 
names_to_use <- c("room_type")
NYA_dataset[names_to_use]<-map(NYA_dataset[names_to_use], as.factor)
#Hot one encoding the room types
encoding_onehot<-one_hot(as.data.table(NYA_dataset$room_type))
encoding_onehot<-setDF(encoding_onehot)
NYA_dataset<- data.frame(NYA_dataset,encoding_onehot)
#glimpse(NYA_dataset)
NYA_dataset[names_to_use]<-NULL
#glimpse(NYA_dataset)

#NYA_normalise<- as.data.frame(lapply(NYA_rand[names_to_use], normalize))




normalise<-function(){
  
}




