#Clear global obejcts 
rm(list = ls())
#clear the console area 
cat("\014")

#Libraries put here 
MYLIBRARIES<-c("caret",
               "outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "stats",
               "PerformanceAnalytics",
               "dplyr")

#install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

#additional R scripts here 
source("functions.R")
set.seed(123)

#Constants defined here 
DATASET_FILENAME = "AB_NYC_2019.csv"
AVAILABILITY_365 = "availability_365"
ENTIRE_HOME = "Entire home/apt"
PRIVATE_ROOM = "Private room"
SHARED_ROOM = "Shared room"
#Value Constants
DATA_SPLIT = 3
START_VALUE = 1

main<-function(){
  print("Inside main function !!!!!!")
  airbnb_dataset<-read.csv(DATASET_FILENAME)
  print(nrow(airbnb_dataset))
  print("########")
  # positions<-which(airbnb_dataset[,ncol(airbnb_dataset)]!=0)
  # extract_r1<-airbnb_dataset[positions,]
  # print(nrow(extract_r1))
  # print(extract_r1)
  print("~~~~~~~~~~~~~~~~~")
  processed_dataset<-preProcessingData(dataset = airbnb_dataset)
  #print(nrow(processed_dataset))
  #rand_dataset<-processed_dataset[order(runif(nrow(removed_dataset))),]
  #print(processed_dataset)
  train_dataset<-processed_dataset
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  
  
  #plotting_function(dataset = processed_dataset)
  entire_home<-train_dataset[train_dataset$room_type == ENTIRE_HOME,] #Subset data frame for entire home
  private_room<-train_dataset[train_dataset$room_type == PRIVATE_ROOM,] #Subset data frame for private room
  shared_room<-train_dataset[train_dataset$room_type == SHARED_ROOM,] #Subset data frame for shared room
  #------------------- Plots for Private Rooms -------------------------# 
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(entire_home)
  training_data<-private_room[1:1000, c("latitude","longitude", "price")]
  testing_data<-private_room[-(1:1000), c("latitude","longitude", "price")]
  
  price<-training_data$price
  latitude<-training_data$latitude
  longitude<-training_data$longitude
  # print(latitude)
  # print("~~~~~~~~~~~")
  # print(longitude)
  # print("~~~~~~~~~~~")
  # print(price)
  # print("~~~~~~~~~~~")
  
  scatterplot3d(latitude,longitude, price, main="Test 1")
  
  # Plot by Location divided in Neighbourhood groups
  plot(longitude,latitude,col=private_room$neighbourhood_group, main = "Neighbourhood groups") 
  legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , col=c("Red","Green","Blue","Cyan","Black"))
  

       
  
}

preProcessingData<-function(dataset){
  include_pos<-which(dataset[,ncol(dataset)]!=0)
  avail_dataset<-dataset[include_pos,]
  rand_dataset<-avail_dataset[order(runif(nrow(avail_dataset))),]
  upper_limit<-nrow(rand_dataset)/DATA_SPLIT
  train_dataset<-rand_dataset[START_VALUE:upper_limit,]
  print(nrow(train_dataset))
  return(train_dataset)
}

plotting_function<-function(dataset){
  # Generates subsets based on room_type
  #Entire_home <- NYA_rand[NYA_rand$room_type == 'Entire home/apt',] # Subset for Entire home/apt
  #Private_room <- NYA_rand[NYA_rand$room_type == 'Private room',] # Subset for Private Room
  #Shared_room <- NYA_rand[NYA_rand$room_type == 'Shared room',] # Subset for Shared Room
  
}
main()
print("end")
