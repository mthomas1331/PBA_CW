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
#install.packages("class")
#install.packages("gmodels")
library(class)
library(gmodels)
library(caret)
#additional R scripts here 
source("functions.R")
set.seed(123)

#Constants defined here 
DATASET_FILENAME = "AB_NYC_2019.csv"
AVAILABILITY_365 = "availability_365"
NEIGHBOURHOOD_GROUP = "neighbourhood_group"
ENTIRE_HOME = "Entire home/apt"
PRIVATE_ROOM = "Private room"
SHARED_ROOM = "Shared room"
#Value Constants
DATA_SPLIT = 3
START_VALUE = 1
K_NEIGHBOUR = 3

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
  print(names(train_dataset))
  
  #plotting_function(dataset = processed_dataset)
  entire_home<-train_dataset[train_dataset$room_type == ENTIRE_HOME,] #Subset data frame for entire home
  private_room<-train_dataset[train_dataset$room_type == PRIVATE_ROOM,] #Subset data frame for private room
  shared_room<-train_dataset[train_dataset$room_type == SHARED_ROOM,] #Subset data frame for shared room
  #------------------- Plots for Private Rooms -------------------------# 
  #print(names(entire_home))
  training_data<-private_room[1:1000, c("latitude","longitude", "price")]
  testing_data<-private_room[-(1:1000), c("latitude","longitude", "price")]
  training_labels<-private_room[1:1000, c(NEIGHBOURHOOD_GROUP)]
  testing_labels<-private_room[-(1:1000), c(NEIGHBOURHOOD_GROUP)]
  
  price<-training_data$price
  latitude<-training_data$latitude
  longitude<-training_data$longitude
  # print(latitude)
  # print("~~~~~~~~~~~")
  # print(longitude)
  # print("~~~~~~~~~~~")
  # print(price)
  # print("~~~~~~~~~~~")
  
  plotting_function(latitude,longitude,price, private_room)
  
  #scatterplot3d(latitude,longitude, price, main="Test 1")
  
  # Plot by Location divided in Neighbourhood groups
 # plot(longitude,latitude,col=private_room$neighbourhood_group, main = "Neighbourhood groups") 
  #legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch=1, col=c("Red","Green","Blue","Cyan","Black"))
       
  #Testing with KNN 
 # print(names(train_dataset))

  knn_function(training_data,testing_data,training_labels,testing_labels,private_room)
  
}

preProcessingData<-function(dataset){
  include_pos<-which(dataset[,ncol(dataset)]!=0)
  avail_dataset<-dataset[include_pos,]
  rand_dataset<-avail_dataset[order(runif(nrow(avail_dataset))),]
  upper_limit<-nrow(rand_dataset)/DATA_SPLIT
  train_dataset<-rand_dataset[START_VALUE:upper_limit,]
  #write.csv(train_dataset, file = "PREPROCESSED_AIRBNB.csv")
  return(train_dataset)
}

plotting_function<-function(latitude, longitude, price, private_room){
  scatterplot3d(latitude,longitude, price, main="Test 1")
  
  # Plot by Location divided in Neighbourhood groups
  plot(longitude,latitude,col=private_room$neighbourhood_group, main = "Neighbourhood groups") 
  legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch=1, col=c("Red","Green","Blue","Cyan","Black"))
}

knn_function<-function(training_data,testing_data,training_labels,testing_labels,dataset){
  
  test_pred<-knn(train = training_data, test = testing_data,cl = training_labels,k = K_NEIGHBOUR)
  showResults<- data.frame(test_pred,testing_labels)
  names(showResults)<- c("Predicted", "Test Values")
  print(showResults)
  CrossTable(x=testing_labels, y=test_pred, prop.chisq = FALSE)
  
  ###Training models 
  #print(names(getModelInfo()))
  index<-createDataPartition(dataset$neighbourhood,p=0.75,list=FALSE)
  dataset.train<-dataset[index,]
  dataset.test<-dataset[-index,]
  print(names(dataset.train))
  model_knn<-train(dataset.train[,c("latitude","longitude","price"), dataset.train[,c(NEIGHBOURHOOD_GROUP)]], method = 'knn', preProcess = c) 
  predictions<-predict(object=model_knn,dataset.test[,c("latitude","longitude","price")])
  table(predictions)
  #Confusion matrix
  confusionMatrix(predictions,dataset.test[,c(NEIGHBOURHOOD_GROUP)])
      
}


main()
print("end")
