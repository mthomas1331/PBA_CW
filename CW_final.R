# R script for group coursework 
# clears all objects in global environment
rm(list = ls())

print(paste("WORKING DIRECTORY: ", getwd()))

groupLibraries<-c("ggplot2", "dplyr", "tidyverse", "GGally", "ggExtra", "caret",
                  "glmnet", "leaflet", "plotly", "class", "scatterplot3d", "caret",
                  "corrplot", "keras", "data.table", "mltools", "boot", "plyr", "mlr", 
                  "neuralnet", "tensorflow", "nnet", "NeuralNetTools" 
                  )

library(pacman)
pacman::p_load(char = groupLibraries, install = TRUE, character.only = TRUE)

source("plot_functions.R")

#Pre-defined constants 
AIRBNB_FILENAME <- "AB_NYC_2019.csv"
ID <- "id"
HOST_ID <- "host_id"
NUMBER_OF_REVIEWS <- "number_of_reviews"
LAST_REVIEW <- "last_review"
REVIEWS_PER_MONTH <- "reviews_per_month"
ROOM_TYPE <- "room_type"
HOST_NAME <- "host_name"
NEIGHBOURHOOD_GROUP <- "neighbourhood_group"
NEIGHBOURHOOD <- "neighbourhood"

DATA_SPLIT <- 0.7
NN_MODEL_TEST <- "NN_model_test.rds"
NN_MODEL_5H <- "NN_model_5H.rds"
MODEL_NN_1 <- "Model_NN_1.rds"
MODEL_NN_2 <- "Model_NN_2.rds"

#This code has been adapted to suit this dataset 
#sources used here:  
#https://rpubs.com/julianhatwell/annr
#https://github.com/hanhanwu/Hanhan_Data_Science_Practice/blob/master/AI_Experiments/R_neural_network_basics.R
#https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
neural_network_2  <- function(dataset) {
  index <- createDataPartition(dataset$price, p = DATA_SPLIT, list = FALSE)
  train_data <- dataset[index,]
  glimpse(train_data)
  test_data <- dataset[-(index),]
  glimpse(test_data)
  max_val <- apply(dataset,2,max)
  min_val <- apply(dataset, 2, min)
  dataset_scaled <- as.data.table(scale(dataset, center = min_val, 
                                        scale = max_val - min_val))
  #print(summarizeColumns(dataset_scaled))
  #glimpse(dataset_scaled)
  
  trainNN <- dataset_scaled[index,]
  testNN <- dataset_scaled[-index,]
  
  set.seed(123)
  # code to save model to save time 
  if (file.exists(MODEL_NN_1)) {
    print("Loading neural network model file")
    #load(NNMODEL)
    NN_model <- readRDS(MODEL_NN_1)
    plot(NN_model)
  } else {
    print("Start neural network")
    NN_model <- neuralnet(price ~ latitude + longitude + room_type + minimum_nights + availability_365,
                          trainNN, hidden = 3, linear.output = F)
    print("END of neural network")
    saveRDS(NN_model, "Model_NN_1.rds") #code to save model to save time
    plot(NN_model)
  }
  #Another neural network - without room_type as a factor 
  if (file.exists(MODEL_NN_2)) {
    print("Loading neural network model file")
    NN_model_2 <- readRDS(MODEL_NN_2)
    plot(NN_model_2)
  } else {
    print("Start neural network")
    NN_model_2 <- neuralnet(price ~ latitude + longitude + minimum_nights + availability_365,
                          trainNN, hidden = 3, linear.output = F)
    print("END of neural network")
    saveRDS(NN_model_2, "Model_NN_2.rds") #code to save model to save time
    plot(NN_model_2)
  }
  
  #Performance measure for first neural network 
  plot(NN_model)
  predict_NN <- neuralnet::compute(NN_model, testNN[,c(1:3,5:6)])
  unscaled_predict_NN<- predict_NN$net.result * (max(dataset$price) - min(dataset$price)) + 
    min(dataset$price)
  
  ##plot prediction and test values 
  plot(test_data$price, unscaled_predict_NN, col = 'blue', pch = 4, ylab = "Prediction values", xlab = "Real values")
  abline(0,1,lwd=2)
  RMSE_NN <- sqrt(sum(test_data$price - unscaled_predict_NN)^2) / nrow(test_data)
  print(paste("ROOT MEAN SQUARE ERROR FOR NEURAL NETWORK: " ,RMSE_NN)) # Root MEAN SQUARE ERROR 
  
  #Performance measure for second neural network
  plot(NN_model_2)
  predict_NN_2 <- neuralnet::compute(NN_model_2, testNN[,c(1:2,5:6)])
  unscaled_predict_NN_2<- predict_NN_2$net.result * (max(dataset$price) - min(dataset$price)) + 
    min(dataset$price)
  
  ##plot prediction and test values 
  plot(test_data$price, unscaled_predict_NN_2, col = 'red', pch = 4, ylab = "Prediction values", xlab = "Real values")
  abline(0,1,lwd=2)
  RMSE_NN_2 <- sqrt(sum(test_data$price - unscaled_predict_NN_2)^2) / nrow(test_data)
  print(paste("ROOT MEAN SQUARE ERROR FOR NEURAL NETWORK 2: " ,RMSE_NN_2)) # Root MEAN SQUARE ERROR 
  
}

#kNN algorithm ###
#Code used from the source
#source - https://www.edureka.co/blog/knn-algorithm-in-r/
knn_model <- function(dataset) {
  set.seed(123)
  NYA_rand <- dataset
  NYA_rand.subset <- NYA_rand[c('price', 'latitude', 'longitude')]
  
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  
  NYA_rand.subset.n <- as.data.frame(lapply(NYA_rand.subset[,2:3], normalize))

  dat.d <- sample(1:nrow(NYA_rand.subset.n), size=nrow(NYA_rand.subset.n)*DATA_SPLIT, replace=FALSE)
  
  train.NYA_rand <- NYA_rand.subset[dat.d,]
  test.NYA_rand <-NYA_rand.subset[-dat.d,]
  
  train.NYA_rand_labels <- NYA_rand.subset[dat.d,1]
  test.NYA_rand_labels <- NYA_rand.subset[-dat.d,1]
  
  knn.10 <- knn(train=train.NYA_rand, test=test.NYA_rand, cl=train.NYA_rand_labels, k=4)
  #print(summary(knn.10))
  
  ACC.10 <- 100 * sum(test.NYA_rand_labels == knn.10) / NROW (test.NYA_rand_labels)
  
  i=1
  k.optm=1
  for (i in 1:50) {
    knn.mod <- knn(train=train.NYA_rand, test=test.NYA_rand, cl=train.NYA_rand_labels, k=i)
    k.optm[i] <- 100 * sum(test.NYA_rand_labels == knn.mod) / NROW (test.NYA_rand_labels)
    #k=i
    #cat(k, '=', k.optm[i], '\n')
  }
  
  plot(k.optm, type='b', xlab="K- Value", ylab="Accuracy level")
  
}

pre_processing_data <- function(dataset) {
  
  NYAirbnb <- dataset
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
  
  return(NYA_rand)
}

main <- function() {
  
  NYAirbnb <- read.csv(AIRBNB_FILENAME, encoding = "UTF-8", stringsAsFactors = F, na.strings = c("")) # Reads the dataset
  
  NYA_rand<-pre_processing_data(NYAirbnb)
  
  ##kNN algortihm 
  knn_model(NYA_rand)
  
  ###Neural Network code### 
  NYA_dataset<-NYA_rand
  #Reducing number of columns we need to process
  names_to_delete<-c("name", "host_name", "neighbourhood_group", "neighbourhood", "calculated_host_listings_count")
  NYA_dataset[names_to_delete] <- NULL
  #glimpse(NYA_dataset) #should be 6 columns at this point 
  

  #Using numeric to turn the rpom types to numeric values ~~~~~~~
  NYA_numeric <- NYA_dataset
  NYA_numeric[,3]<-as.numeric(NYA_numeric[,3]) - 1
  #glimpse(NYA_numeric)
  NYA_matrix <- as.matrix(NYA_numeric)
  #glimpse(NYA_matrix)
  dimnames(NYA_matrix) <- NULL
  #print(NYA_matrix)
  #NYA_matrix_norm <- tensorflow::normalize(NYA_matrix)
  #glimpse(NYA_matrix_norm)

  NN_second<-neural_network_2(NYA_numeric) #Code here for best neural network 
  
  #END OF ALTERNTIVE TESTING ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #~~~~CALL TO ORIGINAL NEURAL NETWORK FUNCTION~~~~~~~~~~~~~~~~#
  # names_to_use <- c("room_type")
  # NYA_dataset[names_to_use]<-map(NYA_dataset[names_to_use], as.factor)
  # 
  # NYA_dataset[names_to_use]<-NULL
  # 
  # price<-data.frame(NYA_dataset$price)
  # NYA_dataset[c("price")]<-NULL
  # NYA_normalise<-data.frame(NYA_dataset,price)
  # print("START NEURAL NETWORK 1")
  # NN_original<-neural_network(NYA_normalise) # Original neural network 
  # print("END OF NEURAL NETWORK 1")

}

# clears the console area 
cat("\014")

print("Start of program")
#Starts random numbers at the same sequence
set.seed(123)
main()
print("END of program")

### CODE ADAPTED FROM THESE SOURCES  - 
#https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
#https://rpubs.com/julianhatwell/
#https://github.com/hanhanwu/Hanhan_Data_Science_Practice/blob/master/AI_Experiments/R_neural_network_basics.R
#https://www.kaggle.com/scsaurabh/complete-analysis-of-airbnb-data-new-york-city
#https://www.kaggle.com/josipdomazet/mining-nyc-airbnb-data-using-r
