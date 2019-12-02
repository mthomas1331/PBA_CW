# clears all objects in "global environment"
rm(list=ls())
# clears the console area
cat("\014")

# install relevant libraries
#install.packages("tensorflow")

#install.packages("neuralnet ")

#install.packages("boot")
#install.packages("plyr")

# load library
library(neuralnet)
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
library(boot)
library(plyr)
library(mlr)
library(data.table)
library(caret)
library(neuralnet)
library(boot)
library(plyr)
library(matrixStats)
library(neuralnet)
library(tensorflow)
library(glmnet)

library(nnet)

library(NeuralNetTools)

library(MASS)

library(ISLR)

library(caTools) # sample.split

library(boot) # cv.glm

library(faraway) # compact lm summary "sumary" function

library(caret) # useful tools for machine learning

library(corrplot)
library(neuralnet)

library(NeuralNetTools)

library(nnet)

library(RSNNS)

library(clusterGeneration)

library(devtools)

library(caret)

library(caret)
library(dplyr)
library(lattice)
library(ggplot2)
library(parallel)
library(doParallel)

data(neuraldat)





#CONSTANTS - used below 
NN_MODEL <- "NN_model_2.rds"
NN_MODEL_ORG <-  "NN_model_1.rds"
NN_MODEL_3 <- "NN_model_3.rds"
NN_MODEL_10 <- "NN_model_10.rds"
NN_MODEL_TEST <-  "NN_model_test.rds"

set.seed(123)

ANN_neural_network<-function(dataset){
  #predictor variable must scaled data for neural network 
  #glimpse(dataset)
  #print(paste("number of data = ", nrow(dataset)))
  dataset.scaled <- as.data.frame(scale(dataset))
  #glimpse(dataset.scaled)
  min.price <- min(dataset$NYA_dataset.price)
  #print(min.price)
  max.price <- max(dataset$NYA_dataset.price)
  #print(max.price)
  dataset.scaled$NYA_dataset.price <- scale(dataset$NYA_dataset.price, center = min.price, scale = max.price - min.price)
  #glimpse(dataset.scaled)
  #Train-test split 
  #index<-sample(1:nrow(dataset),round(0.70*nrow(dataset)))
  dataset.split <- sample.split(dataset$NYA_dataset.price, SplitRatio = 0.7)
  dataset.train.scaled <- dataset.scaled[dataset.split, ]
  dataset.test.scaled <- dataset.scaled[!dataset.split, ]
  #print(dataset.train.scaled)
  print("~~~~~~~")
  #print(dataset.test.scaled)
  
  train_names <- names(dataset.train.scaled)
  #print(train_names)
  #neuralnet library doesn't accept ~. notation so formula is used
  dataset.formula <- as.formula(paste("NYA_dataset.price ~", paste(train_names[!train_names %in% "NYA_dataset.price"], collapse = " + ")))
  set.seed(123)
  
  #saveRDS(NN_model, "NN_model_2.rds") # code to save model to save time
  if (file.exists(NN_MODEL_ORG)) {
    print("Loading neural network model file")
    #load(NNMODEL)
    dataset.nn.5.3 <- readRDS(NN_MODEL_ORG)
  } else {
    ##Neural network preforms here 
    print("START NEURAL NETWORK")
    dataset.nn.5.3 <- neuralnet(dataset.formula, data = dataset.train.scaled, hidden = 3, linear.output = FALSE, err.fct = "sse")
    print("END NEURAL NETWORK")
    saveRDS(dataset.nn.5.3, "NN_model_1.rds") #code to save model to save time
  }
  
  #Performance metrics 
  plot(dataset.nn.5.3)
  
  dataset.5.3.preds.scaled <- neuralnet::compute(dataset.nn.5.3, dataset.test.scaled)
  dataset.price.unscaled <- (dataset.test.scaled$price) * (max.price - min.price) + min.price
  dataset.5.3.preds <- dataset.5.3.preds.scaled$net.result * (max(dataset$NYA_dataset.price) -
                                                                min(dataset$NYA_dataset.price)) + min(dataset$NYA_dataset.price)
  
  #glimpse(dataset.5.3.preds)
  print(dataset.price.unscaled)
  #Calculate MSE
  #dataset.MSE <- reg.measures(dataset.5.3.preds, dataset.price.unscaled)
  MSE_value <- sum((dataset.price.unscaled - dataset.5.3.preds)^2)/nrow(dataset.test.scaled)
  print(sum(dataset.price.unscaled - dataset.5.3.preds))
  print("#########")
  print(paste("MSE = ", MSE_value))
  #dataset.MSE <- data.frame(dataset.5.3.preds, dataset.price.unscaled)
  plot(dataset.test.scaled$NYA_dataset.price, dataset.5.3.preds, 
       col="red", main = "Real test vs predicted ",
       pch=1, cex=0.7, xlab = "actual value", ylab = "predicted value")
  #line.fit <- lm(dataset.formula, data = dataset.train.scaled)
  #abline(line.fit)
  abline(0,1,lwd=2)
  
  # #cross-validation using caret
  # print("Caret model cross-validation")
  # model.nn <- caret::train(NYA_dataset.price~., data = dataset.train.scaled, method = "nnet", preProc = c("center", "scale"))
  # print(model.nn$results)
  # print("CROSS VALIDATION WITH CARET END")
  # 
  # #fast cross validation 
  # set.seed(123)
  # lm.fit <- glm(NYA_dataset.price~., data = dataset)
  # cross_val<- cv.glm(dataset, lm.fit, K = 10)$delta[1]
  # print("Cross_validation single result:")
  # print(cross_val)
  # cv.error<- NULL
  # k<-10
  # pbar<- create_progress_bar('text')
  # pbar$init(5)
  # for (i in 1:k) {
  #   index <- sample(1:nrow(dataset), round(0.7 * nrow(dataset)))
  #   train.cv <- dataset.scaled[index,]
  #   test.cv <- dataset.scaled[-index,]
  #   nn <- neuralnet(dataset.formula, data = train.cv, hidden = 3, linear.output = F)
  #   pr.nn <- neuralnet::compute(nn, test.cv[,1:4])
  #   pr.nn <- pr.nn$net.result * (max(dataset$NYA_dataset.price) - min(dataset$NYA_dataset.price)) + 
  #     min(dataset$NYA_dataset.price)
  #   test.cv.r <- (test.cv$NYA_dataset.price) * (max(dataset$NYA_dataset.price) - min(dataset$NYA_dataset.price)) + 
  #     min(dataset$NYA_dataset.price)
  #   cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
  #   pbar$step()
  # }
  # print("Mean Cross-Validation results")
  # mean_cv <- mean(cv.error)
  # print(mean_cv)
  # boxplot(cv.error, xlab = "MSE CV", col = 'cyan', border = 'blue', 
  #         names = 'CV error (MSE)', main = 'CV error (MSE) for NN', horizontal = TRUE)
  # 
}

test_NN <- function(dataset) {
  
  index <- createDataPartition(dataset$price, p = 0.7, list = FALSE)
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
 
  #saveRDS(NN_model, "NN_model_2.rds") # code to save model to save time
  # if (file.exists(NN_MODEL)) {
  #   print("Loading neural network model file")
  #   #load(NNMODEL)
  #   NN_model <- readRDS(NN_MODEL)
  # } else {
  #   print("Start neural network")
  #   NN_model <- neuralnet(price ~ latitude + longitude + room_type + minimum_nights + availability_365,
  #   trainNN, hidden = 3, linear.output = F)
  #   print("END of neural network")
  #   saveRDS(NN_model, "NN_model_2.rds") #code to save model to save time
  # }

  if (file.exists(NN_MODEL_TEST)) {
    print("Loading neural network model file")
    #load(NNMODEL)
    NN_model <- readRDS(NN_MODEL_TEST)
  } else {
    print("Start neural network")
    NN_model <- neuralnet(price ~ latitude + longitude + minimum_nights + availability_365,
                          trainNN, hidden = 3, linear.output = F, err.fct = "sse")
    print("END of neural network")
    saveRDS(NN_model, "NN_model_test.rds") #code to save model to save time
  }
  
  plot(NN_model)
  predict_NN <- neuralnet::compute(NN_model, testNN[,c(1:2,5:6)])
  unscaled_predict_NN<- predict_NN$net.result * (max(dataset$price) - min(dataset$price)) + 
    min(dataset$price)
  
  ##plot prediction and test values 
  plot(test_data$price, unscaled_predict_NN, col = 'blue', pch = 4, ylab = "Prediction result", xlab = "Test value")
  abline(10,1,lwd=2)
  RMSE_NN <- sqrt(sum(test_data$price - unscaled_predict_NN)^2) / nrow(test_data)
  print(paste("ROOT MEAN SQUARE ERROR: " ,RMSE_NN)) # Root MEAN SQUARE ERROR 
  
  #Cross validation to find best model 
  set.seed(123)
  dataset_RMSE <- glm(price~., data = dataset)
  cv_RMSE <- cv.glm(dataset, dataset_RMSE, cost = RMSE, K = 10)$delta[1]
  print(paste("CROSS_VALIDATION RMSE: ", cv_RMSE))
  
  print(summary(NN_model))
  #Fast cross validation 
  
  
}

main<-function(){
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
#glimpse(NYA_dataset) #should be 6 columns at this point 
names_to_use <- c("room_type")
NYA_dataset[names_to_use]<-map(NYA_dataset[names_to_use], as.factor)

###Neural network 1 
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
#checkNN<-test_NN(NYA_numeric) #Code here for best neural network 

#END OF ALTERNTIVE TESTING ~~~~~~~~~~~~~~~~~~~~~~~~~~~
NYA_dataset[names_to_use]<-NULL
#glimpse(NYA_dataset)

#print(names(NYA_dataset))
price<-data.frame(NYA_dataset$price)
#glimpse(price)
NYA_dataset[c("price")]<-NULL
#glimpse(NYA_dataset)
NYA_normalise<-data.frame(NYA_dataset,price)

NeuralNetwork<-ANN_neural_network(NYA_normalise) # Original neural network 
}

main()
