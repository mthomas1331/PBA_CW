# clears all objects in "global environment"
rm(list=ls())
# clears the console area
cat("\014")

set.seed(123)
# install relevant libraries
#install.packages("keras")

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
  
  ##Neural network preforms here 
  print("START NEURAL NETWORK")
  dataset.nn.5.3 <- neuralnet(dataset.formula, data = dataset.train.scaled, hidden = 3, linear.output = FALSE)
  print("END NEURAL NETWORK")
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
       col="red", main = "Real vs predicted NN\ntwo hidden layers",
       pch=1, cex=0.7, xlab = "actual value", ylab = "predicted value")
  line.fit <- lm(dataset.formula, data = dataset.train.scaled)
  abline(line.fit)
  
  #cross-validation using caret
  print("Caret model cross-validation")
  model.nn <- caret::train(NYA_dataset.price~., data = dataset.train.scaled, method = "nnet", preProc = c("center", "scale"))
  #(model.nn)
  
  #fast cross validation 
  set.seed(200)
  lm.fit <- glm(NYA_dataset.price~., data = dataset)
  cross_val<- cv.glm(dataset, lm.fit, K = 10)$delta[1]
  print("Cross_validation single result:")
  print(cross_val)
  set.seed(450)
  cv.error<- NULL
  k<-10
  pbar<- create_progress_bar('text')
  pbar$init(5)
  for (i in 1:k) {
    index <- sample(1:nrow(dataset), round(0.7 * nrow(dataset)))
    train.cv <- dataset.scaled[index,]
    test.cv <- dataset.scaled[-index,]
    nn <- neuralnet(dataset.formula, data = train.cv, hidden = 3, linear.output = F)
    pr.nn <- neuralnet::compute(nn, test.cv[,1:4])
    pr.nn <- pr.nn$net.result * (max(dataset$NYA_dataset.price) - min(dataset$NYA_dataset.price)) + 
      min(dataset$NYA_dataset.price)
    test.cv.r <- (test.cv$NYA_dataset.price) * (max(dataset$NYA_dataset.price) - min(dataset$NYA_dataset.price)) + 
      min(dataset$NYA_dataset.price)
    cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    pbar$step()
  }
  print("Mean Cross-Validation results")
  mean_cv <- mean(cv.error)
  print(mean_cv)
  boxplot(cv.error, xlab = "MSE CV", col = 'cyan', border = 'blue', 
          names = 'CV error (MSE)', main = 'CV error (MSE) for NN', horizontal = TRUE)
  
}

linear_model<-function(dataset){
  dataset.scaled <- as.data.frame(scale(dataset))
  glimpse(dataset.scaled)

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
  
  #formula 
  train_names <- names(dataset.train.scaled)
  print(train_names)
  dataset.formula <- as.formula(paste("NYA_dataset.price ~", paste(train_names[!train_names %in% "NYA_dataset.price"], collapse = " + ")))
  
  fit_lm <- lm(dataset.formula, data = dataset.train.scaled)
  temp_test <- predict(fit_lm,dataset.test.scaled)
  plot(dataset.test.scaled[["latitude"]],temp_test, xlab = "latitude",ylab = "predicted")
  
  
  
  
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
#Hot one encoding the room types
#encoding_onehot<-one_hot(as.data.table(NYA_dataset$room_type))
#encoding_onehot<-setDF(encoding_onehot)
#NYA_dataset<- data.frame(NYA_dataset,encoding_onehot)
#glimpse(NYA_dataset)
NYA_dataset[names_to_use]<-NULL
#glimpse(NYA_dataset)

#print(names(NYA_dataset))
price<-data.frame(NYA_dataset$price)
#glimpse(price)
NYA_dataset[c("price")]<-NULL
#glimpse(NYA_dataset)
NYA_normalise<-data.frame(NYA_dataset,price)

#linearModel<-linear_model(NYA_normalise)
NeuralNetwork<-ANN_neural_network(NYA_normalise)
}

main()
