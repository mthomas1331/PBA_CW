# test_neural<-function(cereal_dataset){
#   sample_size<-0.6*nrow(cereal_dataset)
#   set.seed(80)
#   index<-sample(seq_len(nrow(cereal_dataset)), size = sample_size)
#   print(index)
#   
#   data_train<- cereal_dataset[index,]
#   data_test<- cereal_dataset[-index,]
#   
#   #normalisation of data 
#   maxC<-apply(cereal_dataset, 2, max) # 1 indicates rows in matrix
#   minC<-apply(cereal_dataset, 2, min) # 2 indcates columns in matrix
#   #glimpse(cereal_dataset)
#   print(paste("MAX: ", maxC, " MIN: ", minC))
#   scaled<-as.data.frame(scale(cereal_dataset, center = minC, scale = maxC - minC))
#   
#   #Train and test datasets for NN
#   trainNN<-scaled[index, ]
#   testNN<-scaled[-index, ]
#   
#   #Fit the neural network
#   set.seed(2)
#   glimpse(trainNN)
#   NN<-neuralnet(rating~calories + protein + fat + sodium + fiber, trainNN, hidden = 3,  linear.output = T)
#   plot(NN) # black lines show connections with weights. Weights are calculated using 
#   #back propagation. Blue line os the bias term.
#   
#   #Prediction using Neural network 
#   predict_testNN<-compute(NN, testNN[,c(1:5)])
#     #compute(NN, testNN[,c(1:5)])
#   predict_testNN<-(predict_testNN$net.result * (max(cereal_dataset$rating) - min(cereal_dataset$rating))) + min(cereal_dataset$rating)
#   
#   plot(data_test$rating, predict_testNN, col = 'blue', pch = 16, ylab = "predicted rating NN", xlab = "real rating")
#   abline(0,1)
#   
#   #Calculate ROOT MEAN SQUARE ERROR (RSME)
#   RMSE.NN<-(sum((data_test$rating - predict_testNN)^2) / nrow(data_test))^0.5
#   print(paste("RSME: ", RMSE.NN))
#   
#   #Initialise variables 
#   set.seed(50)
#   k<-100
#   RSME.NN<-NULL
#   List<-list()
#   
#   #fit neural network model within nested for loop 
#   for (j in 10:65) {
#     for (i in 1:k) {
#       index<-sample(1:nrow(cereal_dataset),j)
#       
#       trainNN<-scaled[index, ]
#       testNN<-scaled[-index, ]
#       data_test<-cereal_dataset[-index,]
#       
#       
#       NN<-neuralnet(rating~calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output = T)
#       plot(NN) # black lines show connections with weights. Weights are calculated using 
#       #back propagation. Blue line os the bias term.
#       
#       #Prediction using Neural network 
#       predict_testNN<-compute(NN, testNN[,c(1:5)])
#       predict_testNN<-(predict_testNN$net.result * (max(cereal_dataset$rating) - min(cereal_dataset$rating))) + min(cereal_dataset$rating)
#       
#       plot(data_test$rating, predict_testNN, col = 'blue', pch = 16, ylab = "predicted rating NN", xlab = "real rating")
#       abline(0,1)
#       
#       #Calculate ROOT MEAN SQUARE ERROR (RSME)
#       RMSE.NN<-(sum((data_test$rating - predict_testNN)^2) / nrow(data_test))^0.5
#       print(paste("RSME: ", RMSE.NN))
#     }
#     List[[j]] = RMSE.NN
#   }
#   
#   Matrix.RMSE<-do.call(cbind,List)
#   
# }
# 
# real_neural<-function(dataset){
#   
#   #Sample 70% of the data as training and 30% as testing
#   sample_size<-0.7 * nrow(dataset)
#   index<-sample(1:nrow(dataset),round(0.70*nrow(dataset)))
#   print(sample_size)
#   #glimpse(index)
#   set.seed(3)
#   max<-apply(dataset, 2, max)
#   min<-apply(dataset, 2, min)
#   #glimpse(apply(dataset,2,function(x) sum(is.na(x))))
#   #print(paste("MAX: ", max, " MIN: ", min))
#   normalised<-as.data.frame(scale(dataset, center = min, scale = max - min))
#   #glimpse(normalised)
#   #Train and Test for NN
#   #NN_train<-normalised[(1:sample_size), ]
#   #NN_test<-normalised[-(1:sample_size), ]
#   NN_train<-normalised[index, ]
#   NN_test<-normalised[-index, ]
#   #Fit the neural network 
#   set.seed(140)
#   lm_fit<-glm(price~., data = NN_train)
#   print(summary(lm_fit))
#   pr_lm <- predict(lm_fit, NN_test)
#   MSE_lm <- sum((pr_lm - NN_test$price)^2)/nrow(NN_test)
#   print(paste("MSE: ", MSE_lm))
#   print("~~~~~~~~~~~~~~~~~~~~~~")
#   #print(typeof(NN_test[,c(1:4)]))
#   #glimpse(NN_train)
#   NN_train_names<-names(NN_train)
#   formula_term<-as.formula(paste("price ~", paste(NN_train_names[!NN_train_names %in% "price"], collapse = " + ")))
#   NN<-neuralnet::neuralnet(formula_term,NN_train, hidden = 3, linear.output = T) 
#   #price ~ latitude + longitude + minimum_nights + availability_365
#   #plot(NN)
#   #glimpse(NN_test[,c(1:4)])
#   #glimpse(NN_test[1:4])
#   #NN<-data.matrix(NN, rownames.force = NA)
#   print(typeof(NN))
#   predictNN<-neuralnet::compute(NN,NN_test[1:5])
#   #glimpse(predictNN)
#   predictNN<-predictNN$net.result*(max(dataset$price)-min(dataset$price))+min(dataset$price)
#   testR<-(NN_test$price)*(max(dataset$price)-min(dataset$price)+min(dataset$price))
#   
#   MSE_NN<-sum((testR-predictNN)^2)/nrow(NN_test)
#   print(paste(MSE_NN))#MEAN SQUARED ERROR
#   
# }