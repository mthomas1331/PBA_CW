set.seed(123)

NYAirbnb <- read.csv("AB_NYC_2019.csv", encoding = "UTF-8", stringsAsFactors = F, na.strings = c("")) # Reads the dataset

# Why use NA Ref: https://www.statmethods.net/input/missingdata.html
#NYAirbnb[, 16][NYAirbnb[, 16] == 0] <- NA # Sets all the 0 values in latitude to NA

#NYAirbnb <- NYAirbnb[complete.cases(NYAirbnb[,16]),] # Gets rid of the rows with latitude = NA

names_to_delete <- c("id","host_id","number_of_reviews","last_reviews","reviews_per_month")
NYAirbnb[names_to_delete] <- NULL 
NYAirbnb<-NYAirbnb %>% filter(availability_365>0)
print(NYAirbnb$availability_365)
#-------------Getting rid of outliers--------------------#
lq = quantile(NYAirbnb$price)[2] #lower quartile
uq = quantile(NYAirbnb$price)[4] #upper quartile

iqr = uq - lq #interquartile range

upperlim = uq + (iqr*1.5)
lowerlim = lq - (iqr*1.5)

NYAirbnb<-NYAirbnb[!(NYAirbnb$price < lowerlim),]
NYAirbnb<-NYAirbnb[!(NYAirbnb$price > upperlim),]

NYA_rand <- NYAirbnb[sample(nrow(NYAirbnb)),] # Randomise order of dataframe





# Generates subsets based on room_type
Entire_home <- NYA_rand[NYA_rand$room_type == 'Entire home/apt',] # Subset for Entire home/apt
Private_room <- NYA_rand[NYA_rand$room_type == 'Private room',] # Subset for Private Room
Shared_room <- NYA_rand[NYA_rand$room_type == 'Shared room',] # Subset for Shared Room



# -----------------------IGNORE THIS PART-----------------------------#
#sample <- NYA_rand[1:10454,] # Select first 10454 for random sampling

#write.csv(sample,"C:/Users/azwad/Desktop/Uni/Year 3/Semester 1/Practical Business Analytics/CW/rand_sample.csv", row.names = FALSE) # Saves random sample to your own device

#---------------------------------------------------------------------#






#------------------- Plots for Private Rooms -------------------------# 

# 3D Plot with location and price
library(scatterplot3d)


training_data <- Private_room[1:14000, c("latitude","longitude", "price")]
testing_data = Private_room[-(1:14000), c("latitude","longitude", "price")]


price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

par(mar= c(5, 5, 2, 1) + 0.1)

scatterplot3d(longitude,latitude, price, main="3D Scatter plot for Private Rooms")

# Plot by Location divided in Neighbourhood groups

plot(longitude,latitude,col=Private_room$neighbourhood_group, main = "Private Rooms")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"))

#------------------- Plots for Entire Home -------------------------#
training_data <- Entire_home[1:14000, c("latitude","longitude", "price")]
testing_data = Entire_home[-(1:14000), c("latitude","longitude", "price")]

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

plot(longitude,latitude,col=Entire_home$neighbourhood_group, main = "Entire Homes")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"))

#------------------- Plots for Shared Rooms -------------------------#
training_data <- Shared_room[1:14000, c("latitude","longitude", "price")]
testing_data = Shared_room[-(1:14000), c("latitude","longitude", "price")]

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

plot(longitude,latitude,col=Shared_room$neighbourhood_group, main = "Shared Rooms")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"))


# ----------------------Plot by room type---------------#
training_data <- NYA_rand[1:14000, c("latitude","longitude", "price")]
testing_data = NYA_rand[-(1:14000), c("latitude","longitude", "price")]

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

plot(longitude,latitude,col=c(c(1,2,3),c(NYA_rand$room_type)), main = "Room Type", xlab = "Longitude", ylab = "Latitude")
legend("topleft", legend=c("Entire Home","Private Rooms","Shared Rooms") , pch= 1, col=c("Red","Black","Green"))

#------------------- Plots for Price -------------------------#
training_data <- NYA_rand[1:14000, c("latitude","longitude", "price")]
testing_data = NYA_rand[-(1:14000), c("latitude","longitude", "price")]

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

plot(longitude,latitude,col=heat.colors(price), main = "Price")




library("ggplot2")
library(dplyr)

airbnb_nh <- NYA_rand %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(mean(price), 2))

p <- ggplot(NYA_rand, aes(price)) + 
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.3, fill = "purple")  + 
  ggtitle("Distribution of price by neighbourhood groups") +
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = airbnb_nh,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group)

print(p)

ps <- ggplot(NYA_rand, aes(price)) + 
      geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
      geom_density(alpha = 0.3, fill = "purple")  + 
      ggtitle("Distribution of price by neighbourhood groups",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
      geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
      geom_text(data = airbnb_nh,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
      facet_wrap(~neighbourhood_group) + scale_x_log10() 
  


print(ps)

