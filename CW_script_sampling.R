NYAirbnb <- read.csv("AB_NYC_2019.csv") # Reads the dataset

# Why use NA Ref: https://www.statmethods.net/input/missingdata.html
NYAirbnb[, 16][NYAirbnb[, 16] == 0] <- NA # Sets all the 0 values in latitude to NA

NYAirbnb <- NYAirbnb[complete.cases(NYAirbnb[,16]),] # Gets rid of the rows with latitude = NA

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

training_data <- Private_room[1:1000, c("latitude","longitude", "price")]
testing_data = Private_room[-(1:1000), c("latitude","longitude", "price")]

price<-training_data$price
latitude<-training_data$latitude
longitude<-training_data$longitude

scatterplot3d(latitude,longitude, price, main="Test 1")

# Plot by Location divided in Neighbourhood groups
plot(longitude,latitude,col=Private_room$neighbourhood_group, main = "Neighbourhood groups")
legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , col=c("Red","Green","Blue","Cyan","Black"))
