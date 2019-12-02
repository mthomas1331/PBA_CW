# Plotting functions

#scatter() :
#
# This function will plot a scatter graph with a legend
#
# INPUT :  double  - lon  -  Longitude of Airbnb accomodation 
#       :  double  - lat  -  Latitude of Airnbn accomodation
#       :  integer - type -      
scatter <- function(lon,lat,type,title, leg){
  plot(x = lon,y = lat, col = type, main = title)
  if(leg == "Neighbourhood") {
    legend("topleft", legend=c("Brooklyn","Manhattan","Queens","Staten Island","Bronx") , pch= 1, col=c("Red","Green","Blue","Cyan","Black"), cex = 0.75, pt.cex = 1.5)
  }
  else if(leg == "Room") {
    legend("topleft", legend=c("Entire Home","Private Rooms","Shared Rooms") , pch= 1, col=c("Red","Black","Green"), cex = 0.75, pt.cex = 1.5)
  } else {
    
  }
}


plot_3d <- function(lon,lat,price) {
  scatterplot3d(lon, lat, price, main = "3D Scatter Plot", xlab =   "Longitude", ylab = "Latitude", zlab = "Price")
}


dist <- function(dataset,measure,group) {
  ggplot(dataset, aes(price)) + 
    geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
    geom_density(alpha = 0.3, fill = "purple")  + 
    ggtitle("Distribution of price by neighbourhood groups") +
    geom_vline(data = measure, aes(xintercept = price), size = 2, linetype = 3) +
    facet_wrap(group)
  
}
  

log_dist <- function(dataset, measure,title,group) {
  ggplot(dataset, aes(price)) + 
    geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
    geom_density(alpha = 0.3, fill = "purple")  + 
    ggtitle("Distribution of price by neighbourhood groups",
            subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
    geom_vline(data = measure, aes(xintercept = price), size = 2, linetype = 3) +
    geom_text(data = measure,y = 2, aes(x = price + 140 , label = paste(title,price)), color = "darkgreen", size = 4) +
    facet_wrap(group) + scale_x_log10()
}
