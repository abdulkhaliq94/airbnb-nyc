library(readr)
airbnb <- read_csv("airbnb.csv", na = "-")

airbnb <- na.omit(airbnb)

# categorical
## barplot of the categorical data about the locations with the highest listings across New York City
barplot(table(airbnb$neighbourhood_group), main = "Locations With The Most Listings",
                                            xlab = "Five Boroughs Of New York City",
                                            ylab = "Number of listings")


# quantitative
## a histogram of the availability of places to stay throughout the year
hist(airbnb$availability_365, main = "Availability Throughout The Year",
                              xlab = "Available Places",
                              ylab = "Number of Listings Available")

# quantitative
## a histogram of the price distributions and range
hist(airbnb$price, breaks=100, main = "Price Distributions",
                                            xlab = "Price",
                                            ylab = "Most Frequent Charges")

# categorical
## barplot of the types of rooms the guests prefer
barplot(table(airbnb$room_type), main = "Room Types Guests Prefer",
                                  xlab = "Types of Rooms",
                                  ylab = "Most Frequently Preferred")


# removed outliers 
# much better plot with left skewed graph
hist(airbnb_price, breaks=100, main = "Price Distributions",
                                      xlab = "Price",
                                      ylab = "Most Frequent Charges")
