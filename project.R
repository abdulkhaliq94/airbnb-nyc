library(readr)
airbnb <- read_csv("airbnb.csv", na = "-")

airbnb <- na.omit(airbnb)

# categorical
## barplot of the categorical data about the locations with the highest
## listings across New York City
barplot(table(airbnb$neighbourhood_group), 
                                      main = "Locations With The Most Listings",
                                      xlab = "Five Boroughs Of New York City",
                                      ylab = "Number of listings")


# quantitative
## a histogram of the availability of places to stay throughout the year
hist(airbnb$availability_365, main = "Availability Throughout The Year",
                              xlab = "Days of the year",
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

# ------------------------------------------------------------------------------
## Outliers

# removing outliers
airbnb_price <- airbnb$price[airbnb$price <= 500]
airbnb_price

# removed outliers 
# much better plot with left skewed graph
hist(airbnb_price, breaks=100, main = "Price Distributions",
                                      xlab = "Price",
                                      ylab = "Most Frequent Charges")


# ------------------------------------------------------------------------------
## mean, median, variance and standard deviation for price
# mean
mean(airbnb$price) # mean = 152.7207

# trimmed mean
mean(airbnb$price, trim = 0.05) # trimmed mean = 127.4977

# median
median(airbnb$price) # median = 106

# variance
var(airbnb$price) # var = 57674.03

# standard deviation
sd(airbnb$price) # standard deviation = 240.1542
# ------------------------------------------------------------------------------


## mean, median, variance and standard deviation for 
## calculated host listings count
# mean
mean(airbnb$calculated_host_listings_count) # mean = 7.14

# median
median(airbnb$calculated_host_listings_count) # median = 1

# variance
var(airbnb$calculated_host_listings_count) # var = 1085.868

# standard deviation
sd(airbnb$calculated_host_listings_count) # sd = 32.95252


# ------------------------------------------------------------------------------

## Scatter plots and Correlation
## Scatter plot between price and minimum nights available

plot(price ~ minimum_nights, data = airbnb, 
                                      main = "Price Range and Minimum Nights",
                                      xlab = "Minimum Nights", ylab = "Price")

## Correlation

cor(airbnb$price, airbnb$minimum_nights) # cor = 0.0427

# ------------------------------------------------------------------------------







