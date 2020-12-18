library(readr)
airbnb <- read_csv("airbnb.csv", na = "-")

airbnb <- na.omit(airbnb)

# categorical
## barplot of the categorical data about the locations with the highest
## listings across New York City
barplot(table(airbnb$neighbourhood_group), 
                                main = "Locations With The Most Listings",
                                xlab = "Five Boroughs Of New York City",
                                ylab = "Number of listings",
                                col = c("#F4A460", "#CD5C5C", "#008000",
                                        "#FF8C00", "#FF0000"))


# quantitative
## a histogram of the availability of places to stay throughout the year
hist(airbnb$availability_365, main = "Availability Throughout The Year",
                              xlab = "Days of the year",
                              ylab = "Number of Listings Available",
                              col = c("#B00000", "#DC143C", "#0000FF",
                                      "#FF69B4", "#FFD700"))


# quantitative
## a histogram of the price distributions and range
hist(airbnb$price, breaks=100, main = "Price Distributions",
                              xlab = "Price in $",
                              ylab = "Number of Charges",
                              col = c("#008000"))

# categorical
## barplot of the types of rooms the guests prefer
barplot(table(airbnb$room_type), main = "Room Types Hosts Rent",
                                  xlab = "Types of Rooms",
                                  ylab = "Most Frequently Preferred",
                                  col = c("#87CEFA", "#2F4F4F", "#A52A2A"))

# ------------------------------------------------------------------------------
## Outliers

# removing outliers
airbnb_price <- airbnb$price[airbnb$price <= 500]
airbnb_price

hist(airbnb_price, breaks=10, main = "Price Distributions",
                               xlab = "Price in $",
                               ylab = "Most Frequent Charges",
                               col = c("#006400", "#008000", "#32CD32", 
                                       "#98FB98"))

# removed outliers 
# much better plot with left skewed graph
hist(airbnb_price, breaks=100, main = "Price Distributions",
                                      xlab = "Price in $",
                                      ylab = "Most Frequent Charges",
                                      col = c("#FFC0CB", "#DB7093", "#FFA07A",
                                              "#E9967A", "#FA8072", "#F08080",
                                              "#CD5C5C", "#FF0000", "#DC143C",
                                              "#B22222", "#8B0000"))

# narrowing down some more
airbnb_price2 <- airbnb$price[airbnb$price <= 200]

hist(airbnb_price2, breaks=100, main = "Price Distributions",
                                xlab = "Price in $",
                                ylab = "Most Frequent Charges",
                                col = c("#FFC0CB", "#DB7093", "#FFA07A",
                                        "#E9967A", "#FA8072", "#F08080",
                                        "#CD5C5C", "#FF0000", "#DC143C",
                                        "#B22222", "#8B0000"))

# ------------------------------------------------------------------------------
## mean, median, variance and standard deviation for price
# mean
mean(airbnb$price) # mean = 152.7207

# trimmed mean
mean(airbnb$price, trim = 0.05) # trimmed mean = 127.4977
mean(airbnb$price, trim = 0.10) # trimmed mean = 121.4338

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

# mean
mean(airbnb$calculated_host_listings_count, trim=0.05) # trimmed mean = 1.8633

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

## Confidence Intervals for the mean 

xbar = mean(airbnb$price)     # 152.7207
xbar

s = sd(airbnb$price)          # 240.1542
s

n = 48895
  
t = qt(0.025, 48895-1)  # -1.96
t

t = qt(0.975, 48895-1)  # 1.96
t

L = xbar - t* s/sqrt(n) # L = 150.592
L

U = xbar + t* s/sqrt(n) # u = 154.8494
U

# ------------------------------------------------------------------------------

xbar = mean(airbnb$minimum_nights)  # 7.02
xbar

s = sd(airbnb$minimum_nights)       # 20.51
s

n = 48895

t = qt(0.025, 48895-1) # -1.96
t

t = qt(0.975, 48895-1)  #1.96
t

L = xbar - t*s/sqrt(n)   # 6.84
L

U = xbar + t*s/sqrt(n)  #7.2117
U

# ------------------------------------------------------------------------------
## Linear Regression model

airbnb.lm <- lm(price ~ minimum_nights + number_of_reviews + 
                  reviews_per_month, data = airbnb)
airbnb.lm

predict(airbnb.lm, newdata = data.frame(minimum_nights = 7, 
                                        number_of_reviews = 120, 
                                        reviews_per_month = 4.2))  # 128.1993

hist(resid(airbnb.lm), main = "Histogram of the residuals",
                        xlab = "Residuals",
                        col = c("#006400", "#008000", "#32CD32",
                        "#98FB98"))

plot(fitted.values(airbnb.lm), resid(airbnb.lm), 
                                    main = "Fitted Values vs. Residuals",
                                    xlab = "Fitted or Actual Observed values",
                                    ylab = "Residuals", ylim = c(-200, 2000))
abline(h=0)

summary(airbnb.lm) # R-squared = 0.001939









