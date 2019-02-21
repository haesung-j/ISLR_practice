#### Lab : Cross-Validation and the Bootstrap ####

#### The Validation Set Approach 
library(ISLR)
set.seed(1)
train <- sample(392,196)                 # random subset of 196 observations out of the original 392

# Use the subset option in lm() to fit a linear reg. 
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2) # MSE of the 196 observation in validation set

# estimate the test error for polynimial and cubic regression
lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# If we choose a different train set, we'll obtain differnet errors
set.seed(2)
train <- sample(392,196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2) 

lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# result : using quadratic func. of horsepower performs better than linear func. of horsepower