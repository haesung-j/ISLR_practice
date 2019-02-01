#### Lab : Linear Regression ####

# library() fucntion is used to load libraries, or groups of functions
# The MASS library contains the Boston data set.
library(MASS)
dim(Boston)        # 506 14
names(Boston)
?Boston
attach(Boston)

#### Simple Linear Regression
# using lm() to fit simple linear regression model, with medv as response and lstat as predictor.
lm.fit <- lm(medv~lstat, data=Boston)
lm.fit               # It returns Coefficients

summary(lm.fit)      # It returns p-values, standard errors for coefficients, as well as R^2, F
names(summary(lm.fit)) # there are many pieces of information in lm.fit
coef(lm.fit)
confint(lm.fit)        # It retunrs confidence interval for the coefficient estimates
confint(lm.fit, level = 0.90)

predict(lm.fit, data.frame(lstat=c(5,10,15)), interval='confidence')  # C.I for prediction of medv
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval='prediction')  # P.I for prediction of medv


plot(lstat, medv, pch=20, col='grey')
abline(lm.fit, lwd=3, col='red')         # Add least square regression line

par(mfrow=c(2,2))
plot(lm.fit)                             # diagnostic plots
plot(predict(lm.fit), residuals(lm.fit)) # residuals() compute the residuals from linear regrssion fit
plot(predict(lm.fit), rstudent(lm.fit))  # rsudent() returns the studentized residual
plot(hatvalues(lm.fit))                  # hatvalues() returns Leverage statistics
which.max(hatvalues(lm.fit))             # which.max() returns the point that has the largest lev.stat

#### Multiple Linear Regresstion
lm.fit <- lm(medv ~ lstat+age, data=Boston)
summary(lm.fit)

lm.fit <- lm(medv~., data=Boston)        # using all of the predictors
summary(lm.fit)

library(car)
vif(lm.fit)                              # compute VIF

#### Interaction Terms
# X1:X2 means include an interaction term between X1 and X2
# X1*X2 means include X1, X2 and X1*X2 as predictors
summary(lm(medv~lstat*age, data=Boston))

#### Non-linear Transformation of the Predictors
# regression of medv onto lstat and lstat^2
lm.fit2 <- lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

# To test between 2-models, use anova()
# H0 : M1 and M2 fit the data equally well, H1: M2 is better then M1 -- *M2 must include M1*
lm.fit <- lm(medv~lstat, data=Boston)
anova(lm.fit, lm.fit2)         # Reject H_0
par(mfrow=c(2,2))
plot(lm.fit2)

# Instead of using I(), poly() fuction is better approach
lm.fit5 <- lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit5)

# There are many other method for non-linear such as log trasformation
lm.fit <- lm(medv~log(rm), data=Boston)
summary(lm.fit)

#### Qualitative Predictors
library(ISLR)
dim(Carseats)                  # 400 11
names(Carseats)
head(Carseats)
summary(Carseats)              # ShelveLoc is qualitative predictor with 3-levels
attach(Carseats)

lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)                # R generate dummy variables automatically
contrasts(ShelveLoc)           # it reurns the coding for dummy variables


