#### Stock Market Data
library(ISLR)
names(Smarket)
dim(Smarket)              # 1250 9
summary(Smarket)          # Direction is qulitative, ow are quantitative.

# check correlation without Direction(qulitative)
cor(Smarket[, -9])        # Only Year,Volumn have substantial correlation
attach(Smarket)
plot(Year, Volume)


#### Logistic Regression
# Fit model to predict Direction using Lag1 ~ Lag5 and Volume
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family = binomial)
summary(glm.fit)           # Smallest p-value : Lag1 but 0.14 & negative coefficient
coef(glm.fit)              # Access the coefficients for fitted model
summary(glm.fit)$coef

glm.probs <- predict(glm.fit, type='response')  # output probs of the form P(Y=1|X)
head(glm.probs)
contrasts(Direction)            # R has created dummy variable with a 1 for Up

glm.pred <- rep('Down',nrow(Smarket))   # If predict probs>0.5, We predict Direction is Up
glm.pred[glm.probs>0.5] <- 'Up'

table(glm.pred, Direction)      # confusion matrix
mean(glm.pred == Direction)     # 0.5216

