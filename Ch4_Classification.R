#### Stock Market Data
library(ISLR)
names(Smarket)
dim(Smarket)              # 1250 9
summary(Smarket)          # Direction is qulitative, ow are quantitative.

# check correlation without Direction(qulitative)
cor(Smarket[, -9])        # Only Year,Volumn have substantial correlation
attach(Smarket)
plot(Year, Volume)


#### Logistic Regression ####
## Fit model to predict Direction using Lag1 ~ Lag5 and Volume
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

## Now split data set - train set & test set
train <- Year<2005
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)               # 252 9
Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, subset=train, family=binomial)
glm.probs <- predict(glm.fit, newdata=Smarket.2005, type='response')  # predict probs for Smarket.2005
glm.pred <- rep('Down',nrow(Smarket.2005))
glm.pred[glm.probs>0.5] <- 'Up'
table(glm.pred, Direction.2005)   # Confusion matrix
mean(glm.pred == Direction.2005)  # 0.4801587

## To improve model, remove predictors that have high p-value
glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket, subset=train, family=binomial)
glm.probs <- predict(glm.fit, newdata=Smarket.2005, type='response')
glm.pred <- rep('Down', nrow(Smarket.2005))
glm.pred[glm.probs>0.5] <- 'Up'
mean(glm.pred == Direction.2005)  # 0.5595238 

## prediction with particular values of predictor
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)), type='response')


#### Linear Discriminant Analysis(LDA) ####
# fit LDA model using lda(), which is part of MASS library
library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit     # it gives Prior probs, Group means, Coefficients of LD
plot(lda.fit)

# predict() returns a list : Class, Posterior, x
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)     # 0.5595238

# the result is gained by using 50% threshold to posterior probs
sum(lda.pred$posterior[,1]>0.5)     # 70    - Down
sum(lda.pred$posterior[,1]<=0.5)    # 182   - Up
lda.pred$posterior[1:20,]          # Notice that the posterir probs output coreespond to the probs of Down

# If you want to use other threshold, 0.9
sum(lda.pred$posterior[,1]>0.9)   # 0  -> result : No Down

#### Quadratic Discriminant Analysis(QDA) ####
# using qda() which is also part of MASS library
qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit        # it doesn't give the coefficients

qda.pred <- predict(qda.fit, newdata=Smarket.2005)
qda.class <- qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005) # 0.5992063


#### K-Nearest Neighbors (KNN) ####
# using knn(), which is part of the class library - it needs 4 inputs
# A matrix containing the predictors associated with the train set, labeled train.X
# A matrix containing the predictios associated with the test set, labeled test.X
# A vector containing the class label for the training set, labeled train.Direction below
# A value for K, the number of nearest neighbors to be used by the classifier
#
# We predict Direction using Lag1, Lag2

library(class)
train.X <- cbind(Lag1,Lag2)[train,]
test.X <- cbind(Lag1,Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred ,Direction.2005)
mean(knn.pred == Direction.2005)   # 0.5  --- K=1 model is very flexible

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)   # 0.5357143
