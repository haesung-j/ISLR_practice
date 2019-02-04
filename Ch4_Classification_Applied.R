#### 10. Use Weekly data(ISLR package) ####
#(a) Produce some numerical and graphical summaries of the Weekly data. Do there appear to be any 
#    pattern?

library(ISLR)
?Weekly
head(Weekly)
summary(Weekly)        # all variables are quantative, except Direction
cor(Weekly[,-9])       # Only Year&Volume have high correlation
plot(Weekly)           # Year&Volume have non-linear relationship, No other patterns

#(b) Use logistic regression with Direction as the response and the five lag variables plus Volume
#    as predictiors. Use Summary function to print the results. Do any of the predictors appear to
#    be statistically significant? If so, which ones?

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)
summary(glm.fit)       # Lag2 have Pr(>|z|)=0.0296

#(c) Compute the confusion matrix and overall fraction of correct predictions. Explain what the 
#    confusion matrix is telling you about the types of mistakes made by logistic regression.
attach(Weekly)
glm.probs <- predict(glm.fit, type = 'response')
contrasts(Direction)    # Up is 1, Down is 0
glm.pred <- rep('Down',nrow(Weekly))
glm.pred[glm.probs>0.5] <- 'Up'
table(glm.pred, Direction)  
# when market goes Up, the model predict well : 557/(557+48) = 0.9207
# But when market goes Down, the model predict wrong : 53/(430+53) = 0.1097
# Overall correction: (54+557) / (54+48+430+557) = 0.5611

#(d) Now fit the logistic regression model using a training data period from 1990 to 2008, with Lag2
#    as the only predictor. Compute the confusion matrix and the overall fraction of correct predict-
#    ors for the held out data.

train <- Year<2009
test <- !train
Weekly.test <- Weekly[test,]
Direction.test <- Direction[test]

glm.fit <- glm(Direction~Lag2, data=Weekly, subset=train, family=binomial)
glm.probs <- predict(glm.fit, newdata=Weekly.test, type='response')
glm.pred <- rep('Down',nrow(Weekly.test))
glm.pred[glm.probs>0.5] <- 'Up'
table(glm.pred, Direction.test)       # (9+56)/104 = 0.625

#(e) Repeat (d) using LDA
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.pred <- predict(lda.fit, newdata=Weekly.test)
lda.class <- lda.pred$class
table(lda.class, Direction.test)     # (9+56)/104 = 0.625  same as logistic regression

#(e) Repeat (d) using QDA
qda.fit <- qda(Direction ~ Lag2, data=Weekly, subset=train)
qda.pred <- predict(qda.fit, newdata=Weekly.test)
qda.class <- qda.pred$class
table(qda.class, Direction.test)    # 61/104 = 0.5865, this model predict Direction will be all 'Up'

#(g) Repeat (d) using KNN with K=1
library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[test])
train.D <- Direction[train]

knn.pred <- knn(train.X, test.X, train.D, k=1)
table(knn.pred, Direction.test)   # (21+32)/104 = 0.5096

#(h) Which of these methods appears to provide the best results on this data?
# Logistic regression & LDA

#### 11. Use Auto data - ISLR package ####
#(a) Create a binary variable, mpg01. use median() to mpg
library(ISLR)
head(Auto)
summary(Auto$mpg)

mpg01 <- rep(0,nrow(Auto))
mpg01[Auto$mpg>median(Auto$mpg)] <- 1
Auto <- data.frame(Auto, mpg01)

# Explore the data graphically in order to investigate the association between mpg01 and others.
attach(Auto)
cor(Auto[,-9])
plot(Auto)
boxplot(mpg01, horsepower)
boxplot(mpg01, displacement)
boxplot(mpg01, weight)
boxplot(mpg01, acceleration)


# cylinders displacement weight horsepower have correlate with mpg01

#(c) split the data into a traing set and a test set
train <- sample(1:nrow(Auto), nrow(Auto)*0.5)
test <- -train
Auto.test <- Auto[test,]
mpg01.test <- mpg01[test]

#(d) Perform LDA on the training data & Compute test error of the model
library(MASS)
lda.fit <- lda(mpg01 ~cylinders+displacement+weight+horsepower, data=Auto, subset=train)
lda.fit
lda.pred <- predict(lda.fit, newdata=Auto.test)
lda.class <- lda.pred$class
table(lda.class, mpg01.test)
mean(lda.class != mpg01.test)       # test error = 0.1122449

#(e) Perform QDA on the training data & Compute test error of the model
qda.fit <- qda(mpg01~cylinders+displacement+weight+horsepower, data=Auto, subset=train)
qda.class <- predict(qda.fit, newdata=Auto.test)$class
table(qda.class, mpg01.test)
mean(qda.class != mpg01.test)       # test error = 0.122449

#(f) Perform Logistic reg. on the training data & Compute test error of the model
glm.fit <- glm(mpg01~cylinders+displacement+weight+horsepower,data=Auto, subset=train,family=binomial)
glm.probs <- predict(glm.fit, newdata=Auto.test, type='response')
glm.pred <- rep(0,length(test))
glm.pred[glm.probs>0.5] <- 1
table(glm.pred, mpg01.test)
mean(glm.pred != mpg01.test)       # test error = 0.1071429

# (g) Perform KNN on the training data & Compute test error of the model & select K
library(class)
train.X <- cbind(cylinders,displacement,weight,horsepower)[train,]
test.X  <- cbind(cylinders,displacement,weight,horsepower)[test,]
train.mpg01 <- mpg01[train]

knn.pred <- knn(train.X, test.X, train.mpg01, k=1)
mean(knn.pred != mpg01.test)      # test error = 0.127551

knn.pred <- knn(train.X, test.X, train.mpg01, k=2)
mean(knn.pred != mpg01.test)      # test error = 0.122449

knn.pred <- knn(train.X, test.X, train.mpg01, k=10)
mean(knn.pred != mpg01.test)      # test error = 0.1428571

knn.pred <- knn(train.X, test.X, train.mpg01, k=50)
mean(knn.pred != mpg01.test)      # test error = 0.122449

# k=50 or 2 is best