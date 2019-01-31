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

