library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)

data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
modFit <- train(y~.,data=vowel.train,method="rf")
prediction <- predict(modFit, vowel.test)
confusionMatrix(prediction, vowel.test$y)
#Accuracy : 0.6061   

modFit1 <- train(y~.,data=vowel.train,method="gbm")
prediction1 <- predict(modFit1, vowel.test)
confusionMatrix(prediction1, vowel.test$y)
#Accurace 0.5325

predDF <- data.frame(prediction, prediction1, y = vowel.test$y)
acc = sum(predDF[predDF[predDF$prediction == predDF$prediction1,][1] == predDF$y,])

sum(prediction[predDF$prediction == predDF$prediction1] == 
        predDF$y[predDF$prediction == predDF$prediction1]) / 
    sum(predDF$prediction == predDF$prediction1)

#Q2
data(AlzheimerDisease)
set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

rf_mod <-train(diagnosis~.,data=training,method="rf")
gbm_mod <-train(diagnosis~.,data=training,method="gbm")
lda_mod <-train(diagnosis~.,data=training,method="lda")

rf_pred<- predict(rf_mod, testing)
gbm_pred<- predict(gbm_mod, testing)
lda_pred <- predict(lda_mod, testing)

confusionMatrix(rf_pred, testing$diagnosis)
confusionMatrix(gbm_pred, testing$diagnosis)
confusionMatrix(lda_pred, testing$diagnosis)

predDF <- data.frame(rf_pred,gbm_pred,lda_pred,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~.,method="rf",data=predDF)
combPred <- predict(combModFit,predDF)

sum(combPred == testing$diagnosis)/length(testing$diagnosis)

#better Stacked Accuracy: 0.80 is better than random forests and lda and the same as boosting.


#Q3
set.seed(3523)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modFit <- train(CompressiveStrength ~.,data = training, method = 'lasso' )

plot.enet(modFit$finalModel, xvar = "penalty", use.color = TRUE)
#cement

#Q4
setwd('/Users/Anush/Desktop/R_WD/practical_ml')
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

mod <- bats(tstrain)
fcast<- forecast(mod, level = 95,  h = dim(testing)[1])
accuracy(fcast,testing)
plot(forecast(mod))

sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
    dim(testing)[1]
#96%

#Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
modFit <- svm(CompressiveStrength ~ ., data = training)
prediction <- predict(modFit, testing)
accuracy(prediction, testing$CompressiveStrength)

sqrt(sum((prediction - testing$CompressiveStrength)^2))
#6.72

# RF Accuracy = 0.6082
# GBM Accuracy = 0.5152
# Agreement Accuracy = 0.6361
# 
# Stacked Accuracy: 0.80 is better than all three other methods
# 
# Cement
# 
# 96
# 
# 6.72