setwd('/Users/Anush/Desktop/R_WD/practical_ml/')
library(AppliedPredictiveModeling)
data(AlzheimerDisease)


adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[trainIndex,]

#question 2

library(AppliedPredictiveModeling)
library(Hmisc)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

ccut <- function(x){
    cut2(x, g = 4)
}
tmp <- training
a <- cut2(training$FlyAsh, g = 5)
qplot(x = inTrain,y =training$CompressiveStrength, data = training, color=a )


#Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

ggplot(training, aes(x=Superplasticizer)) +  
    geom_histogram(binwidth=.0005, colour="black", fill="white")

ggplot(training, aes(x=log10(Superplasticizer))) +  
    geom_histogram(binwidth=.05, colour="black", fill="white")

#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

columns <- colnames(training[grep("^IL", colnames(training))])

preProc <- preProcess(training[columns], method = 'pca', thresh = 0.9)

#Q5
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

testing_new <- testing[c('diagnosis',columns)]
training_new <- training[c('diagnosis',columns)]

preProc <- preProcess(training[columns], method = 'pca', thresh = 0.8)
trainPC <- predict(preProc,training_new[,-1])
modelFit <- train(training_new$diagnosis ~ ., method = "glm", data = trainPC)
testPC <- predict(preProc,testing_new[,-1] )
confusionMatrix(testing_new$diagnosis, predict(modelFit, testPC))
