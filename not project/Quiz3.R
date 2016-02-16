library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
require(rpart)
require(rattle)

#q1
training <- segmentationOriginal[segmentationOriginal$Case == 'Train',]
testing <- segmentationOriginal[segmentationOriginal$Case == 'Test',]

set.seed(125)
modFit <- train(Class ~ .,method="rpart",data=training)
fancyRpartPlot(modFit$finalModel)
# PS WS PS not possible to predict

#q2
#The bias is larger and the variance is smaller.
#Under leave one out cross validation K is equal to the sample size.

#q3
load('olive')
olive = olive[,-1]
modFit <- train(Area ~ .,method="rpart",data=olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit,newdata=newdata)
#2.783. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

#q4
library('ElemStatLearn')
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit<- train(chd ~ age + alcohol +obesity + tobacco + typea + ldl,
               method = "glm", family = "binomial" , data=trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

prediction <- predict(modFit, trainSA)
values <- trainSA$chd
missClass(values,prediction)

prediction1 <- predict(modFit, testSA)
values1 <- testSA$chd
missClass(values1,prediction1)
#Test Set Misclassification: 0.31
#Training Set: 0.27

#Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)

modFit <- train(y~.,data=vowel.train,method="rf",importance = TRUE)
fm <- modFit$finalModel
varImp(modFit)

a<- varImp(fm)[1]