setwd('/Users/Anush/Desktop/R_WD/practical_ml')
library(rpart)
library(caret)
library(randomForest)
library(rpart)
training <- read.csv('pml-training.csv')
testing <- read.csv('pml-testing.csv')
#calculate variance for all variables
#remove variabls with 0 variance: 
#amplitude_yaw_belt
#amplitude_yaw_dumbbell
#amplitude_yaw_forearm
toRemove <- c('amplitude_yaw_belt','amplitude_yaw_dumbbell','amplitude_yaw_forearm')
columns <- colnames(training)[-c(1:7,26, 101, 139, 160)]

tmp <- training[columns]
#replace all missing entries with NA
tmp[tmp == ''] <- NA

#col <- colnames(tmp)
#vectors <- c('skewness_yaw_belt','kurtosis_yaw_belt','kurtosis_yaw_dumbbell',
         #    'skewness_yaw_dumbbell','kurtosis_yaw_forearm','skewness_yaw_forearm')
#col[!colnames(tmp) %in% vectors]

##check how many nans i have per observation and per variable


naCount <- apply(tmp,2, is.na)
naCount1<- apply(naCount,2, sum)
# We can observe that there is a number of variable that have 19216 NA values out of 19622 observations
# The remaining 2%of observations that have values for these variable cannot be used to predict the mode
# the variable that have 98% NAs are removed from the mode
naCount1
toRem <- names(naCount1[!(naCount1 == 0)])

tmp <- tmp[,!colnames(tmp) %in% toRem]
#check if there are NA values
sum(is.na(tmp))
# ALL NA values were removed



# We observe that the following variables do not carry any values except for  #DIV/0!, 
# they can also be removed from the analysis
skewness_yaw_belt
kurtosis_yaw_belt
kurtosis_yaw_dumbbell
skewness_yaw_dumbbell
kurtosis_yaw_forearm
skewness_yaw_forearm

#tmp[tmp == '#DIV/0!'] <- 'NA'

#check variables variance 
#c <- apply(tmp, 2, var, na.rm = TRUE )
# toRemv <- names(c[is.na(c)])
# tmp <- tmp[,!colnames(tmp) %in% toRemv]

b<- sapply(tmp, function(x) as.numeric(as.character(x)))
preProc <- preProcess(b, method = 'pca', na.remove = TRUE,thresh = 0.8)

preprocObs<- predict(preProc, b)
# creating cross validation 10 folds
flds <- createFolds(tmp[,1], k = 10, list = TRUE, returnTrain = FALSE)
accuracyTree <- c()
accuracyRF <- c()

for (i in 1:10){
    testInd <- flds[[i]]
    test = tmp[testInd, ]
    train <- tmp[-testInd,]
    #modelTree <- rpart(training$classe[-testInd] ~ ., data=train, method="class")
    #predictionTree <- predict(modelTree, test, type = "class")
    #acc <- confusionMatrix(predictionTree, training$classe[testInd])$overall['Accuracy']
    #accuracyTree <- c(accuracyTree, acc)  
    
    i = 1
    modelRF <- randomForest(training$classe[-testInd] ~ ., data=train, method="class")
    predictionRF <- predict(modelRF, test, type = "class")
    acc1 <- confusionMatrix(predictionRF, training$classe[testInd])$overall['Accuracy']
    accuracyRF <- c(accuracyRF, acc1)  
}

#the Accuracy of classification tree :
meanAccClass <- mean(accuracy)


#try to use model 2 - random forest

for (i in 1:10){
    train <- tmp[flds[[i]],]
   
}

mean(accuracyRF)
#100% accuracy use random forest to predict 

cnamesTest <- c(colnames(tmp),'classe')
testing <- testing[,colnames(testing) %in% cnamesTest]