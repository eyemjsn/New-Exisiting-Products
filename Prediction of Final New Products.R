library(caret)
existingproducts <- read_csv("D:/Database Analytics/Course 2/R Tutorial Data Sets/Task 3/existingproductattributes2017.csv")
newdataframe <- dummyVars("~.", data = existingproducts)
readyexisting <- data.frame(predict(newdataframe,newdata = existingproducts))
str(readyexisting)
summary(readyexisting)
#removing attributes that will skew the results to be overfitting
readyexisting$BestSellersRank <- NULL
summary(readyexisting)
readyexisting$x5StarReviews <- NULL
readyexisting$ProductNum <- NULL
library(corrplot)
corrData <- cor(readyexisting)
corrplot(corrData)
readyexisting <- readyexisting[-73,]
readyexisting <- readyexisting[-66,]
readyexisting <- readyexisting[-50,]
readyexisting <- readyexisting[-32,]
readyexisting <- readyexisting[-29,]
set.seed(123)
inTraining <- createDataPartition(readyexisting$Volume,p=.75,list = FALSE)
training <- readyexisting[inTraining,]
testing <- readyexisting[-inTraining,]
rfControl <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
rfFit <- train(Volume~.,data = training, method="rf",trControl=rfControl,tuneLength=1)
rfFit
rfPreds <- predict(rfFit,testing)
rfPreds
postResample(rfPreds,testing$Volume)

library(caret)
newdataframe <- dummyVars("~.", data = existingproducts)
readyexisting1 <- data.frame(predict(newdataframe,newdata = existingproducts))
str(readyexisting1)
summary(readyexisting1)
readyexisting1$BestSellersRank <- NULL
summary(readyexisting1)
readyexisting1$x5StarReviews <- NULL
readyexisting1$ProductNum <- NULL
library(corrplot)
corrData1 <- cor(readyexisting1)
corrplot(corrData)
readyexisting1 <- readyexisting1[-73,]
readyexisting1 <- readyexisting1[-50,]
set.seed(123)
inTraining1 <- createDataPartition(readyexisting1$Volume,p=.70,list = FALSE)
training1 <- readyexisting1[inTraining1,]
testing1 <- readyexisting1[-inTraining1,]
rfControl1 <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
rfFit1 <- train(Volume~.,data = training1, method="rf",trControl=rfControl1,tuneLength=1)
rfFit1
rfPreds1 <- predict(rfFit1,testing1)
postResample(rfPreds1,testing1$Volume)

##notuneLength
set.seed(123)
inTrainingnotune <- createDataPartition(readyexisting1$Volume,p=.70,list = FALSE)
trainingnotune <- readyexisting1[inTrainingnotune,]
testingnotune <- readyexisting1[-inTrainingnotune,]
rfControlnotune <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
rfFitnotune <- train(Volume~.,data = trainingnotune, method="rf",trControl=rfControlnotune)
rfFitnotune
rfPrednotune <- predict(rfFitnotune,testingnotune)
postResample(rfPrednotune,testing1$Volume)

##SVM
set.seed(123)
inTrainingsvm <- createDataPartition(readyexisting1$Volume,p=.70,list = FALSE)
trainingsvm1 <- readyexisting1[inTrainingsvm,]
testingsvm1 <- readyexisting1[-inTrainingsvm,]
svmControl <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
svmFit <- train(Volume~.,data = trainingsvm1, method="svmLinear",trControl=svmControl,tuneLength=1)
svmFit
svmPreds <- predict(svmFit,testing1)
postResample(svmPreds,testing1$Volume)

###svm Without tuneLength
set.seed(123)
inTrainingsvm1 <- createDataPartition(readyexisting1$Volume,p=.70,list = FALSE)
trainingsvmnotune <- readyexisting1[inTrainingsvm1,]
testingsvmnotune <- readyexisting1[-inTrainingsvm1,]
svmControlnotune <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
svmFitnotune <- train(Volume~.,data = trainingsvmnotune, method="svmLinear",trControl=svmControlnotune)
svmFitnotune
svmPredsnotune <- predict(svmFitnotune,testing1)
postResample(svmPredsnotune,testing1$Volume)
### Trying GradientBoosting
set.seed(123)
inTrainingGB <- createDataPartition(readyexisting1$Volume,p=.70,list = FALSE)
trainingGB <- readyexisting1[inTrainingGB,]
testingGB <- readyexisting1[-inTrainingGB,]
gbControl <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
gbFit <- train(Volume~.,data = trainingGB, method="gbm",trControl=gbControl,tuneLength=1)
gbFit
gbPreds <- predict(gbFit,testing1)
postResample(gbPreds,testing1$Volume)
###
### gbnotunelength
set.seed(123)
inTrainingGBnotune <- createDataPartition(readyexisting1$Volume,p=.70,list = FALSE)
trainingGBnotune <- readyexisting1[inTrainingGBnotune,]
testingGBnotune <- readyexisting1[-inTrainingGBnotune,]
gbControlnotune <- trainControl(method = "repeatedcv",number = 10,repeats = 1)
gbFitnotune <- train(Volume~.,data = trainingGBnotune, method="gbm",trControl=gbControlnotune)
gbFitnotune
gbPrednotune <- predict(gbFitnotune,testing1)
postResample(gbPrednotune,testing1$Volume)
##
##
## importing New Products Need to remove attributs x5stars,productnumb,bestsellers
newproducts <- read_csv("D:/Database Analytics/Course 2/R Tutorial Data Sets/Task 3/newproductattributes2017.csv")
productdataframe <- dummyVars("~.", data = newproducts)
readynewproducts <- data.frame(predict(productdataframe,newdata = newproducts))
str(readynewproducts)
readynewproducts$BestSellersRank <- NULL
readynewproducts$x5StarReviews <- NULL
readynewproducts$ProductNum <- NULL
summary(readynewproducts)
##now checking fitmodel with newproducts for results
##
prednewproduct <- predict(rfFitnotune,readynewproducts)
prednewproduct
## place results into table
install.packages(xlsx)
library(xlsx)
write.table(prednewproduct,file = "Prediction of New Products.csv",sep = " ")
write.table(prednewproduct,file = "Prediction of New Products.xls",sep = " ")

