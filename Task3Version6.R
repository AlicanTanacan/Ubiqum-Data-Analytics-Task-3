# Module 2 - Task 3: Multiple Regression in R
# Alican Tanaçan
# Version 6 - Creating Models for PositiveServiceReview, NegativeServiceReview, StarWeighted
# with 0.75 data partition.

## Libraries ----
library(caret)
library(ggplot2)
library(corrplot)
library(tidyverse)

## Import Data ----
ExistingProductsData <- read.csv("existingproductattributes2017.csv")

## Preprocessing ----
# To explore our data frame, outliers and NA's:
summary(ExistingProductsData)
# To see and treat outliers in volume, and create a new data frame for preprocessing:
boxplot(ExistingProductsData)
boxplot(ExistingProductsData$Volume)$out
VolumeOutliers <- boxplot(ExistingProductsData$Volume)$out
ExistingProductsData[which(ExistingProductsData$Volume %in% VolumeOutliers), ]
ExProdNoOut <- ExistingProductsData[-which(ExistingProductsData$Volume %in%
                                             VolumeOutliers), ]
# To find out product types:
levels(ExProdNoOut$ProductType)
# To remove a certain product type rows:
ExProdProcessed <- ExProdNoOut[ExProdNoOut$ProductType != "ExtendedWarranty", ]
ExProdProcessed$ProductType <- factor(ExProdProcessed$ProductType)

## Feature Engineering ----
# To create new features/columns:
ExProdProcessed["StarWeighted"] <- NA
ExProdProcessed["TotalStarReviews"] <- NA
ExProdProcessed$StarWeighted <- as.numeric(ExProdProcessed$StarWeighted)
ExProdProcessed$TotalStarReviews <- as.numeric(ExProdProcessed$TotalStarReviews)
# To calculate total and weighted mean of star reviews for the new columns:
ExProdProcessed$StarWeighted <- (ExProdProcessed$x5StarReviews*5 +
                                   ExProdProcessed$x4StarReviews*4 +
                                   ExProdProcessed$x3StarReviews*3 +
                                   ExProdProcessed$x2StarReviews*2 +
                                   ExProdProcessed$x1StarReviews*1) / rowSums(ExProdProcessed[4:8])
ExProdProcessed$TotalStarReviews <- rowSums(ExProdProcessed[4:8])
# Removing column with NA's:
ExProdProcessed$BestSellersRank <- NULL

## Dummifing The Data ----
# To give binary values to every product type:
ExProdDummy <- dummyVars(" ~ .", data = ExProdProcessed)
ExProdReady <- data.frame(predict(ExProdDummy, newdata = ExProdProcessed))
# To see the correlation
corrData <- cor(ExProdReady)
corrplot(corrData)
corrData

## Feature Selection ----
# To remove uncorrelated/unrelated variables:
ExProdReady[,c(1:18, 21:26, 29)] <- list(NULL)
# To see the correlation again
corrData <- cor(ExProdReady)
corrplot(corrData)
corrData

## Creating Data Partition ----
set.seed(69)
intrain <- createDataPartition(y = ExProdReady$Volume, p = 0.75, list = FALSE)
trainingset <- ExProdReady[intrain,]
testingset <-ExProdReady[-intrain,]

## Random Forest (Manual) Model Training ----
set.seed(199)
RFControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
RFGrid1 <- expand.grid(mtry=c(1,2,3))
system.time(RFModel1 <- train(Volume~., data = trainingset, method = "rf",
                              trControl = RFControl1, tuneGrid = RFGrid1))
RFModel1
RFPredict1 <- predict(RFModel1, testingset)
RFPredict1
postResample(RFPredict1, testingset$Volume)

## Random Forest (Auto) Model Training ----
set.seed(249)
RFcontrol2 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
RFModel2 <- train(Volume~., data = trainingset, method = "rf",
                  trControl = RFcontrol2, tuneLength = 2)
RFModel2
RFPredict2 <- predict(RFModel2, testingset)
RFPredict2
postResample(RFPredict2, testingset$Volume)

## Support Vector Machine Model Training ----
set.seed(299)
SVMControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
SVMGrid1 <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
SVMModel1 <- train(Volume ~., data = trainingset, method = "svmLinear", 
                   trControl = SVMControl1, preProcess = c("center", "scale"),
                   tuneGrid = SVMGrid1, tuneLength = 10)
SVMModel1
SVMPredict1 <- predict(SVMModel1, testingset)
SVMPredict1
postResample(SVMPredict1, testingset$Volume)

## kNN Model Training ----
set.seed(349)
trainX <- trainingset[,names(trainingset) != "Volume"]
preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
preProcValues
set.seed(400)
kNNControl1 <- trainControl(method="repeatedcv", search = "random",
                            repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
kNNModel1 <- train(Volume ~ ., data = trainingset, method = "knn",
                   trControl = kNNControl1, preProcess = c("center","scale"),
                   tuneLength = 20)
kNNModel1
kNNPredict1 <- predict(kNNModel1, testingset)
kNNPredict1
postResample(kNNPredict1, testingset$Volume)

## To Try Each Model On "PC, Laptop, Smartphone, Netbook" Sample:
ExProdSample <- ExistingProductsData %>% filter(ProductType == "PC" |
                                                  ProductType == "Laptop" |
                                                  ProductType == "Netbook" |
                                                  ProductType == "Smartphone")
ExProdSample$ProductType <- factor(ExProdSample$ProductType)
# To create new features/columns:
ExProdSample["StarWeighted"] <- NA
ExProdSample["TotalStarReviews"] <- NA
ExProdSample$StarWeighted <- as.numeric(ExProdSample$StarWeighted)
ExProdSample$TotalStarReviews <- as.numeric(ExProdSample$TotalStarReviews)
# To calculate total and weighted mean of star reviews for the new columns:
ExProdSample$StarWeighted <- (ExProdSample$x5StarReviews*5 +
                                ExProdSample$x4StarReviews*4 +
                                ExProdSample$x3StarReviews*3 +
                                ExProdSample$x2StarReviews*2 +
                                ExProdSample$x1StarReviews*1) / rowSums(ExProdSample[4:8])
ExProdSample$TotalStarReviews <- rowSums(ExProdSample[4:8])
# Removing column with NA's:
ExProdSample$BestSellersRank <- NULL
# Prediction analyses:
RFPredict1onSample <- predict(RFModel1, ExProdSample)
RFPredict1onSample
RFPredict2onSample <- predict(RFModel2, ExProdSample)
RFPredict2onSample
SVMPredict1onSample <- predict(SVMModel1, ExProdSample)
SVMPredict1onSample
kNNPredict1onSample <- predict(kNNModel1, ExProdSample)
kNNPredict1onSample

## To Visualize The Errors ----
# On testing set:
ExProdErrSample1 <- data.frame(RFPredict1, RFPredict2, SVMPredict1,
                               kNNPredict1, testingset$Volume)
ggplot()+
  geom_line(ExProdErrSample1, mapping = aes(x = c(1:16), y = testingset.Volume),
            colour = "red")+
  geom_line(ExProdErrSample1, mapping = aes(x = c(1:16), y = RFPredict1),
            colour = "black")+
  geom_line(ExProdErrSample1, mapping = aes(x = c(1:16), y = RFPredict2),
            colour = "green")+
  geom_line(ExProdErrSample1, mapping = aes(x = c(1:16), y = SVMPredict1),
            colour = "blue")+
  geom_line(ExProdErrSample1, mapping = aes(x = c(1:16), y = kNNPredict1),
            colour = "purple")
# On sample set:
ExProdErrSample2 <- data.frame(RFPredict1onSample, RFPredict2onSample, SVMPredict1onSample,
                               kNNPredict1onSample, ExProdSample$Volume)
ggplot()+
  geom_line(ExProdErrSample2, mapping = aes(x = c(1:13), y = ExProdSample.Volume),
            colour = "red")+
  geom_line(ExProdErrSample2, mapping = aes(x = c(1:13), y = RFPredict1onSample),
            colour = "black")+
  geom_line(ExProdErrSample2, mapping = aes(x = c(1:13), y = RFPredict2onSample),
            colour = "green")+
  geom_line(ExProdErrSample2, mapping = aes(x = c(1:13), y = SVMPredict1onSample),
            colour = "blue")+
  geom_line(ExProdErrSample2, mapping = aes(x = c(1:13), y = kNNPredict1onSample),
            colour = "purple")

## To Make Predictions On New Products Data ----
NewProductsData <- read.csv("newproductattributes2017.csv")
# Preprocess the new products list:
NewProductsData["StarWeighted"] <- NA
NewProductsData["TotalStarReviews"] <- NA
NewProductsData$StarWeighted <- as.numeric(NewProductsData$StarWeighted)
NewProductsData$TotalStarReviews <- as.numeric(NewProductsData$TotalStarReviews)
# To calculate total and weighted mean of star reviews for the new columns:
NewProductsData$StarWeighted <- (NewProductsData$x5StarReviews*5 +
                                   NewProductsData$x4StarReviews*4 +
                                   NewProductsData$x3StarReviews*3 +
                                   NewProductsData$x2StarReviews*2 +
                                   NewProductsData$x1StarReviews*1) / rowSums(NewProductsData[4:8])
NewProductsData$TotalStarReviews <- rowSums(NewProductsData[4:8])
# To predict volume:
newproductspredict <-predict(RFModel1, NewProductsData)
newproductspredict
NewProductsData["PredictedVolume"] <- newproductspredict

## To Create The Excel Of New Products' Predictions ----
write.csv(NewProductsData, file="newproductspredictions6.csv", row.names = TRUE)