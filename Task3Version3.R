# Module 2 - Task 3: Multiple Regression in R
# Alican Tanaçan
# Version 3 - Creating Models for 2StarReviews, PositiveServiceReview, RecommendProduct
# with 0.75 data partition.

## Libraries ----
library(caret)
library(ggplot2)
library(corrplot)
library(tidyverse)
library(data.table)
library(matrixStats)
library(lubridate)
library(e1071)

## Import Data ----
ExistingProductsData <- read.csv("existingproductattributes2017.csv")

## Preprocessing ----
# To explore our data frame, outliers and NA's:
summary(ExistingProductsData)
# To see and treat outliers in volume, and create a new data frame for preprocessing:
boxplot(ExistingProductsData$Volume)$out
VolumeOutliers <- boxplot(ExistingProductsData$Volume)$out
ExistingProductsData[which(ExistingProductsData$Volume %in% VolumeOutliers), ]
ExProdData <- ExistingProductsData[-which(ExistingProductsData$Volume %in%
                                            VolumeOutliers), ]
# To find out product types:
levels(ExProdData$ProductType)
# To remove a certain product type rows:
ExProdData <- ExProdData[ExProdData$ProductType != "ExtendedWarranty", ]
# To create new features/columns:
ExProdData["StarWeighted"] <- NA
ExProdData["TotalStarReviews"] <- NA
ExProdData$StarWeighted <- as.numeric(ExProdData$StarWeighted)
ExProdData$TotalStarReviews <- as.numeric(ExProdData$TotalStarReviews)
# To calculate total and weighted mean of star reviews for the new columns:
ExProdData$StarWeighted <- (ExProdData$x5StarReviews*5 +
                              ExProdData$x4StarReviews*4 +
                              ExProdData$x3StarReviews*3 +
                              ExProdData$x2StarReviews*2 +
                              ExProdData$x1StarReviews*1) / rowSums(ExProdData[4:8])
ExProdData$TotalStarReviews <- rowSums(ExProdData[4:8])

## Dummifing The Data ----
# To give binary values to every product type:
ExProdDummy <- dummyVars(" ~ .", data = ExProdData)
ExProdReady <- data.frame(predict(ExProdDummy, newdata = ExProdData))

## Feature Selection ----
# To remove uncorrelated/unrelated variables:
ExProdReady[,c(1:17, 19, 21, 23:28, 30)] <- list(NULL)
# Selected Variables: "x2StarReviews", "RecommendProduct", "PositiveServiceReview",
# and "Volume".
# To see the correlation between features:
corrData <- cor(ExProdReady)
corrplot(corrData)
corrData

## Creating Data Partition ----
set.seed(99)
intrain <- createDataPartition(y = ExProdReady$Volume, p = 0.75, list = FALSE)
trainingset <- ExProdReady[intrain,]
testingset <-ExProdReady[-intrain,]

## Random Forest (Manual) Model Training ----
set.seed(199)
RFControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
RFGrid1 <- expand.grid(mtry=c(3,4,5,6))
system.time(RFModel1 <- train(Volume~., data = trainingset, method = "rf",
                              trControl = RFControl1, tuneGrid = RFGrid1))
RFModel1
RFPredict1 <- predict(RFModel1, testingset)
RFPredict1
postResample(RFPredict1, testingset$Volume)

## Random Forest (Auto) Model Training ----
set.seed(249)
RFcontrol2 <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
RFmodel2 <- train(Volume~., data = trainingset, method = "rf",
                  trControl = RFcontrol2, tuneLength = 2)
RFmodel2
RFPredict2 <- predict(RFmodel2, testingset)
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

## To Visualize The Errors ----
ExProdErrSample <- data.frame(RFPredict1, RFPredict2, SVMPredict1,
                              kNNPredict1, testingset$Volume)
ggplot()+
  geom_line(ExProdErrSample, mapping = aes(x = c(1:16), y = RFPredict1),
            colour = "purple")+
  geom_line(ExProdErrSample, mapping = aes(x = c(1:16), y = RFPredict2),
            colour = "black")+
  geom_line(ExProdErrSample, mapping = aes(x = c(1:16), y = SVMPredict1),
            colour = "green")+
  geom_line(ExProdErrSample, mapping = aes(x = c(1:16), y = kNNPredict1),
            colour = "blue")+
  geom_line(ExProdErrSample, mapping = aes(x = c(1:16), y = testingset.Volume),
            colour = "red")