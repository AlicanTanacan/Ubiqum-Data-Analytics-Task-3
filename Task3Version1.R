# Module 2 - Task 3: Multiple Regression in R
# Alican Tanaçan
# Version 1 - Task Exploration Before Preprocessing

## Libraries ----
library(caret)
library(ggplot2)
library(corrplot)

## Import Data ----
ExistingProductsData <- read.csv("existingproductattributes2017.csv")

## Data Exploration ----
summary(ExistingProductsData)
str(ExistingProductsData)
plot(ExistingProductsData$ProductType, ExistingProductsData$x5StarReviews)
ggplot(ExistingProductsData, aes(x5StarReviews,PositiveServiceReview,
                                 color = ProductType))+geom_jitter(alpha = 1)

## Preprocessing ----
# To remove NA column:
ExistingProductsData$BestSellersRank <- NULL

## Feature Engineering ----
# To create new features/columns:
ExistingProductsData["StarWeighted"] <- NA
ExistingProductsData["TotalStarReviews"] <- NA
ExistingProductsData$StarWeighted <- as.numeric(ExistingProductsData$StarWeighted)
ExistingProductsData$TotalStarReviews <- as.numeric(ExistingProductsData$TotalStarReviews)
# To calculate total and weighted mean of star reviews for the new columns:
ExistingProductsData$StarWeighted <- (ExistingProductsData$x5StarReviews*5 +
                                        ExistingProductsData$x4StarReviews*4 +
                                        ExistingProductsData$x3StarReviews*3 +
                                        ExistingProductsData$x2StarReviews*2 +
                                        ExistingProductsData$x1StarReviews*1) / rowSums(ExistingProductsData[4:8])
ExistingProductsData$TotalStarReviews <- rowSums(ExistingProductsData[4:8])

## Dummifing The Data ----
# To give binary values to every product type:
ExProdDummy <- dummyVars(" ~ .", data = ExistingProductsData)
ExProdReady <- data.frame(predict(ExProdDummy, newdata = ExistingProductsData))

## Feature Selection ----
# To look certain correlations for PC, Laptop, Netbook and Smartphone:
ExProdReady[,c(1:4, 8, 9, 11:13, 23:27)] <- list(NULL)
names(ExProdReady) <- c("Laptop","Netbook","PC","Smartphone","Price","x5Star","x4Star","x3Star",
                        "x2Star","x1Star","PositiveServ","NegativeServ","Recommend","Volume",
                        "StarWeighted","TotalStarRev")
corrData <- cor(ExProdReady)
corrplot(corrData)
corrData
