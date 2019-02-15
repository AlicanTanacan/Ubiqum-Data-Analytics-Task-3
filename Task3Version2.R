# Module 2 - Task 3: Multiple Regression in R
# Alican Tanaçan
# Version 2 - Creating Sample For Assessment Of "PC, Laptop, Netbook, Smartphone"

## Libraries ----
library(caret)
library(ggplot2)
library(corrplot)
library(tidyverse)

## Import Data ----
ExistingProductsData <- read.csv("existingproductattributes2017.csv")

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

## Creating Sample For PC, Laptop, Netbook, Smartphone ----
levels(ExistingProductsData$ProductType)
ExProdSample <- ExistingProductsData %>% 
  filter(ProductType == "PC" |
           ProductType == "Laptop" |
           ProductType == "Netbook" |
           ProductType == "Smartphone")
ExProdSample$ProductType <- factor(ExProdSample$ProductType)
# Removing column with NA's:
ExProdSample$BestSellersRank <- NULL

## Plotting ----
plot(ExProdSample$ProductType, ExProdSample$x5StarReviews)
plot(ExProdSample$ProductType, ExProdSample$x4StarReviews)
plot(ExProdSample$ProductType, ExProdSample$x3StarReviews)
plot(ExProdSample$ProductType, ExProdSample$x2StarReviews)
plot(ExProdSample$ProductType, ExProdSample$x1StarReviews)
plot(ExProdSample$ProductType, ExProdSample$Volume)
plot(ExProdSample$ProductType, ExProdSample$NegativeServiceReview)
plot(ExProdSample$ProductType, ExProdSample$PositiveServiceReview)

## Dummifing The Data ----
# To give binary values to every product type:
ExProdDummy <- dummyVars(" ~ .", data = ExProdSample)
ExProdReady <- data.frame(predict(ExProdDummy, newdata = ExProdSample))

## Feature Selection ----
# To look certain correlations for PC, Laptop, Netbook and Smartphone:
ExProdReady[,c(5, 15:19)] <- list(NULL)
names(ExProdReady) <- c("Laptop","Netbook","PC","Smartphone","Price","x5Star","x4Star","x3Star",
                        "x2Star","x1Star","PositiveServ","NegativeServ","Recommend","Volume",
                        "StarWeighted","TotalStarRev")
corrData <- cor(ExProdReady)
corrplot(corrData)
corrData
