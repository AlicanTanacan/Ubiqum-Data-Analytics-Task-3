# Module 2 - Task 3: Multiple Regression in R
# Alican Tanaçan
# Version 1 - Task Exploration Before Preprocessing

## Libraries ----
library(caret)
library(ggplot2)
library(corrplot)

## Import Data ----
ExistingProductsData <- read.csv("existingproductattributes2017.csv")

## Preprocessing ----
# To remove NA column:
ExistingProductsData$BestSellersRank <- NULL
# To give binary values to every product type:
ExProdDummy <- dummyVars(" ~ .", data = ExistingProductsData)
ExProdReady <- data.frame(predict(ExProdDummy, newdata = ExistingProductsData))
# To see the correlation between all variables:
corrData <- cor(ExProdReady)
corrplot(corrData, is.corr = FALSE)
corrData
# To look certain correlations for PC, Laptop, Netbook and Smartphone:
ExProdReady[,c(1:4, 8:9, 11:13)] <- list(NULL)
corrData <- cor(ExProdReady)
corrplot(corrData)
corrData

## ggplot ----
ggplot(ExProdReady, aes(Volume, Price,
                        color = as.factor(ProductType.Laptop)))+geom_jitter(alpha = 1)
ggplot(ExProdReady, aes(Volume, PositiveServiceReview,
                        color = as.factor(ProductType.Netbook)))+geom_jitter(alpha = 1)
ggplot(ExProdReady, aes(Volume, PositiveServiceReview,
                        color = as.factor(ProductType.PC)))+geom_jitter(alpha = 1)
ggplot(ExProdReady, aes(Volume, PositiveServiceReview,
                        color = as.factor(ProductType.Smartphone)))+geom_jitter(alpha = 1)