setwd("/Users/macbook/ml_lab/Heesuk")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
tot.df <- rbind(train[,-81], test)

### 1. Basic Analysis
dim(tot.df)
summary(tot.df)
sapply(tot.df, class)
sapply(tot.df, FUN = function(x) sum(is.na(x)))

library(dplyr)
library(ggplot2)
library(corrplot)

## Y Distribution (training set)
ggplot(train, aes(x=SalePrice)) + geom_histogram(binwidth = 10000, fill="black")
  # --> Not well distributed ==> Log
ggplot(train, aes(x=log(SalePrice))) + 
  geom_histogram(binwidth = 0.05, color="black", fill="darkblue")
train$log.SalePrice <- log(train$SalePrice)

## Correlation (training set)
num.var <- train[sapply(train, is.numeric)]
fac.var <- train[sapply(train, is.factor)]
# col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
#                            "#007FFF", "blue", "#00007F"))
  
var.cor <- cor(num.var,  use="complete.obs")

corrplot.mixed(var.cor, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")

