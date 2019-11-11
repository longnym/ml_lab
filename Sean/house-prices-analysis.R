setwd("~/nyc_data_science/ml_lab/Sean")

# load train and test data
train <- read.csv('train.csv')
test <- read.csv('test.csv')
test$SalePrice <- NA
tot.df <- rbind(train, test)

# define code
codes <- list()
codes[['ExterQual']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['ExterCond']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['BsmtQual']] <- c('NA', 'Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['BsmtCond']] <- c('NA', 'Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['BsmtExposure']] <- c('NA', 'No', 'Mn', 'Av', 'Gd')
codes[['BsmtFinType1']] <- c('NA', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ')
codes[['BsmtFinType2']] <- c('NA', 'Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ')
codes[['HeatingQC']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['KitchenQual']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['FireplaceQu']] <- c('NA', 'Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['GarageQual']] <- c('NA', 'Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['GarageCond']] <- c('NA', 'Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['PoolQC']] <- c('NA', 'Fa', 'TA', 'Gd', 'Ex')

codeToNum <- function(table, column) {
  result <- c()
  for (i in 1:nrow(table)) {
    if (is.na(table[i,column])) {
      if ('NA' %in% codes[[column]]) {  # if NA exist in the code
        index <- which(codes[[column]] == 'NA')
      } else {  # if NA doesn't exist in the code (only one case of KitchenQual)
        index <- NA
      }
    } else {
      index <- which(codes[[column]] == table[i,column])
    }
    result <- c(result, c(index))
  }
  result
}

# change factor to category(ordinal)
for (n in names(codes)) {
  tot.df[n] <- codeToNum(tot.df, n)
}

# change numeric to factor
tot.df$MSSubClass = as.factor(tot.df$MSSubClass)

# Impute NA's of MasVnrType, MasVnrArea
tot.df[is.na(tot.df$MasVnrType),]$MasVnrArea <- 0
tot.df[is.na(tot.df$MasVnrType),]$MasVnrType <- 'None'

# impute NA's of LotFrantage (Simple Linier Regression)
lf_missing <- tot.df[is.na(tot.df$LotFrontage),]
lf_no_missing <- tot.df[!is.na(tot.df$LotFrontage),]

lf_model <- lm(LotFrontage ~ LotArea, data=lf_no_missing)
lf_predict <- predict(lf_model, newdata=lf_missing)

tot.df[is.na(tot.df$LotFrontage),]$LotFrontage <- as.integer(lf_predict)

######## TEST
# select only numeric variables
library(dplyr)
tot.df.test <- tot.df %>% select_if(is.numeric)

cor(tot.df.test)

# check correlation
library(corrplot)
corrplot.mixed(cor(train, use='complete.obs'), number.cex = 0.4, tl.cex=0.7, tl.pos = 'lt', order = 'hclust')

# divide train & test
set.seed(0)
index <- sample(1:nrow(train), nrow(train) * 0.8)

train_new <- train[index,]
test_new <- train[-index,]

# multi linear regression model
sample_submission <- lm(SalePrice ~ YrSold + MoSold + LotArea + BedroomAbvGr, data=train_new)
sale_model <- lm(SalePrice ~ . - GrLivArea - TotalBsmtSF - GarageYrBlt, data=train_new)

plot(sample_submission)

require(car)
summary(sale_model)
vif(sale_model)

test_new$Predict <- predict(sale_model, newdata=test_new)

library(Metrics)
rmse(test_new$SalePrice, test_new$Predict)
mae(test_new$SalePrice, test_new$Predict)
