train <- read.csv('train.csv')

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
      index <- which(codes[[column]] == 'NA')
    } else {
      index <- which(codes[[column]] == table[i,column])
    }
    result <- c(result, c(index))
  }
  result
}

# change factor to category(ordinal)
for (n in names(codes)) {
  train[n] <- codeToNum(train, n)
}

# change numeric to factor
train$MSSubClass = as.factor(train$MSSubClass)

# Impute NA's of MasVnrType, MasVnrArea
train[is.na(train$MasVnrType),]$MasVnrArea <- 0
train[is.na(train$MasVnrType),]$MasVnrType <- 'None'

# impute NA's of LotFrantage (Simple Linier Regression)
train_lf_missing <- train[is.na(train$LotFrontage),]
train_lf_no_missing <- train[!is.na(train$LotFrontage),]

lf_model <- lm(LotFrontage ~ LotArea, data=train_lf_no_missing)
lf_predict <- predict(lf_model, newdata=train_lf_missing)

train[is.na(train$LotFrontage),]$LotFrontage <- as.integer(lf_predict)

library(dplyr)
train_numeric <- train %>% select_if(is.numeric)

library(corrplot)
corrplot.mixed(cor(train_numeric, use='complete.obs'), number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")
