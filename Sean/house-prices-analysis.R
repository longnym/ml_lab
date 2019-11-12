setwd('~/nyc_data_science/ml_lab/Sean')

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

# impute NA's of GarageYrBlt 
tot.df$GarageYrBlt <- ifelse((is.na(tot.df$GarageYrBlt) & !is.na(tot.df$GarageType)), tot.df$YearBuilt, tot.df$GarageYrBlt)

# impute NA's of SaleType
tot.df[is.na(tot.df$SaleType),]$SaleType <- 'Oth'

# impute NA's of GarageFinish, GarageCars, GarageArea
tot.df[!is.na(tot.df$GarageType) & is.na(tot.df$GarageFinish),]$GarageFinish <- 'Unf' # has GarageYrBlt
tot.df[is.na(tot.df$GarageCars),]$GarageArea <- 0
tot.df[is.na(tot.df$GarageCars),]$GarageCars <- 0

# impute NA's of Functional
tot.df[is.na(tot.df$Functional),]$Functional <- 'Typ' # most frequent

# impute NA's of KitchenQual
tot.df[is.na(tot.df$KitchenQual),]$KitchenQual <- 3 # median

# impute NA's of BsmtFullBath, BsmtHalfBath
tot.df[is.na(tot.df$BsmtFullBath),]$BsmtFullBath <- 0
tot.df[is.na(tot.df$BsmtHalfBath),]$BsmtHalfBath <- 0

# impute NA's of Electrical
tot.df[is.na(tot.df$Electrical),]$Electrical <- 'FuseA' # most frequent

# impute NA's of BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF
tot.df[is.na(tot.df$TotalBsmtSF),]$TotalBsmtSF <- 0 # BsmtFinType1 = NA, BsmtFinType2 = NA
tot.df[is.na(tot.df$BsmtUnfSF),]$BsmtUnfSF <- 0
tot.df[is.na(tot.df$BsmtFinSF1),]$BsmtFinSF1 <- 0
tot.df[is.na(tot.df$BsmtFinSF2),]$BsmtFinSF2 <- 0

# impute NA's of Exterior1st, Exterior2st
levels(tot.df$Exterior1st) <- c(levels(tot.df$Exterior1st), 'Other')
tot.df[is.na(tot.df$Exterior1st),]$Exterior1st <- 'Other'
tot.df[is.na(tot.df$Exterior2nd),]$Exterior2nd <- 'Other'

# impute NA's of Utilities
tot.df[is.na(tot.df$Utilities),]$Utilities <- 'AllPub'  # most frequent

# impute NA's of MSZoning
tot.df[is.na(tot.df$MSZoning),]$MSZoning <- 'RL'  # most frequent

# NA to None
levels(tot.df$Alley) <- c(levels(tot.df$Alley), 'None')
levels(tot.df$GarageType) <- c(levels(tot.df$GarageType), 'None')
levels(tot.df$GarageFinish) <- c(levels(tot.df$GarageFinish), 'None')
levels(tot.df$Fence) <- c(levels(tot.df$Fence), 'None')
levels(tot.df$MiscFeature) <- c(levels(tot.df$MiscFeature), 'None')

tot.df[is.na(tot.df$Alley),]$Alley <- 'None'
tot.df[is.na(tot.df$GarageType),]$GarageType <- 'None'
tot.df[is.na(tot.df$GarageFinish),]$GarageFinish <- 'None'
tot.df[is.na(tot.df$Fence),]$Fence <- 'None'
tot.df[is.na(tot.df$MiscFeature),]$MiscFeature <- 'None'



######## TEST

# drop columns
tot.df <- tot.df %>% select (-c(Condition2, BldgType, HouseStyle, Exterior1st, Exterior2nd, GarageFinish))

library(caret)
dummy <- dummyVars( ~ ., data=tot.df, fullRank=T)
tot.df.dummy <- data.frame(predict(dummy, newdata = tot.df))

# select only numeric variables
library(dplyr)
train <- tot.df.dummy[1:1460,]

# check correlation
library(corrplot)
corrplot.mixed(cor(train, use='complete.obs'), number.cex = 0.4, tl.cex=0.7, tl.pos = 'lt', order = 'hclust')

# divide train & test
set.seed(0)
index <- sample(1:nrow(train), nrow(train) * 0.9)

train_new <- train[index,]
test_new <- train[-index,]

train_new <- train_new %>% select_if(function(x) !any(is.na(x)))
test_new <- test_new %>% select_if(function(x) !any(is.na(x)))

# multi linear regression model

#### singularities
# GrLivArea = X1stFlrSF + X2ndFlrSF + LowQualFinSF
# TotalBsmtSF = BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF

# BldgTypeDuplex = MSSubClass is 90 (52 row)
# HouseStyle1.5Unf = ? (14 row)
# Exterior2ndCBlock = Exterior1ndCBlock (1 row)

sale_model <- lm(SalePrice ~ . - Id - X1stFlrSF - X2ndFlrSF - LowQualFinSF - BsmtFinSF1 - BsmtFinSF2 - BsmtUnfSF, data=train_new)
sale_model <- update(sale_model, . ~ . - MSSubClass.150)

summary(sale_model)

require(car)
vif(sale_model)

test_new$Predict <- predict(sale_model, newdata=test_new)

# RMSE
sqrt(sum((test_new$Predict - test_new$SalePrice) ^ 2) / nrow(test_new))

# MAE
sum(abs(test_new$Predict - test_new$SalePrice)) / nrow(test_new)
