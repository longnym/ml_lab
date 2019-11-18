library(dplyr)

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

# GarageYrBlt NA --> impute by 'YearBuilt'
tot.df <- tot.df %>% transform(., GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))

tot.df$log.SalePrice <- log(tot.df$SalePrice)

rm(codes, lf_missing, lf_no_missing, lf_model, lf_predict, n, train, test)

library(caret)

df.train <- tot.df[1:1460,]
df.test <- tot.df[1461:2919,]

## Outlier Removal
tot.df <- subset(tot.df, !(tot.df$Id %in% c('524', '1299', '582')))
df.train <- subset(df.train, !(df.train$Id %in% c('524', '1299', '582')))

# multicollinearity : X1stFlrSF, X2ndFlrSF, LowQualFinSF, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF
single.issue <- c('X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF')
df.train[single.issue] = NULL
df.test[single.issue] = NULL
tot.df[single.issue] = NULL

# singularity : BldgType, GarageFinish, HouseStyle, Exterior2nd, 
single.issue <- c('BldgType', 'GarageFinish', 'HouseStyle', 'Exterior2nd')
df.train[single.issue] = NULL
df.test[single.issue] = NULL
tot.df[single.issue] = NULL

df.train$tot.space <- (df.train$TotalBsmtSF + 5 * df.train$GrLivArea + df.train$GarageArea)
df.test$tot.space <- (df.test$TotalBsmtSF + 5 * df.test$GrLivArea + df.test$GarageArea)
tot.df$tot.space <- (tot.df$TotalBsmtSF + 5 * tot.df$GrLivArea + tot.df$GarageArea)

df.train$built.period <- (df.train$YrSold - df.train$YearBuilt)
df.test$built.period <- (df.test$YrSold - df.test$YearBuilt)
tot.df$built.period <- (tot.df$YrSold - tot.df$YearBuilt)

df.train$remod.period <- (df.train$YrSold - df.train$YearRemodAdd)
df.test$remod.period <- (df.test$YrSold - df.test$YearRemodAdd)
tot.df$remod.period <- (tot.df$YrSold - tot.df$YearRemodAdd)

# 파생변수와 동일한 feature deletion 
same.as.new.feature <- c("TotalBsmtSF", "GrLivArea", "GarageArea","YearBuilt", "YrSold","YearRemodAdd")
df.train[same.as.new.feature] = NULL
df.test[same.as.new.feature] = NULL
tot.df[same.as.new.feature] = NULL

multico <- c("ExterQual", "BsmtQual", "FireplaceQu","GarageCond", "PoolQC")
df.train[multico] = NULL
df.test[multico] = NULL
tot.df[multico] = NULL

clear <- c("SalePrice", "Id")
df.train[clear] = NULL
df.test[clear] = NULL
tot.df[clear] = NULL

# convert factor to dummy
dummy <- dummyVars( ~ ., data=tot.df, fullRank=T)
tot.df.all <- data.frame(predict(dummy, newdata = tot.df))

df.train.all <- tot.df.all[1:1457,]
df.test.all <- tot.df.all[1458:2916,]

train.sd.issue <- c('MSSubClass.150', 'Exterior1st.Other')
test.sd.issue <- c('Utilities.NoSeWa', 'Condition2.RRAe', 'Condition2.RRAn', 'Condition2.RRNn',
                   'RoofMatl.Membran', 'RoofMatl.Metal', 'RoofMatl.Roll', 'Exterior1st.ImStucc',
                   'Exterior1st.Stone', 'Heating.OthW', 'Electrical.Mix', 'MiscFeature.TenC')

df.train.all[train.sd.issue] = NULL
tot.df.all[train.sd.issue] = NULL
df.test.all[train.sd.issue] = NULL

df.train.all[test.sd.issue] = NULL
tot.df.all[test.sd.issue] = NULL
df.test.all[test.sd.issue] = NULL

remove(df.test, df.train, tot.df, dummy, multico, clear, same.as.new.feature, single.issue, test.sd.issue, train.sd.issue)

tier.top <- c('tot.space','OverallQual','KitchenQual','built.period','remod.period','Fireplaces')
tier.2nd <- c('LotArea','OverallCond','CentralAir.Y','FullBath','GarageCars','GarageYrBlt','LotFrontage','MasVnrArea','MSSubClass.60','BsmtFinType1','TotRmsAbvGrd','HeatingQC','OpenPorchSF','MSZoning.RL','MSZoning.RM','GarageType.Attchd','HalfBath','BsmtFullBath','Foundation.PConc')
tier.3rd <- c('BsmtExposure','WoodDeckSF','Neighborhood.Crawfor','GarageQual','MSSubClass.30','GarageType.Detchd','BedroomAbvGr','LotShape.Reg','Neighborhood.IDOTRR','SaleCondition.Normal','Neighborhood.OldTown','SaleType.New','Exterior1st.VinylSd','ScreenPorch','SaleCondition.Family','Fence.None','KitchenAbvGr','LotConfig.Inside','MoSold','MasVnrType.None','Neighborhood.NridgHt')
tier.4th <- c('EnclosedPorch','BsmtCond','Foundation.CBlock','GarageType.None','ExterCond','Exterior1st.BrkFace','PavedDrive.Y','RoofStyle.Gable','Neighborhood.BrkSide','Exterior1st.Wd.Sdng','Functional.Typ','SaleType.WD','SaleCondition.Partial','Neighborhood.NAmes','MSSubClass.50','Exterior1st.MetalSd','Electrical.SBrkr','LotConfig.CulDSac','Condition1.Norm','MSSubClass.160','Neighborhood.MeadowV','Neighborhood.StoneBr')

tot.df.all <- tot.df.all %>% select(c(c('log.SalePrice'), tier.top, tier.2nd))

# divide train & validation
set.seed(0)
index <- sample(1:nrow(df.train.all), nrow(df.train.all) * 0.8)

train_new <- tot.df.all[1:nrow(df.train.all),][index,]
validation_new <- tot.df.all[1:nrow(df.train.all),][-index,]

# XGBoost
library(xgboost)

dtrain <- xgb.DMatrix(data=as.matrix(train_new %>% select(-log.SalePrice)), label=train_new$log.SalePrice, missing=NA)
dtest <- xgb.DMatrix(data=as.matrix(validation_new %>% select(-log.SalePrice)), missing=NA)

min.rmse <- c()
nrounds <- c()
param_grid <- expand.grid(
  max_depth = c(9), # default: 6
  min_child_weight = c(1), # default: 1
  gamma = c(0.3), # default: 0
  eta = c(1.0) # default : 0.3
)

for(i in 1:nrow(param_grid)) {
  print(paste0(i,' iteration of ', nrow(param_grid)))
  param <- list(
    booster = 'gblinear',
    objective = 'reg:squarederror',
    eval_metric = 'rmse',
    gamma = param_grid$gamma[i],
    eta = param_grid$eta[i],
    max_depth = param_grid$max_depth[i],
    min_child_weight = param_grid$min_child_weight[i]
  )
  foldsCV <- createFolds(train_new$log.SalePrice, k=10, list=TRUE, returnTrain=FALSE)
  xgb_cv <- xgb.cv(data = dtrain, params = param, nrounds = 2000, prediction = TRUE, maximize = FALSE,
                   folds = foldsCV, print_every_n = 100)
  
  min.rmse[i] <- min(xgb_cv$evaluation_log$test_rmse_mean)
  nrounds[i] <- min(which(xgb_cv$evaluation_log$test_rmse_mean == min(xgb_cv$evaluation_log$test_rmse_mean)))
}

best <- which(min.rmse == min(min.rmse))

param.new <- list(booster = "gblinear",
                  objective = "reg:squarederror",
                  eval_metric = 'rmse',
                  gamma = param_grid$gamma[best],
                  eta = param_grid$eta[best],
                  max_depth = param_grid$max_depth[best],
                  min_child_weight = param_grid$min_child_weight[best])
xgb.mdl <- xgb.train(data = dtrain, nround=nrounds[best], params = param.new)
pred1 <- predict(xgb.mdl, dtest)
pred1 <- exp(pred1)


# GBM
library(gbm)

set.seed(0)
boost.model <- gbm(log.SalePrice ~ ., data = train_new,
                   distribution = "gaussian",
                   cv.folds = 10,
                   n.trees = 1000,
                   interaction.depth = 4)

pred2 <- predict(boost.model, newdata = validation_new, n.trees = 150)
pred2 <- exp(pred2)


# SVM
library(e1071)

svm.model = svm(log.SalePrice ~ ., data = train_new, kernel = 'linear')

pred3 = predict(svm.model, validation_new %>% select(-log.SalePrice))
pred3 <- exp(pred3)


# Linear
library(caret)

# multi linear regression model
sale_model <- train(log.SalePrice ~ ., train_new, method = "lm",
                    trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))

pred4 <- predict(sale_model, newdata=validation_new)
pred4 <- exp(pred4)


# Random Forest
library(randomForest)

set.seed(0)
rf.model = randomForest(log.SalePrice ~ ., data = train_new)

pred5 = predict(rf.model, validation_new, type = "class")
pred5 <- exp(pred5)


pred <- (pred1 + pred2 + pred5) / 3

library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), pred)

# RMSE
rmse(exp(validation_new$log.SalePrice), pred)

# MAE
mae(exp(validation_new$log.SalePrice), pred)