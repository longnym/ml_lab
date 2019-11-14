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

df.train$tot.space <- (df.train$TotalBsmtSF + df.train$GrLivArea + df.train$GarageArea)
df.test$tot.space <- (df.test$TotalBsmtSF + df.test$GrLivArea + df.test$GarageArea)
tot.df$tot.space <- (tot.df$TotalBsmtSF + tot.df$GrLivArea + tot.df$GarageArea)

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

# divide train & validation
set.seed(0)
index <- sample(1:nrow(df.train.all), nrow(df.train.all) * 0.8)

train_new <- tot.df.all[1:nrow(df.train.all),][index,]
validation_new <- tot.df.all[1:nrow(df.train.all),][-index,]
