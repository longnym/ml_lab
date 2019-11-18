setwd("/Users/macbook/ml_lab/Heesuk")
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(doMC)
library(xgboost)

df.train.all <- read.csv("df.train.all.csv")
df.train.all <- df.train.all[,-1]

############################# I. use all features ############################# 

# parallel enable : core 1 -> 4
registerDoMC(cores = 4)
getDoParWorkers()

## use Caret createDataPartition
indexTrain <- createDataPartition(df.train.all$log.SalePrice, 
                                  p=.8, list=F)
train.caret.init <- df.train.all[indexTrain, ]
test.caret.init <- df.train.all[-indexTrain, ]
# grid search control (10-fold CV, 5 repeat, parameter grid search) 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
####### SCALING or not~~~ !!!


# 1) RF Prediction test
# fitting
rf_fit.init <- train(log.SalePrice ~ ., data = train.caret.init,
                     method = "rf", trControl = fitControl, verbose = F)
rf_fit.init
rf.pred.init <- predict(rf_fit.init, newdata = test.caret.init)
rf.rmse_log.init = sqrt(mean((rf.pred.init - test.caret.init$log.SalePrice)^2))
rf.rmse_ori.init = sqrt(mean((exp(rf.pred.init) - exp(test.caret.init$log.SalePrice))^2))
rf.rmse_ori.init

# 2) GBM 
# fitting
gbm_fit.init <- train(log.SalePrice ~ ., data = train.caret.init,
                      method = "gbm", trControl = fitControl, verbose = F)
gbm_fit.init
gbm.pred.init <- predict(gbm_fit.init, newdata = test.caret.init)
gbm.rmse_log.init = sqrt(mean((gbm.pred.init - test.caret.init$log.SalePrice)^2))
gbm.rmse_ori.init = sqrt(mean((exp(gbm.pred.init) - exp(test.caret.init$log.SalePrice))^2))
gbm.rmse_ori.init


# 3) SVM 
# fitting
svm_fit.init <- train(log.SalePrice ~ ., data = train.caret.init,
                      method = "svmLinear", trControl = fitControl, verbose = F)
svm_fit.init
svm.pred.init <- predict(svm_fit.init, newdata = test.caret.init)
svm.rmse_log.init = sqrt(mean((svm.pred.init - test.caret.init$log.SalePrice)^2))
svm.rmse_ori.init = sqrt(mean((exp(svm.pred.init) - exp(test.caret.init$log.SalePrice))^2))
svm.rmse_ori.init


# 4) nnet 
# fitting
nnet_fit.init <- train(log.SalePrice ~ ., data = train.caret.init,
                       method = "nnet", trControl = fitControl, verbose = F)
nnet_fit.init
nnet.pred.init <- predict(nnet_fit.init, newdata = test.caret.init)
nnet.rmse_log.init = sqrt(mean((nnet.pred.init - test.caret.init$log.SalePrice)^2))
nnet.rmse_ori.init = sqrt(mean((exp(nnet.pred.init) - exp(test.caret.init$log.SalePrice))^2))
nnet.rmse_ori.init


# 5) glm 
# fitting
# glm_fit.init <- train(log.SalePrice ~ ., data = train.caret.init,
#                        method = "glm", trControl = fitControl, verbose = F)
# glm_fit.init
# glm.pred.init <- predict(glm_fit.init, newdata = test.caret.init)
# glm.rmse_log.init = sqrt(mean((glm.pred.init - test.caret.init$log.SalePrice)^2))
# glm.rmse_ori.init = sqrt(mean((exp(glm.pred.init) - exp(test.caret.init$log.SalePrice))^2))
# glm.rmse_ori.init


# 6) PLS 
# fitting
pls_fit.init <- train(log.SalePrice ~ ., data = train.caret.init,
                      method = "pls", trControl = fitControl, verbose = F)
pls_fit.init
pls.pred.init <- predict(pls_fit.init, newdata = test.caret.init)
pls.rmse_log.init = sqrt(mean((pls.pred.init - test.caret.init$log.SalePrice)^2))
pls.rmse_ori.init = sqrt(mean((exp(pls.pred.init) - exp(test.caret.init$log.SalePrice))^2))
pls.rmse_ori.init

# 7) XGBoost
xgboost.init <- train(log.SalePrice ~ ., data = train.caret.init,
                      method = "xgbTree", trControl = fitControl, verbose = F)
xgboost.init
xgboost.pred.init <- predict(xgboost.init, newdata = test.caret.init)
xgboost.rmse_log.init = sqrt(mean((xgboost.pred.init - test.caret.init$log.SalePrice)^2))
xgboost.rmse_ori.init = sqrt(mean((exp(xgboost.pred.init) - exp(test.caret.init$log.SalePrice))^2))
xgboost.rmse_ori.init

#####################################################################################

## grouping by variable influence
# TopTier 	6,  2nd Tier 	19,   3rd Tier 	21,  4th Tier 	22,  Garbage 	94

top.tier <- c('tot.space', 'OverallQual', 'KitchenQual', 'built.period', 
              'remod.period', 'Fireplaces', 'log.SalePrice' )
second.tier <- c('LotArea', 'OverallCond', 'CentralAir.Y', 'FullBath', 'GarageCars', 
                 'GarageYrBlt', 'LotFrontage', 'MasVnrArea', 'MSSubClass.60', 
                 'BsmtFinType1', 'TotRmsAbvGrd', 'HeatingQC', 'OpenPorchSF', 'MSZoning.RL', 
                 'MSZoning.RM', 'GarageType.Attchd', 'HalfBath', 'BsmtFullBath', 
                 'Foundation.PConc')
third.tier <- c('BsmtExposure', 'WoodDeckSF', 'Neighborhood.Crawfor', 'GarageQual', 
                'MSSubClass.30', 'GarageType.Detchd', 'BedroomAbvGr', 'LotShape.Reg', 
                'Neighborhood.IDOTRR', 'SaleCondition.Normal','Neighborhood.OldTown', 
                'SaleType.New', 'Exterior1st.VinylSd', 'ScreenPorch', 
                'SaleCondition.Family', 'Fence.None', 'KitchenAbvGr', 
                'LotConfig.Inside', 'MoSold', 'MasVnrType.None', 'Neighborhood.NridgHt' )
forth.tier <- c('EnclosedPorch', 'BsmtCond', 'Foundation.CBlock', 'GarageType.None', 
                'ExterCond', 'Exterior1st.BrkFace', 'PavedDrive.Y','RoofStyle.Gable', 
                'Neighborhood.BrkSide', 'Exterior1st.Wd.Sdng', 'Functional.Typ', 
                'SaleType.WD', 'SaleCondition.Partial', 'Neighborhood.NAmes', 
                'MSSubClass.50', 'Exterior1st.MetalSd', 'Electrical.SBrkr', 
                'LotConfig.CulDSac', 'Condition1.Norm', 'MSSubClass.160', 
                'Neighborhood.MeadowV', 'Neighborhood.StoneBr')


################### II. use Feature selection (Top-Tier + 2nd Tier) ############################# 

train.1.2.tier <- df.train.all[, c(top.tier,second.tier) ]


## use Caret createDataPartition
indexTrain2 <- createDataPartition(train.1.2.tier$log.SalePrice, 
                                   p=.8, list=F)
train.caret.1.2.tier <- train.1.2.tier[indexTrain2, ]
test.caret.1.2.tier <- train.1.2.tier[-indexTrain2, ]
# grid search control (10-fold CV, 5 repeat, parameter grid search)
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
####### SCALING or not ~~~ !!!


# 1) RF Prediction test
# fitting
rf_fit.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
                         method = "rf", trControl = fitControl, verbose = F)
rf_fit.1.2.tier
rf.pred.1.2.tier <- predict(rf_fit.1.2.tier, newdata = test.caret.1.2.tier)
rf.rmse_log.1.2.tier = sqrt(mean((rf.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
rf.rmse_ori.1.2.tier = sqrt(mean((exp(rf.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
rf.rmse_ori.1.2.tier

# 2) GBM 
# fitting
gbm_fit.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
                          method = "gbm", trControl = fitControl, verbose = F)
gbm_fit.1.2.tier
gbm.pred.1.2.tier <- predict(gbm_fit.1.2.tier, newdata = test.caret.1.2.tier)
gbm.rmse_log.1.2.tier = sqrt(mean((gbm.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
gbm.rmse_ori.1.2.tier = sqrt(mean((exp(gbm.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
gbm.rmse_ori.1.2.tier


# 3) SVM 
# fitting
svm_fit.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
                          method = "svmLinear", trControl = fitControl, verbose = F)
svm_fit.1.2.tier
svm.pred.1.2.tier <- predict(svm_fit.1.2.tier, newdata = test.caret.1.2.tier)
svm.rmse_log.1.2.tier = sqrt(mean((svm.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
svm.rmse_ori.1.2.tier = sqrt(mean((exp(svm.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
svm.rmse_ori.1.2.tier


# 4) nnet 
# fitting
nnet_fit.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
                           method = "nnet", trControl = fitControl, verbose = F)
nnet_fit.1.2.tier
nnet.pred.1.2.tier <- predict(nnet_fit.1.2.tier, newdata = test.caret.1.2.tier)
nnet.rmse_log.1.2.tier = sqrt(mean((nnet.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
nnet.rmse_ori.1.2.tier = sqrt(mean((exp(nnet.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
nnet.rmse_ori.1.2.tier


# 5) glm 
# fitting
# glm_fit.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
#                        method = "glm", trControl = fitControl, verbose = F)
# glm_fit.1.2.tier
# glm.pred.1.2.tier <- predict(glm_fit.1.2.tier, newdata = test.caret.1.2.tier)
# glm.rmse_log.1.2.tier = sqrt(mean((glm.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
# glm.rmse_ori.1.2.tier = sqrt(mean((exp(glm.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
# glm.rmse_ori.1.2.tier



# 6) PLS 
# fitting
pls_fit.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
                          method = "pls", trControl = fitControl, verbose = F)
pls_fit.1.2.tier
pls.pred.1.2.tier <- predict(pls_fit.1.2.tier, newdata = test.caret.1.2.tier)
pls.rmse_log.1.2.tier = sqrt(mean((pls.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
pls.rmse_ori.1.2.tier = sqrt(mean((exp(pls.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
pls.rmse_ori.1.2.tier


# 7) XGBoost
xgboost.1.2.tier <- train(log.SalePrice ~ ., data = train.caret.1.2.tier,
                          method = "xgbTree", trControl = fitControl, verbose = F)
xgboost.1.2.tier
xgboost.pred.1.2.tier <- predict(xgboost.1.2.tier, newdata = test.caret.1.2.tier)
xgboost.rmse_log.1.2.tier = sqrt(mean((xgboost.pred.1.2.tier - test.caret.1.2.tier$log.SalePrice)^2))
xgboost.rmse_ori.1.2.tier = sqrt(mean((exp(xgboost.pred.1.2.tier) - exp(test.caret.1.2.tier$log.SalePrice))^2))
xgboost.rmse_ori.1.2.tier


#  LM validation 
lm.1.2.tier = lm(log.SalePrice ~ ., data = test.caret.1.2.tier)  
summary(lm.1.2.tier)
plot(lm.1.2.tier)
library(car) 
influencePlot(lm.1.2.tier) 
vif <- vif(lm.1.2.tier) 



### 2. LASSO #### 

# 2-1) dataset redefine (convert to Matrix)
x = model.matrix(log.SalePrice ~ ., train.1.2.tier)[, -1]  # -1 : remove Index
y = train.1.2.tier$log.SalePrice

set.seed(0)
train = sample(1:nrow(x), nrow(x)*0.8)
test = (-train)
y.test = y[test]

# 2-2) Fit a model:
grid = 10^seq(5, -2, length = 100)
library(glmnet)

lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

# 2-3) Visualization (Plot the coefficients)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

# 2-4) Cross Validation & Lambda visualization : 10-fold cross validation 
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")

# 2-5) Result : Best Lambda
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso       # 0.01
log(bestlambda.lasso)  # -4.60517

# 2-6) Fit a model: Fit a lasso regression, MSE check 
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)  # 0.0130576

# 2-7) Refit a model & Results:
lasso.best_refit = glmnet(x, y, alpha = 1)
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)

# MSE
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
MSE = mean((lasso.bestlambda - y)^2)  # 0.01427454
RMSE = sqrt(MSE) 

# MSE_ori
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
MSE_LASSO.1.2.tier = mean((exp(lasso.bestlambda) - exp(y))^2)  # 0.01427454
MSE_LASSO.1.2.tier_ori = sqrt(MSE_LASSO.1.2.tier)
MSE_LASSO.1.2.tier_ori



###  RIDGE #### 

# 2-2) Fit a model:
grid = 10^seq(5, -2, length = 100)
library(glmnet)

ridge.models = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)

# 2-3) Visualization (Plot the coefficients)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "ridge Regression")

# 2-4) Cross Validation & Lambda visualization : 10-fold cross validation 
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10, lambda = grid)
plot(cv.ridge.out, main = "ridge Regression\n")

# 2-5) Result : Best Lambda
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge       # 0.01
log(bestlambda.ridge)  # -4.60517

# 2-6) Fit a model: Fit a ridge regression, MSE check 
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)  # 0.0130576

# 2-7) Refit a model & Results:
ridge.best_refit = glmnet(x, y, alpha = 0)
predict(ridge.best_refit, type = "coefficients", s = bestlambda.ridge)

# MSE
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
MSE = mean((ridge.bestlambda - y)^2)  # 0.01427454
RMSE = sqrt(MSE) 

# MSE_ori
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
MSE_ridge.1.2.tier = mean((exp(ridge.bestlambda) - exp(y))^2)  # 0.01427454
MSE_ridge.1.2.tier_ori = sqrt(MSE_ridge.1.2.tier)
MSE_ridge.1.2.tier_ori


#####################################################################################



################### II. use Feature selection (Top-Tier only) ############################# 

train.1.tier <- df.train.all[, c(top.tier) ]


## use Caret createDataPartition
indexTrain3 <- createDataPartition(train.1.tier$log.SalePrice, 
                                   p=.8, list=F)
train.caret.1.tier <- train.1.tier[indexTrain3, ]
test.caret.1.tier <- train.1.tier[-indexTrain3, ]
# grid search control (10-fold CV, 5 repeat parameter grid search) 
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
####### SCALING or not~~~ !!!


# 1) RF Prediction test
# fitting
rf_fit.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
                       method = "rf", trControl = fitControl, verbose = F)
rf_fit.1.tier
rf.pred.1.tier <- predict(rf_fit.1.tier, newdata = test.caret.1.tier)
rf.rmse_log.1.tier = sqrt(mean((rf.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
rf.rmse_ori.1.tier = sqrt(mean((exp(rf.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
rf.rmse_ori.1.tier

# 2) GBM 
# fitting
gbm_fit.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
                        method = "gbm", trControl = fitControl, verbose = F)
gbm_fit.1.tier
gbm.pred.1.tier <- predict(gbm_fit.1.tier, newdata = test.caret.1.tier)
gbm.rmse_log.1.tier = sqrt(mean((gbm.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
gbm.rmse_ori.1.tier = sqrt(mean((exp(gbm.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
gbm.rmse_ori.1.tier


# 3) SVM 
# fitting
svm_fit.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
                        method = "svmLinear", trControl = fitControl, verbose = F)
svm_fit.1.tier
svm.pred.1.tier <- predict(svm_fit.1.tier, newdata = test.caret.1.tier)
svm.rmse_log.1.tier = sqrt(mean((svm.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
svm.rmse_ori.1.tier = sqrt(mean((exp(svm.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
svm.rmse_ori.1.tier


# 4) nnet 
# fitting
nnet_fit.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
                         method = "nnet", trControl = fitControl, verbose = F)
nnet_fit.1.tier
nnet.pred.1.tier <- predict(nnet_fit.1.tier, newdata = test.caret.1.tier)
nnet.rmse_log.1.tier = sqrt(mean((nnet.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
nnet.rmse_ori.1.tier = sqrt(mean((exp(nnet.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
nnet.rmse_ori.1.tier


# 5) glm 
# fitting
# glm_fit.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
#                        method = "glm", trControl = fitControl, verbose = F)
# glm_fit.1.tier
# glm.pred.1.tier <- predict(glm_fit.1.tier, newdata = test.caret.1.tier)
# glm.rmse_log.1.tier = sqrt(mean((glm.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
# glm.rmse_ori.1.tier = sqrt(mean((exp(glm.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
# glm.rmse_ori.1.tier



# 6) PLS 
# fitting
pls_fit.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
                        method = "pls", trControl = fitControl, verbose = F)
pls_fit.1.tier
pls.pred.1.tier <- predict(pls_fit.1.tier, newdata = test.caret.1.tier)
pls.rmse_log.1.tier = sqrt(mean((pls.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
pls.rmse_ori.1.tier = sqrt(mean((exp(pls.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
pls.rmse_ori.1.tier


# 7) XGBoost
xgboost.1.tier <- train(log.SalePrice ~ ., data = train.caret.1.tier,
                        method = "xgbTree", trControl = fitControl, verbose = F)
xgboost.1.tier
xgboost.pred.1.tier <- predict(xgboost.1.tier, newdata = test.caret.1.tier)
xgboost.rmse_log.1.tier = sqrt(mean((xgboost.pred.1.tier - test.caret.1.tier$log.SalePrice)^2))
xgboost.rmse_ori.1.tier = sqrt(mean((exp(xgboost.pred.1.tier) - exp(test.caret.1.tier$log.SalePrice))^2))
xgboost.rmse_ori.1.tier



#  LM validation
lm.1.tier = lm(log.SalePrice ~ ., data = train.caret.1.tier)  
summary(lm.1.tier)
plot(lm.1.tier)
library(car) 
influencePlot(lm.1.tier) 
vif <- vif(lm.1.tier) 



### 2. LASSO #### 

# 2-1) dataset redefine (convert to Matrix)
x = model.matrix(log.SalePrice ~ ., train.1.tier)[, -1]  # -1 : remove Index
y = train.1.tier$log.SalePrice

set.seed(0)
train = sample(1:nrow(x), nrow(x)*0.8)
test = (-train)
y.test = y[test]

# 2-2) Fit a model:
grid = 10^seq(5, -2, length = 100)
library(glmnet)

lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

# 2-3) Visualization (Plot the coefficients)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

# 2-4) Cross Validation & Lambda visualization : 10-fold cross validation 
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")

# 2-5) Result : Best Lambda 
bestlambda.lasso = cv.lasso.out$lambda.min
bestlambda.lasso       # 0.01
log(bestlambda.lasso)  # -4.60517

# 2-6) Fit a model: Fit a lasso regression, MSE check 
lasso.bestlambdatrain = predict(lasso.models, s = bestlambda.lasso, newx = x[test, ])
mean((lasso.bestlambdatrain - y.test)^2)  # 0.0130576

# 2-7) Refit a model & Results:
lasso.best_refit = glmnet(x, y, alpha = 1)
predict(lasso.best_refit, type = "coefficients", s = bestlambda.lasso)

# MSE
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
MSE = mean((lasso.bestlambda - y)^2)  # 0.01427454
RMSE = sqrt(MSE) 

# MSE_ori
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
MSE_LASSO.1.tier = mean((exp(lasso.bestlambda) - exp(y))^2)  # 0.01427454
MSE_LASSO.1.tier_ori = sqrt(MSE_LASSO.1.tier)
MSE_LASSO.1.tier_ori



###  RIDGE #### 

# 2-2) Fit a model:
grid = 10^seq(5, -2, length = 100)
library(glmnet)

ridge.models = glmnet(x[train, ], y[train], alpha = 0, lambda = grid)

# 2-3) Visualization (Plot the coefficients)
plot(ridge.models, xvar = "lambda", label = TRUE, main = "ridge Regression")

# 2-4) Cross Validation & Lambda visualization: 10-fold cross validation 
set.seed(0)
cv.ridge.out = cv.glmnet(x[train, ], y[train], alpha = 0, nfolds = 10, lambda = grid)
plot(cv.ridge.out, main = "ridge Regression\n")

# 2-5) Result : Best Lambda
bestlambda.ridge = cv.ridge.out$lambda.min
bestlambda.ridge       # 0.01
log(bestlambda.ridge)  # -4.60517

# 2-6) Fit a model: Fit a ridge regression, MSE check 
ridge.bestlambdatrain = predict(ridge.models, s = bestlambda.ridge, newx = x[test, ])
mean((ridge.bestlambdatrain - y.test)^2)  # 0.0130576

# 2-7) Refit a model & Results:
ridge.best_refit = glmnet(x, y, alpha = 0)
predict(ridge.best_refit, type = "coefficients", s = bestlambda.ridge)

# MSE
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
MSE = mean((ridge.bestlambda - y)^2)  # 0.01427454
RMSE = sqrt(MSE) 

# MSE_ori
ridge.bestlambda = predict(ridge.best_refit, s = bestlambda.ridge, newx = x)
MSE_ridge.1.tier = mean((exp(ridge.bestlambda) - exp(y))^2)  # 0.01427454
MSE_ridge.1.tier_ori = sqrt(MSE_ridge.1.tier)
MSE_ridge.1.tier_ori
