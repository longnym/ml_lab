############################# 3. Feature Enginnering ############################

## 3.1. Feature Creation

# 1) House space feature
# Total space
# - TotalBsmtSF (Total square feet of basement area)
# - GrLivArea (Above grade (ground) living area square feet)
# - GarageArea (Size of garage in square feet)
df.train$tot.space <- (df.train$TotalBsmtSF + df.train$GrLivArea + df.train$GarageArea)
df.test$tot.space <- (df.test$TotalBsmtSF + df.test$GrLivArea + df.test$GarageArea)
tot.df$tot.space <- (tot.df$TotalBsmtSF + tot.df$GrLivArea + tot.df$GarageArea)

# Correlation check
comp <- c("tot.space", "TotalBsmtSF", "GrLivArea", "GarageArea")
cor(df.train[comp], df.train$log.SalePrice)

## Correlation of tot.spacec is higher than other 3 variables.
# tot.space   0.8528168
# TotalBsmtSF 0.6473571
# GrLivArea   0.7250041
# GarageArea  0.6575029

# -> 3 variables will be deleted (train/test/total)


# 2) Period feature
# period variable (how many year passed since house is built...)
# - YearBuilt (Original construction date)
# - YearRemodAdd (Remodel date (same as construction date if no remodeling or additions))
# - MoSold (Month Sold (MM)),  YrSold (Year Sold (YYYY))
df.train$built.period <- (df.train$YrSold - df.train$YearBuilt)
df.test$built.period <- (df.test$YrSold - df.test$YearBuilt)
tot.df$built.period <- (tot.df$YrSold - tot.df$YearBuilt)
filter(df.train, built.period<0) 
filter(df.test, built.period<0)  # 1 row
filter(tot.df, built.period<0)  # 1 row

df.train$remod.period <- (df.train$YrSold - df.train$YearRemodAdd)
df.test$remod.period <- (df.test$YrSold - df.test$YearRemodAdd)
tot.df$remod.period <- (tot.df$YrSold - tot.df$YearRemodAdd)
filter(df.train, remod.period<0) 
filter(df.test, remod.period<0)  # 2 row
filter(tot.df, remod.period<0)  # 2 row

# Corr check
comp2 <- c("built.period", "YearBuilt", "YrSold", "remod.period", "YearRemodAdd")
cor(df.train[comp2], df.train$log.SalePrice)

## Correlation of built.period & remod.period is a little higer than others.
# built.period -0.58742118
# YearBuilt     0.58668710
# YrSold       -0.03781576
# remod.period -0.56818381
# YearRemodAdd  0.56562961


# 3) Porch feature
# Porch space variables
# - Porch.space = 'OpenPorch' + 'EnclosedPorch' + 'X3SsnPorch' + 'ScreenPorch'
df.train$Porch.space <- (df.train$OpenPorchSF + df.train$EnclosedPorch + df.train$X3SsnPorch + df.train$ScreenPorch)
df.test$Porch.space <- (df.test$OpenPorchSF + df.test$EnclosedPorch + df.test$X3SsnPorch + df.test$ScreenPorch)
tot.df$Porch.space <- (tot.df$OpenPorchSF + tot.df$EnclosedPorch + tot.df$X3SsnPorch + tot.df$ScreenPorch)

# Correlation check
comp3 <- c("Porch.space", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch")
cor(df.train[comp3], df.train$log.SalePrice)

## Correlation of Porch.space is not higher than OpenPorchSF
# Porch.space    0.19569931
# OpenPorchSF    0.32490594
# EnclosedPorch -0.14883360
# X3SsnPorch     0.05501908
# ScreenPorch    0.12148848

# -> 4 porch variables VIF < 0, P-value also low (except X3SsnPorch)
#   (especially, ScreenPorch's P-value = 0.0000005)
# ==> will not create "Porch.space" varaible
df.train$Porch.space <- NULL
df.test$Porch.space <- NULL
tot.df$Porch.space <- NULL


# 4) Feature deletion (consider variable similarity, VIF, P-value, etc..)

# feature deletion which related to derivative
same.as.new.feature <- c("TotalBsmtSF", "GrLivArea", "GarageArea", "YearBuilt", "YrSold", "YearRemodAdd")
df.train[same.as.new.feature] = NULL
tot.df[same.as.new.feature] = NULL
df.test[same.as.new.feature] = NULL

## Correlation Analysis (All)  # verify relation of indepantant variables
num.var3 <- df.train[sapply(df.train, is.numeric)]
var.cor3 <- cor(num.var3,  use="complete.obs")
corrplot.mixed(var.cor3, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")
write.csv(var.cor3, "var.cor3.csv")

## Result of verifying between indepantant variables
# ExterQual, BsmtQual --> Corr, VIF, P-value are high with "HOverallQual" ==> delete
# FireplaceQu --> Corr, VIF, P-value are high with "Fireplaces" ==> delete
# GarageCond --> Corr, VIF, P-value are high with "GarageQual" ==> delete
# PoolQC --> Corr, VIF, P-value are high with "PoolArea" ==> delete
multico <- c("ExterQual", "BsmtQual", "FireplaceQu","GarageCond", "PoolQC")
df.train[multico] = NULL
tot.df[multico] = NULL
df.test[multico] = NULL

# 5) Feature Cleansing (SalePrice -> delete (y : log.SalePrice),  ID -> delete)
clear <- c("SalePrice", "Id")
df.train[clear] = NULL
tot.df[clear] = NULL
df.test[clear] = NULL


# 2nd Quick view by LM
model.2nd = lm(log.SalePrice ~ ., data = df.train)
summary(model.2nd)
plot(model.2nd)


# VIF test after cleaning feature
library(car)
par(mfrow=c(1,1))
influencePlot(model.2nd)
vif <- vif(model.2nd)
nrow(vif[vif[,1] > 5,])  ## 32 variables have VIF issue
vif[vif[,1] > 5,]
nrow(vif[vif[,1] > 10,])  ## 18 variables over 10 value


# 5) Final Data set before Feature Selection
#  df.train :  (1457, 62)
#  df.test :  (1459, 62)
#  tot.df :  (2916, 62)

# Clear interim process data
remove(vif, comp, comp2, comp3, i, multico, same.as.new.feature, single.issue, clear)


#### Dummification (factors)
dummy <- dummyVars( ~ ., data = tot.df, fullRank = T)
tot.df.all <- data.frame(predict(dummy, newdata = tot.df))
df.train.all <- tot.df.all[1:1457,]
df.test.all <- tot.df.all[1458:2916,]

tot.df.all.sd <- sapply(tot.df.all, sd)
tot.df.all.sd[tot.df.all.sd == 0]

df.train.all.sd <- sapply(df.train.all, sd)
df.train.all.sd[df.train.all.sd == 0]

df.test.all.sd <- sapply(df.test.all, sd)
df.test.all.sd[df.test.all.sd == 0]

# remove zero Standard Deviation (train & test)
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

sum(is.na(tot.df.all))

## remove useless Dataset
remove(df.test, df.train, tot.df)


## 3.2. Feature Selection

# 1) Subset selection (Mixed) --> R2, AIC, BIC

model.empty = lm(log.SalePrice ~ 1, data = df.train.all) #The model with an intercept ONLY.
model.full = lm(log.SalePrice ~ ., data = df.train.all) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) # The Modern Applied Statistics library.
# Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

# Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))

# In this case, all procedures yield the model with only the Murder, HS.Grad,
# Frost, and Population variables intact.

# Checking the model summary and assumptions of the reduced model.
# forwardAIC
summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)

# backwardAIC
summary(backwardAIC)
plot(backwardAIC)
influencePlot(backwardAIC)
vif(backwardAIC)

# bothAIC.empty
summary(bothAIC.empty)
plot(bothAIC.empty)
influencePlot(bothAIC.empty)
vif(bothAIC.empty)

# bothAIC.full
summary(bothAIC.full)
plot(bothAIC.full)
influencePlot(bothAIC.full)
vif(bothAIC.full)

# forwardBIC
summary(forwardBIC)
plot(forwardBIC)
influencePlot(forwardBIC)
vif(forwardBIC)

# backwardBIC
summary(backwardBIC)
plot(backwardBIC)
influencePlot(backwardBIC)
vif(backwardBIC)

# bothBIC.empty
summary(bothBIC.empty)
plot(bothBIC.empty)
influencePlot(bothBIC.empty)
vif(bothBIC.empty)

# bothBIC.full
summary(bothBIC.full)
plot(bothBIC.full)
influencePlot(bothBIC.full)
vif(bothBIC.full)


# 2) Regulization (Lasso)
### Result of Penalization : 42 variables

# 2-1) dataset -> Matrix
x = model.matrix(log.SalePrice ~ ., df.train.all)[, -1]  # -1 : remove index
y = df.train.all$log.SalePrice

set.seed(0)
train = sample(1:nrow(x), nrow(x)*0.8)
test = (-train)
y.test = y[test]

# 2-2) Fit a model:
library(glmnet)

grid = 10^seq(5, -2, length = 100)
lasso.models = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)

# 2-3) Visualization (Plot the coefficients)
plot(lasso.models, xvar = "lambda", label = TRUE, main = "Lasso Regression")

# 2-4) Cross Validation & Lambda Visualization: 10-fold cross validation
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

# 2-8) Visualize Variable Importance
lasso_imp <- as.data.frame(varImp(lasso.best_refit, lambda = bestlambda.lasso))
lasso_imp <- data.frame(names = rownames(lasso_imp), overall = lasso_imp$Overall)
lasso_imp <- lasso_imp[order(lasso_imp$overall,decreasing = T),]

ggplot(data=lasso_imp[lasso_imp$overall > 0,], aes(x=reorder(names, overall), y=overall)) +
  geom_bar(stat='identity') +
  scale_x_discrete(name='Variable') +
  ylab('Overall') +
  coord_flip()

# MSE & RMSE
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
MSE = mean((lasso.bestlambda - y)^2)  # 0.01427454
RMSE = sqrt(MSE)
RMSE

# original MSE & RMSE (before transfer to log)
lasso.bestlambda = predict(lasso.best_refit, s = bestlambda.lasso, newx = x)
MSE_ori = mean((exp(lasso.bestlambda) - exp(y))^2)  # 0.01427454
RMSE_ori = sqrt(MSE_ori)
RMSE_ori


# 3) Feature Importance from model (Correlation, mRMR, RF, Lasso, PLS, SVM)

# Check Correlation Again

#### Dummy Dataset
## When execute below code of algorithm, Variance "0" warning - deletion is needed.
variance.zero <- c('Neighborhood.Blueste', 'Functional.Sev', 'Condition1.RRNe', 'Exterior1st.AsphShn',
                   'Condition2.PosN', 'Exterior1st.CBlock', 'SaleType.Con', 'Condition2.PosA', 
                   'Exterior1st.BrkComm', 'MiscFeature.Othr', 'SaleType.Oth', 'RoofStyle.Shed' )

df.train.all[variance.zero] = NULL
tot.df.all[variance.zero] = NULL
df.test.all[variance.zero] = NULL


set.seed(1)
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)


# 3-1) PLS
plsFit <- train(log.SalePrice ~ ., data = df.train.all, method = "pls", 
                trControl = ctrl, preProc = c("center", "scale"))
plsFit
pls.importance <- varImp(plsFit)
pls.importance # top 20 
pls.importance.all <- varImp(plsFit)$importance
pls.importance.all

library(ggplot2)
library(dplyr)
library(tidyverse)

varImp(plsFit)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(Overall) %>%
  mutate(rowname = forcats::fct_inorder(rowname )) %>%
  ggplot() +
  geom_col(aes(x = rowname, y = Overall)) +
  coord_flip() +
  theme_bw()

## Issue (11/13 17:30)
## 0 variance issue : Neighborhood.Blueste, Functional.Sev, Condition1.RRNe, Exterior1st.AsphShn, .
##                    Condition2.PosN, Exterior1st.CBlock, SaleType.Con, 


# 3-2) RF
rfFit <- train(log.SalePrice ~ ., data = df.train.all, method = "rf", importance=T,
               trControl = ctrl, preProc = c("center", "scale"))
rfFit
rf.importance <- varImp(rfFit)
rf.importance.all <- varImp(rfFit)$importance

## 0 variance issue : Exterior1st.BrkComm, MiscFeature.Othr, SaleType.Oth, RoofStyle.Shed


# 3-3) Gradient Boosting
library(gbm)
gbmFit <- train(log.SalePrice ~ ., data = df.train.all, method = "gbm",
                trControl = ctrl, preProc = c("center", "scale"))
gbmFit
varImp(gbmFit, n.trees = 150)  # error
gbm.importance.all <- varImp(gbmFit)$importance


# 3-4) mRMR
## from package guide
library(mRMRe)
dd <- mRMR.data(data = df.train.all)
results <- mRMR.classic("mRMRe.Filter", data = dd, target_indices = 160,
                        feature_count = 30)
solutions(results)


# 3-5) SVM linear
svmFit <- train(log.SalePrice ~ ., data = df.train.all, method = "svmLinear",
                trControl = ctrl, preProc = c("center", "scale"))
svm.importance <- varImp(svmFit)
gbm.importance.all <- varImp(svmFit)$importance

## 0 variance issue : Neighborhood.Blueste


# Clear garbage data 
remove('dd','dummy','num.var.train','num.var2','num.var3','a','df.test.all.sd','df.train.all.sd',
       'test.sd.issue', 'tot.df.all.sd', 'train.sd.issue', 'variance.zero')


save.image(file="feature_selection_complete.RData")
