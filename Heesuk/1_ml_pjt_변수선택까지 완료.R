setwd("/Users/macbook/ml_lab/Heesuk")z
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)


############################# 1. Understanding of Data ############################
## Data Import 
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$SalePrice <- NA
tot.df <- rbind(train, test)

dim(tot.df)
summary(tot.df)
sapply(tot.df, class)
sapply(tot.df, FUN = function(x) sum(is.na(x)))

## Missing Data check
na <- data.frame(sapply(tot.df, FUN = function(x) sum(is.na(x))))
na <- data.frame(names = row.names(na), na)
rownames(na) = NULL
colnames(na) = c("var","sum.na")
na <- na %>% filter(., sum.na >0) %>% arrange(., desc(sum.na))
write.csv(na, "na.csv")
## 35 variables have NA


############################# 2. EDA & Pre-processing ############################
## Y Distribution (training set)
ggplot(train, aes(x=SalePrice)) + geom_histogram(binwidth = 10000, fill="black")
  # --> Not well distributed ==> Log
ggplot(train, aes(x=log(SalePrice))) + 
  geom_histogram(binwidth = 0.05, color="black", fill="darkblue")
train$log.SalePrice <- log(train$SalePrice)

## Initial Correlation (training set)
num.var <- train[sapply(train, is.numeric)]
fac.var <- train[sapply(train, is.factor)]
var.cor <- cor(num.var,  use="complete.obs")
corrplot.mixed(var.cor, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")

SalePrice.corr <- as.data.frame(cor(subset(num.var,select=-c(SalePrice, log.SalePrice)), 
               num.var$SalePrice, use="complete.obs"))
SalePrice.corr <- tibble::rownames_to_column(SalePrice.corr)
colnames(SalePrice.corr) = c("Xvar","corr.price")

log.SalePrice.corr <- as.data.frame(cor(subset(num.var,select=-c(SalePrice, log.SalePrice)), 
                                    num.var$log.SalePrice, use="complete.obs"))
log.SalePrice.corr <- tibble::rownames_to_column(log.SalePrice.corr)
colnames(log.SalePrice.corr) = c("Xvar","corr.log.price")

corr.xy.init <- merge(SalePrice.corr, log.SalePrice.corr, by='Xvar')
corr.xy.init <- corr.xy.init[order(-corr.xy.init$corr.price),]
corr.xy.init
remove(SalePrice.corr, log.SalePrice.corr)


################################ Sean Start ##################################
## NA Imputation and Class correction
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

################################ Sean End ##################################

# SalePrice Log value 
tot.df$log.SalePrice <- log(tot.df$SalePrice)

# impute NA (Factor legvel : NA -> No)
library(forcats) 
tot.df$Alley <- fct_explicit_na(tot.df$Alley, na_level = "None")
tot.df$GarageType <- fct_explicit_na(tot.df$GarageType, na_level = "None")
tot.df$GarageFinish <- fct_explicit_na(tot.df$GarageFinish, na_level = "None")
tot.df$Fence <- fct_explicit_na(tot.df$Fence, na_level = "None")
tot.df$MiscFeature <- fct_explicit_na(tot.df$MiscFeature, na_level = "None")

# GarageYrBlt NA --> impute by 'YearBuilt'
tot.df <- tot.df %>% transform(., GarageYrBlt = ifelse(is.na(GarageYrBlt), YearBuilt, GarageYrBlt))

# Class/NA check
data.frame(sapply(tot.df, FUN = function(x) sum(is.na(x))))
class.check <- sapply(tot.df, class)

# Clear interim process data 
remove('codes','fac.var','lf_missing','lf_model','lf_no_missing','na', 'num.var',
       'test','train','var.cor','lf_predict','n','codeToNum','class.check')

# New Train/Test set
df.train <- tot.df[1:1460,] 
df.test <- tot.df[1461:2919,] 


# Outlier check 
num.var.train <- df.train[sapply(df.train, is.numeric)]

# Boxplot 
par(mfrow=c(2,2))
for (i in 1:length(num.var.train)) {
  png(filename = paste("./boxplot/", i, "_", names(num.var.train[i]), ".png", sep=""))
    boxplot(num.var.train[,i], main = names(num.var.train[i]),
          col = "orange", border = "brown", horizontal = TRUE, notch = TRUE )
    dev.off()
}

# Scatter plot 
par(mfrow=c(2,2))
options("scipen" = 100)
for (i in 1:length(num.var.train)) {
  png(filename = paste("./scatterplot/", i, "_scatter_", names(num.var.train[i]), ".png", sep=""))
  plot(num.var.train[,i], round(num.var.train$SalePrice), main = names(num.var.train[i]),
       xlab = names(num.var.train[i]), ylab = "SalePrice", col="red")
  dev.off()
}

## Outlier Check ==> Noise for Prediction .... Removal   
# ID 524 - OverallQuality 10, OverallCond 5, GrLivArea 4676 (Top2), .. =>  Price Low 184,750 
# ID 1299 - OverallQuality 10, OverallCond 5, GrLivArea 5632 (Top), ... =>   Price Low 160,000
# ID 582 - OverallQuality 8, OverallCond 5, 1st FLoor space 2042(Large), 
        #  Garage Large, TotalBsmtSF 2042, ... =>   Price Low  253,293 

## Outlier Removal
tot.df <- subset(tot.df, !(tot.df$Id %in% c('524', '1299', '582')))
df.train <- subset(df.train, !(df.train$Id %in% c('524', '1299', '582')))

## Feature issue clear (singularities)  -- Kwak 임시 (Next step 위한 임시)
# "#### singularities ####
# GrLivArea = 1stFlrSF + 2ndFlrSF + LowQualFinSF
# TotalBsmtSF = BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF

# Numeric feature singularity clear
single.issue <- c('X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF')
df.train[single.issue] = NULL
tot.df[single.issue] = NULL
df.test[single.issue] = NULL

# BldgTypeDuplex = MSSubClass90 (52 row)
# Exterior2ndCBlock = Exterior1ndCBlock (1 row)
# GarageFinishNone = GarageTypeNone (81 row)
# HouseStyle1.5Unf = ? (14 row)

### Singularity issue 관련 삭제 ###
single.issue <- c('X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 
                  'BldgType', 'GarageFinish', 'HouseStyle', 'Exterior2nd')
df.train[single.issue] = NULL
tot.df[single.issue] = NULL
df.test[single.issue] = NULL


# Quick view by LM
model.init = lm(log.SalePrice ~ . -SalePrice, data = df.train) #, singular.ok=FALSE)    ## singular.ok option 작동 안함 
summary(model.init)
plot(model.init)

library(car) 
par(mfrow=c(1,1))
influencePlot(model.init) 
vif <- vif(model.init) 
## VIF Issue : MSSubClass, MSZoning, Neighborhood, Condition2, YearBuilt, RoofStyle, RoofMatl,
# Exterior1st
nrow(vif[vif[,1] > 5,])  ## 32 variables have VIF issue
vif[vif[,1] > 5,]

nrow(vif[vif[,1] > 10,])  ## 18 variables over 10 value

## Corr 분석 (All)
num.var2 <- df.train[sapply(df.train, is.numeric)]
var.cor2 <- cor(num.var2,  use="complete.obs")
corrplot.mixed(var.cor2, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")

## Corr 분석 (X vs. Y)
SalePrice.corr2 <- as.data.frame(cor(subset(num.var2,select=-c(SalePrice, log.SalePrice)), 
                                    num.var2$SalePrice, use="complete.obs"))
SalePrice.corr2 <- tibble::rownames_to_column(SalePrice.corr2)
colnames(SalePrice.corr2) = c("Xvar","corr.price")

log.SalePrice.corr2 <- as.data.frame(cor(subset(num.var2,select=-c(SalePrice, log.SalePrice)), 
                                        num.var2$log.SalePrice, use="complete.obs"))
log.SalePrice.corr2 <- tibble::rownames_to_column(log.SalePrice.corr2)
colnames(log.SalePrice.corr2) = c("Xvar","corr.log.price")

corr.xy.aft.clear <- merge(SalePrice.corr2, log.SalePrice.corr2, by='Xvar')
corr.xy.aft.clear <- corr.xy.aft.clear[order(-corr.xy.aft.clear$corr.price),]
corr.xy.aft.clear
remove(SalePrice.corr2, log.SalePrice.corr2)

## Corr 분석 (X vs. X)


############################# 3. Feature Enginnering ############################

## 1. Feature Creation 

# 1) House space feature
   # Total space 관련 변수  
     # - TotalBsmtSF (지하실 전체 면적(제곱피트))
     # - GrLivArea (지상의 생활 영역 면적 (제곱 피트)),
     # - GarageArea (차고의 면적)

df.train$tot.space <- (df.train$TotalBsmtSF + df.train$GrLivArea + df.train$GarageArea)
df.test$tot.space <- (df.test$TotalBsmtSF + df.test$GrLivArea + df.test$GarageArea)
tot.df$tot.space <- (tot.df$TotalBsmtSF + tot.df$GrLivArea + tot.df$GarageArea)

# Corr check 
comp <- c("tot.space", "TotalBsmtSF", "GrLivArea", "GarageArea")
cor(df.train[comp], df.train$log.SalePrice)

## 상관관계 확인결과, tot.space가 나머지 3개 각각 보다 높음 
# tot.space   0.8528168
# TotalBsmtSF 0.6473571
# GrLivArea   0.7250041
# GarageArea  0.6575029

# -> 3개 각각의 변수 삭제 (검증 후 실행할것 - train/test/total)


# 2) Period feature 
   # 기간 산정 관련 변수 (경과일수 계산 : 날짜변환 후 계산)
   # - YearBuilt (최초 건설 일자)
   # - YearRemodAdd (리모델링 일자 (리모델링이나 증축이 없을 경우 최초 건설 일자와 동일)
   # - MoSold (매각 월 (MM)),  YrSold (매각 년도 (YYYY))

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

## 상관관계 확인결과, 기간경과 변수(2개) 나머지 3개 각각 보다 소폭 높음 
# built.period -0.58742118
# YearBuilt     0.58668710
# YrSold       -0.03781576
# remod.period -0.56818381
# YearRemodAdd  0.56562961


# 3) Porch feature
# Porch space 관련 변수 
# - Porch.space = 'OpenPorch' + 'EnclosedPorch' + 'X3SsnPorch' + 'ScreenPorch'

df.train$Porch.space <- (df.train$OpenPorchSF + df.train$EnclosedPorch + df.train$X3SsnPorch + df.train$ScreenPorch)
df.test$Porch.space <- (df.test$OpenPorchSF + df.test$EnclosedPorch + df.test$X3SsnPorch + df.test$ScreenPorch)
tot.df$Porch.space <- (tot.df$OpenPorchSF + tot.df$EnclosedPorch + tot.df$X3SsnPorch + tot.df$ScreenPorch)

# Corr check 
comp3 <- c("Porch.space", "OpenPorchSF", "EnclosedPorch", "X3SsnPorch", "ScreenPorch")
cor(df.train[comp3], df.train$log.SalePrice)

## 상관관계 확인결과, Porch.space CORR이 높지 않게 나옴 (OpenPorchSF가 더 높음) 
# Porch.space    0.19569931
# OpenPorchSF    0.32490594
# EnclosedPorch -0.14883360
# X3SsnPorch     0.05501908
# ScreenPorch    0.12148848

# -> 4개의 원래 변수는 전부 VIF <0 이고, P-value도 "X3SsnPorch" 외에는 낮게 나옴 
#   (특히 "ScreenPorch"는 P-value가 0.0000005)
# ==> 원래의 4개 변수 유지하고 "Porch.space" 만들지 않음 

df.train$Porch.space <- NULL
df.test$Porch.space <- NULL
tot.df$Porch.space <- NULL


# 4) Feature deletion (유사변수, VIF, P-value 등 고려 )

# 파생변수와 동일한 feature deletion 
same.as.new.feature <- c("TotalBsmtSF", "GrLivArea", "GarageArea","YearBuilt", "YrSold","YearRemodAdd")
df.train[same.as.new.feature] = NULL
tot.df[same.as.new.feature] = NULL
df.test[same.as.new.feature] = NULL

## Corr 분석 (All)  # X간 관계 검증 위함 
num.var3 <- df.train[sapply(df.train, is.numeric)]
var.cor3 <- cor(num.var3,  use="complete.obs")
corrplot.mixed(var.cor3, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")
write.csv(var.cor3, "var.cor3.csv")

## X변수 간 Correlation 및 VIF, LM Coef 검증 결과 
# ExterQual, BsmtQual --> OverallQual과 Corr 매우 높음, VIF 계수 큼,  P-value 큼 ==> 삭제 
# FireplaceQu --> Fireplaces와 Corr 매우 높음, VIF 큼, P-value 큼 ==> 삭제 
# GarageCond --> GarageQual과 Corr 매우 높음, VIF 큼, P-value 큼 ==> 삭제 
# PoolQC --> PoolArea와 Corr 매우 높음, VIF 큼, P-value 큼 ==> 삭제 

multico <- c("ExterQual", "BsmtQual", "FireplaceQu","GarageCond", "PoolQC")
df.train[multico] = NULL
tot.df[multico] = NULL
df.test[multico] = NULL

# 5) Feature 정제 (SalePrice -> 삭제(y는 log값),  ID 삭제)
clear <- c("SalePrice", "Id")
df.train[clear] = NULL
tot.df[clear] = NULL
df.test[clear] = NULL


# 2nd Quick view by LM
model.2nd = lm(log.SalePrice ~ ., data = df.train) #, singular.ok=FALSE)    ## singular.ok option 작동 안함 
summary(model.2nd)
plot(model.2nd)



library(car) 
par(mfrow=c(1,1))
influencePlot(model.2nd) 
vif <- vif(model.2nd) 
nrow(vif[vif[,1] > 5,])  ## 32 variables have VIF issue
vif[vif[,1] > 5,]
nrow(vif[vif[,1] > 10,])  ## 18 variables over 10 value


# 5) Feature Selection 이전 최종 Data set 
  #  df.train :  (1457, 62)
  #  df.test :  (1459, 62)
  #  tot.df :  (2916, 62)

# Clear interim process data 
remove('vif','comp','comp2','comp3','i', 'multico', 
       'same.as.new.feature','single.issue','clear')

## feature name, class
# tmp <- sapply(tot.df, class)
# write.csv(tmp, "init.variables.class.csv")

### Feature selection 이전으로 이동하여 전체 재 실행 
#### Dummification
dummy <- dummyVars( ~ ., data = tot.df, fullRank = T)
tot.df.all <- data.frame(predict(dummy, newdata = tot.df))
df.train.all <- tot.df.all[1:1457,] 
df.test.all <- tot.df.all[1458:2916,] 

# ## feature name, class
# tmp2 <- sapply(tot.df.all, class)
# write.csv(tmp2, "dummy.variables.class.csv")

tot.df.all.sd <- sapply(tot.df.all, sd)
tot.df.all.sd[tot.df.all.sd == 0]

df.train.all.sd <- sapply(df.train.all, sd)
df.train.all.sd[df.train.all.sd == 0]

df.test.all.sd <- sapply(df.test.all, sd)
df.test.all.sd[df.test.all.sd == 0]

# SD zero (train)
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


#############################  요기 위 까지 (11/13 17시)   #########################


## Dataset 정제 
remove(df.test, df.train, tot.df)

## 2. Feature Selection 

# 1) Subset selection (Mixed) --> R2, AIC, BIC
# 
model.empty = lm(log.SalePrice ~ 1, data = df.train.all) #The model with an intercept ONLY.
model.full = lm(log.SalePrice ~ ., data = df.train.all) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) #The Modern Applied Statistics library.
#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))

#In this case, all procedures yield the model with only the Murder, HS.Grad,
#Frost, and Population variables intact.

#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
# avPlots(forwardAIC)
# confint(forwardAIC)

summary(backwardAIC)
plot(backwardAIC)
influencePlot(backwardAIC)
vif(backwardAIC)

summary(bothAIC.empty)
plot(bothAIC.empty)
influencePlot(bothAIC.empty)
vif(bothAIC.empty)

summary(bothAIC.full)
plot(bothAIC.full)
influencePlot(bothAIC.full)
vif(bothAIC.full)

summary(forwardBIC)
plot(forwardBIC)
influencePlot(forwardBIC)
vif(forwardBIC)

summary(backwardBIC)
plot(backwardBIC)
influencePlot(backwardBIC)
vif(backwardBIC)

summary(bothBIC.empty)
plot(bothBIC.empty)
influencePlot(bothBIC.empty)
vif(bothBIC.empty)

summary(bothBIC.full)
plot(bothBIC.full)
influencePlot(bothBIC.full)
vif(bothBIC.full)


# forwardAIC$coefficients
# forwardAIC$model
# forwardAIC$rank
# forwardAIC$model
# forwardAIC$model
# forwardAIC$model
# forwardAIC$model
# forwardAIC$model


# 2) Regulization (Lasso) -- 최종 모델링까지 
### Penalization 결과인 42개 변수 활용

# 2-1) dataset 재정리 (Matrix화)
x = model.matrix(log.SalePrice ~ ., df.train.all)[, -1]  # -1은 Index 제거 용
y = df.train.all$log.SalePrice

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

# 2-4) Cross Validation & Lambda값 시각화: 10-fold cross validation 
set.seed(0)
cv.lasso.out = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid)
plot(cv.lasso.out, main = "Lasso Regression\n")

# 2-5) Result : Best Lambda값 
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
MSE_ori = mean((exp(lasso.bestlambda) - exp(y))^2)  # 0.01427454
RMSE_ori = sqrt(MSE_ori)
RMSE_ori




# 3) Feature Importance (Correlation, mRMR, RF, Lasso, PLS, SVM)

# Correlation :  위 형식대로 재 작업

#### Dummy 포함한 Dataset으로 재 실행 
## 아래 Algorith 실행 시 Variance "0" warning - deletion
variance.zero <- c('Neighborhood.Blueste', 'Functional.Sev', 'Condition1.RRNe', 'Exterior1st.AsphShn',
                   'Condition2.PosN', 'Exterior1st.CBlock', 'SaleType.Con', 'Condition2.PosA', 
                   'Exterior1st.BrkComm', 'MiscFeature.Othr', 'SaleType.Oth', 'RoofStyle.Shed' )

df.train.all[variance.zero] = NULL
tot.df.all[variance.zero] = NULL
df.test.all[variance.zero] = NULL


set.seed(1)

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# 1. PLS
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
  ggplot()+
  geom_col(aes(x = rowname, y = Overall))+
  coord_flip()+
  theme_bw()

## Issue 재 검토 (11/13 17:30)
## 0 variance issue : Neighborhood.Blueste, Functional.Sev, Condition1.RRNe, Exterior1st.AsphShn, .
##                    Condition2.PosN, Exterior1st.CBlock, SaleType.Con, 

# 2. RF
rfFit <- train(log.SalePrice ~ ., data = df.train.all, method = "rf", importance=T,
                trControl = ctrl, preProc = c("center", "scale"))
rfFit
rf.importance <- varImp(rfFit)
rf.importance.all <- varImp(rfFit)$importance

## 0 variance issue : Exterior1st.BrkComm, MiscFeature.Othr, SaleType.Oth, RoofStyle.Shed

library(gbm)
# 3. Gradient Boosting
gbmFit <- train(log.SalePrice ~ ., data = df.train.all, method = "gbm", 
               trControl = ctrl, preProc = c("center", "scale"))
gbmFit
varImp(gbmFit, n.trees = 150)  # error
gbm.importance.all <- varImp(gbmFit)$importance


# Grid search for RF
# customGrid <- expand.grid(mtry = 1:10)
# rfFit2 <- train(log.SalePrice ~ ., data = df.train, method = "rf", trControl = ctrl, 
#                  tuneGrid = customGrid, verbose = F)
# rf_fit2


# 4. mRMR
## Package guide 기준
library(mRMRe)
dd <- mRMR.data(data = df.train.all)
results <- mRMR.classic("mRMRe.Filter", data = dd, target_indices = 160,
                        feature_count = 30)
solutions(results)


# 5. SVM linear
svmFit <- train(log.SalePrice ~ ., data = df.train.all, method = "svmLinear", 
                trControl = ctrl, preProc = c("center", "scale"))
svm.importance <- varImp(svmFit)
gbm.importance.all <- varImp(svmFit)$importance


## 0 variance issue : Neighborhood.Blueste


# Clear garbage data 
remove('dd','dummy','num.var.train','num.var2','num.var3','a','df.test.all.sd','df.train.all.sd',
       'test.sd.issue', 'tot.df.all.sd', 'train.sd.issue', 'variance.zero')



save.image(file="feature_selection_complete.RData")
