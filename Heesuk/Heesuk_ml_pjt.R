setwd("/Users/macbook/ml_lab/Heesuk")
library(dplyr)
library(ggplot2)
library(corrplot)

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

### temp ###
single.issue <- c('X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 
                  'BldgType', 'GarageFinish', 'HouseStyle', 'Exterior2nd')
df.train[single.issue] = NULL
tot.df[single.issue] = NULL
df.test[single.issue] = NULL


# Quick view by LM
model.init = lm(log.SalePrice ~ ., data = df.train) #, singular.ok=FALSE)    ## singular.ok option 작동 안함 
summary(model.init) 
plot(model.init)
library(car) 
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



# 3) Feature deletion (유사변수, VIF 등 )





## 2. Feature Selection 

# 1) Subset selection (Mixed) --> R2, AIC, BIC

# 2) Regulization (Lasso)

# 3) Feature Importance (Correlation, mRMR, RF, Lasso, PLS, SVM)



## 3. Conclustion -> Core feature + alpha(?)
##############################################



################## Modeling ##################

### LM, Elastic/Lasso/Ridge, RF, Boosting, SVR(?)

##############################################




# na.check <- data.frame(sapply(tot.df, FUN = function(x) sum(is.na(x))))
# na.check <- data.frame(names = row.names(na.check), na.check)
# rownames(na.check) = NULL
# colnames(na.check) = c("var","sum.na")
# na.check <- na.check %>% filter(., sum.na >0) %>% arrange(., desc(sum.na))
# write.csv(na.check, "na_check.csv")


# # 1) LotFrontage NA
# colnames(tot.df %>% select(., contains("Lot")))
# # "LotFrontage" "LotArea"     "LotShape"    "LotConfig"  
# 
# # (1.1) LotFrontage vs. LotArea
# plot(tot.df$LotArea, tot.df$LotFrontage)
# plot(tot.df$LotArea, tot.df$LotFrontage, xlim = c(0,50000))
# plot(tot.df$LotArea, tot.df$LotFrontage, xlim = c(0,30000), ylim=c(0,150))
# cor(tot.df$LotArea, tot.df$LotFrontage, use="complete.obs")
# 
# # (1.2) LotShape vs. LotArea
# plot(tot.df$LotShape, tot.df$LotFrontage)
# 
# # (1.3) LotShape vs. LotArea
# plot(tot.df$LotConfig, tot.df$LotFrontage)


