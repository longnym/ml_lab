library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
require(scales)


############################# 1. Understanding of Data ############################
## Data Import
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$SalePrice <- NA
tot.df <- rbind(train, test)

dim(tot.df)
summary(tot.df)
sapply(tot.df, class)

## Missing Data check
na <- data.frame(
  variable = colnames(tot.df),
  na.count = sapply(tot.df, FUN = function(x) sum(is.na(x))),
  class = sapply(tot.df, class)
)
na <- na %>% filter(., na.count > 0) %>% arrange(., desc(na.count))
write.csv(na, "na.csv", row.names = FALSE)
## 35 variables have NA

ggplot(data=na, aes(x=reorder(variable, na.count), y=na.count, fill=class)) +
  geom_bar(stat='identity') +
  scale_x_discrete(name='Variable') +
  ylab('NA Count') +
  coord_flip()


############################# 2. EDA & Pre-processing ############################
## Y Distribution (training set)
point <- format_format(big.mark = ",", decimal.mark = ",", scientific = FALSE)
ggplot(train, aes(x=SalePrice)) + geom_histogram(binwidth = 10000, fill="black") +
  scale_x_continuous(labels = point)

# --> Not well distributed ==> Log
ggplot(train, aes(x=log(SalePrice))) +
  geom_histogram(binwidth = 0.05, fill="darkblue")
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


## NA Imputation and Class correction
# define codes
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

# SalePrice Log value
tot.df$log.SalePrice <- log(tot.df$SalePrice)

# impute NA (Factor legvel : NA -> None)
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


##### Outlier check
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


## Feature issue clear (singularities)
# "#### singularities ####
# GrLivArea = 1stFlrSF + 2ndFlrSF + LowQualFinSF
# TotalBsmtSF = BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF

# BldgTypeDuplex = MSSubClass90 (52 row)
# Exterior2ndCBlock = Exterior1ndCBlock (1 row)
# GarageFinishNone = GarageTypeNone (81 row)
# HouseStyle1.5Unf = ? (14 row)

### Singularity issue (Remove) ###
# BldgType, HouseStyle < MSSubClass
# Exterior2nd : Almost Similar with Exterior1nd
single.issue <- c('X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 
                  'BldgType', 'GarageFinish', 'HouseStyle', 'Exterior2nd')
df.train[single.issue] = NULL
tot.df[single.issue] = NULL
df.test[single.issue] = NULL


# Quick view by LM
model.init = lm(log.SalePrice ~ . -SalePrice, data = df.train)
summary(model.init)
plot(model.init)

# VIF Test
library(car) 
par(mfrow=c(1,1))
influencePlot(model.init) 
vif <- vif(model.init) 

## VIF Issue : MSSubClass, MSZoning, Neighborhood, Condition2, YearBuilt, RoofStyle, RoofMatl, Exterior1st
nrow(vif[vif[,1] > 5,])  ## 31 variables have VIF issue
vif[vif[,1] > 5,]

nrow(vif[vif[,1] > 10,])  ## 17 variables over 10 value
vif[vif[,1] > 10,]

## Correlation Analysis (All)
num.var2 <- df.train[sapply(df.train, is.numeric)]
var.cor2 <- cor(num.var2,  use="complete.obs")
corrplot.mixed(var.cor2, number.cex = 0.4, tl.cex=0.5, tl.pos = "lt", order = "hclust")

## Correlation Analysis (X vs. Y)
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
