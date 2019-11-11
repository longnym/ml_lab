setwd("/Users/macbook/ml_lab/Heesuk")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test$SalePrice <- NA
tot.df <- rbind(train, test)


### 1. Basic Analysis
dim(tot.df)
summary(tot.df)
sapply(tot.df, class)
sapply(tot.df, FUN = function(x) sum(is.na(x)))

library(dplyr)
library(ggplot2)
library(corrplot)

## Y Distribution (training set)
ggplot(train, aes(x=SalePrice)) + geom_histogram(binwidth = 10000, fill="black")
  # --> Not well distributed ==> Log
ggplot(train, aes(x=log(SalePrice))) + 
  geom_histogram(binwidth = 0.05, color="black", fill="darkblue")
train$log.SalePrice <- log(train$SalePrice)

## Correlation (training set)
num.var <- train[sapply(train, is.numeric)]
fac.var <- train[sapply(train, is.factor)]
# col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white", "cyan", 
#                            "#007FFF", "blue", "#00007F"))
  
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

corr.xy <- merge(SalePrice.corr, log.SalePrice.corr, by='Xvar')
corr.xy <- corr.xy[order(-corr.xy$corr.price),]
corr.xy
remove(SalePrice.corr, log.SalePrice.corr)



## Missing Data & Imputation
na <- data.frame(sapply(tot.df, FUN = function(x) sum(is.na(x))))
na <- data.frame(names = row.names(na), na)
rownames(na) = NULL
colnames(na) = c("var","sum.na")
na <- na %>% filter(., sum.na >0) %>% arrange(., desc(sum.na))
write.csv(na, "na.csv")
## 34 variables have NA


################################ Sean Start ##################################

## KWAK modify : train -> tot.df

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

################################ Sean End ##################################




################################ Kwak Start ##################################

################## EDA (TBD), Outlier, NA Check,...  ##################





################## Feature Engineering ##################
tot.df.train <- tot.df[1:1460,] 

## 1. Feature creation 

# 0) Quick view by LM
model.init = lm(SalePrice ~ ., data = tot.df.train)    ## class 정제 필요 
summary(model.init) 
plot(model.init)
influencePlot(model.init) 
vif(model.init) 

# 1) House space feature
   # Total space 관련 변수  
     # - TotalBsmtSF (지하실 전체 면적(제곱피트))
     # - 1stFlrSF (1층 면적 (제곱 피트))
     # - 2ndFlrSF (2층 면적 (제곱 피트))
     # - ?? GrLivArea (지상의 생활 영역 면적 (제곱 피트)),
     #   ?? LowQualFinSF (저 품질로 완성된 면적 (제곱 피트) (모든 층))

# 2) Period feature 
   # 기간 산정 관련 변수 (경과일수 계산 : 날짜변환 후 계산)
     # - YearBuilt (최초 건설 일자)
     # - YearRemodAdd (리모델링 일자 (리모델링이나 증축이 없을 경우 최초 건설 일자와 동일)
     # - ?? GarageYrBlt (차고 건설 연도)
     # - ?? MoSold (매각 월 (MM)),  YrSold (매각 년도 (YYYY))


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


