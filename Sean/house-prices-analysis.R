train <- read.csv('train.csv')
summary(train)

codes <- list()
codes[['ExterQual']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['ExterCond']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['BsmtQual']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')  # exist NA
codes[['BsmtCond']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')  # exist NA
codes[['BsmtExposure']] <- c('No', 'Mn', 'Av', 'Gd')  # exist NA
codes[['BsmtFinType1']] <- c('Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ')  # exist NA
codes[['BsmtFinType2']] <- c('Unf', 'LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ')  # exist NA
codes[['HeatingQC']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['KitchenQual']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')
codes[['FireplaceQu']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex') # exist NA
codes[['GarageQual']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')  # exist NA
codes[['GarageCond']] <- c('Po', 'Fa', 'TA', 'Gd', 'Ex')  # exist NA
codes[['PoolQC']] <- c('Fa', 'TA', 'Gd', 'Ex')  # exist NA

codeToNum <- function(table, column) {
  result <- c()
  for (i in 1:nrow(table)) {
    index <- ifelse(is.na(table[i,column]), 0, which(codes[[column]] == table[i,column]))
    result <- c(result, c(index))
  }
  result
}

for (n in names(codes)) {
  train[n] <- codeToNum(train, n)
}
