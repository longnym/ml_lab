library(dplyr)
library(caret)

# multicollinearity : X1stFlrSF, X2ndFlrSF, LowQualFinSF, BsmtFinSF1, BsmtFinSF2, BsmtUnfSF
# singularity : Condition2, BldgType, HouseStyle, Exterior1st, Exterior2nd, GarageFinish
# (Condition2.RRAn, BldgType.Duplex, HouseStyle.1.5Unf, Exterior1st.Other, Exterior2nd.CBlock, GarageFinish.None)
# useless column : ID
drop_cols <- c('Id', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF',
               'Condition2', 'BldgType', 'HouseStyle', 'Exterior1st', 'Exterior2nd', 'GarageFinish', 'GarageYrBlt')

# drop columns
tot.df <- tot.df %>% select(-drop_cols)

# convert factor to dummy
dummy <- dummyVars( ~ ., data=tot.df, fullRank=T)
tot.df.dummy <- data.frame(predict(dummy, newdata = tot.df))

# divide train & test
set.seed(0)
index <- sample(1:nrow(train), nrow(train) * 0.8)

train_new <- tot.df.dummy[1:nrow(train),][index,]
test_new <- tot.df.dummy[1:nrow(train),][-index,]
