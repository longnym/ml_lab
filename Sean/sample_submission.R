# load train and test data
train <- read.csv('train.csv')
test <- read.csv('test.csv')

sample_submission <- lm(SalePrice ~ YrSold + MoSold + LotArea + BedroomAbvGr, data=train)

require(car)
summary(sample_submission)
vif(sample_submission)

sale_predict <- predict(sample_submission, newdata=test)
test$SalePrice <- sale_predict

test[,c('Id', 'SalePrice')]
