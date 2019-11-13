library(glmnet)

# lasso regression
set.seed(0)
grid = 10^seq(5, -2, length = 100)
train_x = model.matrix(SalePrice ~ ., train_new)[, -1]
train_y = train_new$SalePrice
lasso.models.train = glmnet(train_x, train_y, alpha = 1, lambda = grid)

cv.lasso.out = cv.glmnet(train_x, train_y, lambda = grid, alpha = 1, nfolds = 10)
log(cv.lasso.out$lambda.min)
plot(cv.lasso.out, main = "Lasso Regression\n")

validation_new$Predict = NULL
test_x = model.matrix(SalePrice ~ ., validation_new)[, -1]
validation_new$Predict <- predict(lasso.models.train, s = cv.lasso.out$lambda.min, newx = test_x)

selected_var <- coef(lasso.models.train ,s = cv.lasso.out$lambda.min, exact = TRUE)
selected_var

# RMSE
sqrt(sum((validation_new$Predict - validation_new$SalePrice) ^ 2) / nrow(validation_new))

# MAE
sum(abs(validation_new$Predict - validation_new$SalePrice)) / nrow(validation_new)
