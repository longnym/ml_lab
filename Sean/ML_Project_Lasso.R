library(glmnet)

# lasso regression
set.seed(0)
grid = 10^seq(5, -2, length = 100)
train_x = model.matrix(log.SalePrice ~ ., train_new)[, -1]
train_y = train_new$log.SalePrice
lasso.models.train = glmnet(train_x, train_y, alpha = 1, lambda = grid)

cv.lasso.out = cv.glmnet(train_x, train_y, lambda = grid, alpha = 1, nfolds = 10)
log(cv.lasso.out$lambda.min)
plot(cv.lasso.out, main = "Lasso Regression\n")

test_x = model.matrix(log.SalePrice ~ ., validation_new)[, -1]
pred <- predict(lasso.models.train, s = cv.lasso.out$lambda.min, newx = test_x)

selected_var <- coef(lasso.models.train ,s = cv.lasso.out$lambda.min, exact = TRUE)
selected_var

imp <- as.data.frame(varImp(lasso.models.train, lambda = cv.lasso.out$lambda.min))
imp <- data.frame(names = rownames(imp), overall = imp$Overall)
imp <- imp[order(imp$overall,decreasing = T),]
imp
write.csv(imp, 'lasso_importance.csv')

# RMSE
sqrt(sum((exp(pred) - exp(validation_new$log.SalePrice)) ^ 2) / nrow(validation_new))

# MAE
sum(abs(exp(pred) - exp(validation_new$log.SalePrice))) / nrow(validation_new)
