library(caret)

# multi linear regression model
sale_model <- train(log.SalePrice ~ ., train_new, method = "lm",
                    trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))

# remove dummy singularities
summary(sale_model)

pred <- predict(sale_model, newdata=validation_new)

library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))
