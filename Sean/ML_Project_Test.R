library(corrplot)
require(car)

# multi linear regression model
sale_model <- lm(log.SalePrice ~ ., data=train_new)

# remove dummy singularities
summary(sale_model)
vif(sale_model)

pred <- predict(sale_model, newdata=validation_new)


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))
