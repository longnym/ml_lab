library(corrplot)
require(car)

# multi linear regression model
sale_model <- lm(log.SalePrice ~ ., data=train_new)

# remove dummy singularities
summary(sale_model)
vif(sale_model)

pred <- predict(sale_model, newdata=validation_new)

# RMSE
sqrt(sum((exp(pred) - exp(validation_new$log.SalePrice)) ^ 2) / nrow(validation_new))

# MAE
sum(abs(exp(pred) - exp(validation_new$log.SalePrice))) / nrow(validation_new)
