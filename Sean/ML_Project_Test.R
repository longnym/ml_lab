library(caret)
library(car)

# Corr to 10
# var.cor <- cor(tot.df[, sapply(tot.df, is.numeric) %>% select(-c('Id','log.SalePrice'))],  use="complete.obs")
# corrplot.mixed(var.cor, number.cex = 0.35, tl.cex=0.6, tl.pos = "lt", order = "FPC")

# multi linear regression model
sale_model <- lm(log.SalePrice ~ ., train_new)

influencePlot(sale_model)

# remove dummy singularities
pred <- predict(sale_model, newdata=validation_new)

library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))