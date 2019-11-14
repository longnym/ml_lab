library(e1071)

svm.model = svm(log.SalePrice ~ ., data = train_new, kernel = 'linear')

summary(svm.model)

pred = predict(svm.model, validation_new)


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))

