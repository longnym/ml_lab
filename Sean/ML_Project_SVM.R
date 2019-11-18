library(e1071)

# cv.svm.model = tune(svm, log.SalePrice ~ ., data = train_new, kernel = "linear", 
#                      ranges = list(cost=10^(-2:2), gamma=c(0.1, 0.25,0.5,0.75,1,2)))

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

