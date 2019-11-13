library(xgboost)

validation_new$Predict = NULL

dtrain <- xgb.DMatrix(data=as.matrix(train_new %>% select(-SalePrice)), label=train_new$SalePrice, missing=NA)
feature_names <- names(train_new %>% select(-SalePrice))
dtest <- xgb.DMatrix(data=as.matrix(validation_new %>% select(-SalePrice)), missing=NA)

xgb.mdl <- xgb.train(data = dtrain, nround=100)
validation_new$Predict = predict(xgb.mdl, dtest)

# Compute feature importance matrix
importance_matrix <- xgb.importance(feature_names, model = xgb.mdl)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

# RMSE
sqrt(sum((validation_new$Predict - validation_new$SalePrice) ^ 2) / nrow(validation_new))

# MAE
sum(abs(validation_new$Predict - validation_new$SalePrice)) / nrow(validation_new)
