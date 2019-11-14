library(xgboost)

dtrain <- xgb.DMatrix(data=as.matrix(train_new %>% select(-log.SalePrice)), label=train_new$log.SalePrice, missing=NA)
dtest <- xgb.DMatrix(data=as.matrix(validation_new %>% select(-log.SalePrice)), missing=NA)

foldsCV <- createFolds(train_new$log.SalePrice, k=7, list=TRUE, returnTrain=FALSE)

param <- list(booster = "gblinear", objective = "reg:linear", eval_metric = 'rmse')

xgb_cv <- xgb.cv(data = dtrain, params = param, nrounds = 1000, prediction = TRUE, maximize = FALSE,
                 folds = foldsCV, print_every_n = 20)

nrounds <- which(xgb_cv$pred == min(xgb_cv$pred))

xgb.mdl <- xgb.train(data = dtrain, nround=nrounds)
pred = predict(xgb.mdl, dtest)

# Compute feature importance matrix
feature_names <- names(train_new %>% select(-log.SalePrice))
importance_matrix <- xgb.importance(feature_names, model = xgb.mdl)
write.csv(importance_matrix, 'xgboost_importance.csv')

# Nice graph
xgb.plot.importance(importance_matrix[1:20,])


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))
