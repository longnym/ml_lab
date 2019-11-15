library(xgboost)

dtrain <- xgb.DMatrix(data=as.matrix(train_new %>% select(-log.SalePrice)), label=train_new$log.SalePrice, missing=NA)
dtest <- xgb.DMatrix(data=as.matrix(validation_new %>% select(-log.SalePrice)), missing=NA)

min.rmse <- c()
nrounds <- c()
# param_grid <- expand.grid(
#   max_depth = c(3, 6, 9), # default: 6
#   min_child_weight = c(1, 3, 5), # default: 1
#   gamma = c(0, 0.3, 0.5), # default: 0
#   eta = c(0.1, 0.3, 1) # default : 0.3
# )

param_grid <- expand.grid(
  max_depth = c(9), # default: 6
  min_child_weight = c(5), # default: 1
  gamma = c(2), # default: 0
  eta = c(1) # default : 0.3
)

for(i in 1:nrow(param_grid)) {
  print(paste0(i,' iteration of ', nrow(param_grid)))
  param <- list(
    booster = "gblinear",
    objective = "reg:linear",
    eval_metric = 'rmse',
    gamma = param_grid$gamma[i],
    eta = param_grid$eta[i],
    max_depth = param_grid$max_depth[i],
    min_child_weight = param_grid$min_child_weight[i]
  )
  foldsCV <- createFolds(train_new$log.SalePrice, k=10, list=TRUE, returnTrain=FALSE)
  xgb_cv <- xgb.cv(data = dtrain, params = param, nrounds = 2000, prediction = TRUE, maximize = FALSE,
                   folds = foldsCV, print_every_n = 100)
  
  min.rmse[i] <- min(xgb_cv$evaluation_log$test_rmse_mean)
  nrounds[i] <- min(which(xgb_cv$evaluation_log$test_rmse_mean == min(xgb_cv$evaluation_log$test_rmse_mean)))
}

best <- which(min.rmse == min(min.rmse))

param.new <- list(booster = "gblinear",
                  objective = "reg:linear",
                  eval_metric = 'rmse',
                  gamma = param_grid$gamma[best],
                  eta = param_grid$eta[best],
                  max_depth = param_grid$max_depth[best],
                  min_child_weight = param_grid$min_child_weight[best])
xgb.mdl <- xgb.train(data = dtrain, nround=nrounds[best], params = param.new)
pred <- predict(xgb.mdl, dtest)

# Compute feature importance matrix
feature_names <- names(train_new %>% select(-log.SalePrice))
importance_matrix <- xgb.importance(feature_names, model = xgb.mdl)
write.csv(importance_matrix, 'xgboost_importance.csv')

xgb.plot.importance(importance_matrix, top_n = 10)


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))
