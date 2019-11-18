#################### 1. multi linear regression model ########################

library(caret)
library(car)

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



#################### 2. lasso regression model ########################

library(glmnet)

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


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))



#################### 3. random forest model ########################

library(randomForest)

set.seed(0)
oob.err = numeric(10)
for (mtry in 1:10) {  # 51~60
  fit = randomForest(SalePrice ~ ., data = train_new, mtry = mtry + 50)
  oob.err[mtry] = fit[['mse']][500]
  cat("We're performing iteration", mtry, "\n")
}

plot(1:10, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

set.seed(0)
rf.model = randomForest(log.SalePrice ~ ., data = train_new)  # default : num_var / 3
rf.model

imp <- as.data.frame(importance(rf.model))
imp <- data.frame(names = rownames(imp), overall = imp$IncNodePurity)
imp <- imp[order(imp$overall,decreasing = T),]
imp
write.csv(imp, 'randomforest_importance.csv')

varImpPlot(rf.model)

pred = predict(rf.model, validation_new, type = "class")


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))



#################### 4. generalized boosted regression model (GBM) ########################

library(gbm)

set.seed(0)
boost.model = gbm(log.SalePrice ~ ., data = train_new,
                  distribution = "gaussian",
                  cv.folds = 10,
                  n.trees = 1000,
                  interaction.depth = 4)

imp <- as.data.frame(varImp(boost.model, numTrees = 1000))
imp <- data.frame(names = rownames(imp), overall = imp$Overall)
imp <- imp[order(imp$overall,decreasing = T),]

ggplot(data=imp[imp$overall > 0,], aes(x=reorder(names, overall), y=overall)) +
  geom_bar(stat='identity') +
  scale_x_discrete(name='Variable') +
  ylab('Overall') +
  coord_flip()

write.csv(imp, 'boosting_importance.csv')

n.trees = seq(from = 50, to = 1000, by = 50)
pred = predict(boost.model, newdata = validation_new, n.trees = n.trees)

berr = with(validation_new, apply((pred - log.SalePrice)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

n.tree = n.trees[which(berr == min(berr))]
pred = predict(boost.model, newdata = validation_new, n.trees = n.tree)


library(Metrics)

# RMSLE
rmsle(exp(validation_new$log.SalePrice), exp(pred))

# RMSE
rmse(exp(validation_new$log.SalePrice), exp(pred))

# MAE
mae(exp(validation_new$log.SalePrice), exp(pred))



#################### 5. support vector machine (SVM) ########################

library(e1071)

cv.svm.model = tune(svm, log.SalePrice ~ ., data = train_new, kernel = "linear",
                     ranges = list(cost=10^(-2:2), gamma=c(0.1, 0.25,0.5,0.75,1,2)))

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



#################### 6. XGBoost ########################

library(xgboost)

dtrain <- xgb.DMatrix(data=as.matrix(train_new %>% select(-log.SalePrice)), label=train_new$log.SalePrice, missing=NA)
dtest <- xgb.DMatrix(data=as.matrix(validation_new %>% select(-log.SalePrice)), missing=NA)

min.rmse <- c()
nrounds <- c()
param_grid <- expand.grid(
  max_depth = c(3, 6, 9), # default: 6
  min_child_weight = c(1, 3, 5), # default: 1
  gamma = c(0, 0.3, 1, 2), # default: 0
  eta = c(0.1, 0.3, 1) # default : 0.3
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
