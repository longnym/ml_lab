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
imp
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
