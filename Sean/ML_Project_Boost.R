library(gbm)

set.seed(0)
boost.model = gbm(log.SalePrice ~ ., data = train_new,
                  distribution = "gaussian",
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

pred = predict(boost.model, newdata = validation_new, n.trees = 200)

# RMSE
sqrt(sum((exp(pred) - exp(validation_new$log.SalePrice)) ^ 2) / nrow(validation_new))

# MAE
sum(abs(exp(pred) - exp(validation_new$log.SalePrice))) / nrow(validation_new)
