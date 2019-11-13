library(gbm)

set.seed(0)
boost.model = gbm(log.SalePrice ~ ., data = train_new,
                  distribution = "gaussian",
                  n.trees = 10000,
                  interaction.depth = 4)

imp <- as.data.frame(varImp(boost.model, numTrees = 10000))
imp <- data.frame(names = rownames(imp), overall = imp$Overall)
imp <- imp[order(imp$overall,decreasing = T),]
imp

n.trees = seq(from = 100, to = 10000, by = 100)

pred = predict(boost.model, newdata = validation_new, n.trees = n.trees)

# RMSE
sqrt(sum((exp(pred) - exp(validation_new$log.SalePrice)) ^ 2) / nrow(validation_new))

# MAE
sum(abs(exp(pred) - exp(validation_new$log.SalePrice))) / nrow(validation_new)
