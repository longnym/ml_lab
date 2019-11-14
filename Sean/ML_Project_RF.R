library(randomForest)

# set.seed(0)
# oob.err = numeric(10)
# for (mtry in 1:10) {  # 51~60
#   fit = randomForest(SalePrice ~ ., data = train_new, mtry = mtry + 50)
#   oob.err[mtry] = fit[['mse']][500]
#   cat("We're performing iteration", mtry, "\n")
# }
# 
# plot(1:10, oob.err, pch = 16, type = "b",
#      xlab = "Variables Considered at Each Split",
#      ylab = "OOB Mean Squared Error",
#      main = "Random Forest OOB Error Rates\nby # of Variables")

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
