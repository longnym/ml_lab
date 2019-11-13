library(randomForest)

set.seed(0)
rf.model = randomForest(SalePrice ~ ., data = train_new)  # default : num_var / 3
rf.model

importance(rf.model)
varImpPlot(rf.model)

validation_new$Predict = NULL
validation_new$Predict = predict(rf.model, validation_new, type = "class")

# RMSE
sqrt(sum((validation_new$Predict - validation_new$SalePrice) ^ 2) / nrow(validation_new))

# MAE
sum(abs(validation_new$Predict - validation_new$SalePrice)) / nrow(validation_new)

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