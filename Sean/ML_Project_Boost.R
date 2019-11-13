library(gbm)

set.seed(0)
boost.model = gbm(SalePrice ~ ., data = train_new,
                  distribution = "gaussian",
                  n.trees = 10000,
                  interaction.depth = 4)

summary(boost.model)

n.trees = seq(from = 100, to = 10000, by = 100)

validation_new$Predict = NULL
validation_new$Predict = predict(boost.model, newdata = validation_new, n.trees = n.trees)

# RMSE
sqrt(sum((validation_new$Predict - validation_new$SalePrice) ^ 2) / nrow(validation_new))

# MAE
sum(abs(validation_new$Predict - validation_new$SalePrice)) / nrow(validation_new)
