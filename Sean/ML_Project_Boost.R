library(gbm)

# MSSubClass.150, Neighborhood.Blueste has no variance
set.seed(0)
boost.model = gbm(SalePrice ~ ., data = train_new %>% select(-c(MSSubClass.150, Neighborhood.Blueste)),
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 4)

summary(boost.model)

n.trees = seq(from = 100, to = 1000, by = 10)

validation_new$Predict = NULL
validation_new$Predict = predict(boost.model, newdata = validation_new, n.trees = n.trees)

# RMSE
sqrt(sum((validation_new$Predict - validation_new$SalePrice) ^ 2) / nrow(validation_new))

# MAE
sum(abs(validation_new$Predict - validation_new$SalePrice)) / nrow(validation_new)
