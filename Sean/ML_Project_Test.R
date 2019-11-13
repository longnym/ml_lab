library(corrplot)
require(car)

# check correlation
corrplot.mixed(cor(tot.df[,sapply(tot.df, is.numeric)], use='complete.obs'), number.cex = 0.4, tl.cex=0.7, tl.pos = 'lt', order = 'hclust')

# multi linear regression model
sale_model <- lm(SalePrice ~ ., data=train_new)

# remove dummy singularities
sale_model <- update(sale_model, . ~ . - MSSubClass.150 - Neighborhood.Blueste)
summary(sale_model)
vif(sale_model)

test_new$Predict <- predict(sale_model, newdata=test_new)

# RMSE
sqrt(sum((test_new$Predict - test_new$SalePrice) ^ 2) / nrow(test_new))

# MAE
sum(abs(test_new$Predict - test_new$SalePrice)) / nrow(test_new)
