rm(list = ls())

# 1 divide train and test sets ####
data <- read.csv(file="./merged_data.csv", head=T)
# remove possible outliners
summary(data$AveAGB)
hist(data$AveAGB,breaks=100)
data <- data[data$AveAGB<250,]
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
data.train <- data[ind==1,]
data.test<- data[ind==2,]

# 2 select features ####
library(VSURF)
colnames(data)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}
# agb 13
# dem 14:16
# s1 17:28
# p2 29:33
# l8 34:130(technical metric -> 120:122)

xs <- data[c(14:130)]
y <- data[[13]]
s1.vsurf <- VSURF(xs,y,parallel=TRUE)
# quick summary
summary(s1.vsurf)
names(s1.vsurf)
colnames(xs[s1.vsurf$varselect.pred])
plot(s1.vsurf)
s1.vsurf$err.pred

sel.band <- names(xs[s1.vsurf$varselect.pred])

# 
library(randomForest)
rf_model <- randomForest(x=data.train[sel.band], y=data.train$AveAGB, data=data.train, importance=TRUE,ntree=500,mtry=4)
# train data 
rf_model$predicted
data.train$AveAGB
summary(rf_model$predicted)
summary(data.train$AveAGB)
err_train=data.train$AveAGB-rf_model$predicted
rmse_train <- rmse(err_train) 
rmse_train
r2_train <- r2(data.train$AveAGB, rf_model$predicted)
r2_train
plot(data.train$AveAGB, rf_model$predicted,xlim=c(0,200),ylim=c(0,200))


# test data
test_pred <- predict(rf_model, newdata=data.test)
err_test=data.test$AveAGB-test_pred
rmse_test <- rmse(err_test)
rmse_test
r2_test <- r2(data.test$AveAGB, test_pred)
r2_test

