rm(list = ls())

# 1 prep evaluating method ####
# quick test on VSURF selected features
# rmse(e) - calculate rmse 
rmse <- function(error)
{
  sqrt(mean(error^2))
}
# r2 - calculate r2
r2 <- function(x,y)
{
  cor(x,y)^2
}

library(randomForest)
# define feature test function
featureTest <- function(train_set,test_set){
  rf_model <- randomForest(AveAGB ~ . , data=train_set, importance=TRUE,ntree=500,mtry=floor(ncol(train_set)/3))
  # train data 
  err_train=train_set$AveAGB-rf_model$predicted
  rmse_train <- rmse(err_train)  
  r2_train <- r2(train_set$AveAGB, rf_model$predicted)
  # test data
  test_pred <- predict(rf_model, newdata=test_set)
  err_test=test_set$AveAGB-test_pred
  rmse_test <- rmse(err_test)
  r2_test <- r2(test_set$AveAGB, test_pred)
  # print train set info
  print(rf_model)
  print("For training set-->")
  print("Actual-->")
  print(summary(train_set$AveAGB))
  print("Prediction-->")
  print(summary(rf_model$pred))
  print(paste("rmse:",rmse_train))
  print(paste("r2:",r2_train))
  # print test set info
  print("for test set-->")
  print("Actual-->")
  print(summary(test_set$AveAGB))
  print("Prediction-->")
  print(summary(rf_model$pred))
  print(paste("rmse:",rmse_test))
  print(paste("r2:",r2_test))
  # making some simple plot 
  par(mfrow=(c(1,2)))
  plot(train_set$AveAGB,  rf_model$pred, col="blue", pch=19) # train
  points(test_set$AveAGB, test_pred, col="red", pch=15) # test
  plot(rf_model)
  varImpPlot(rf_model) 
}

# 2 test selected features ####
# l8
load("l8.vsurf.Rdata")
load(file="l8_selected_train.Rdata")
load(file="l8_selected_test.Rdata")
featureTest(l8_selected_train,l8_selected_test)
# s1
load("s1.vsurf.Rdata")
load(file="s1_selected_train.Rdata")
load(file="s1_selected_test.Rdata")
featureTest(s1_selected_train,s1_selected_test)
# p2
load("vsurf_p2.Rdata")
load(file="p2_selected_train.Rdata")
load(file="p2_selected_test.Rdata")
featureTest(p2_selected_train,p2_selected_test)

# 3 use selected features
# dem <- read.csv(file="./dem.csv", head=T)
# dem_all <- merge(plots,dem,by="FIDs")
# colnames(dem_all)
# dem_train <- dem_all[dem_all$FIDs%in%trainIDs,]
# dem_train[c(1:12,14:16)] <- NULL;colnames(dem_train)
# dem_test <- dem_all[dem_all$FIDs%in%testIDs,]
# dem_test[c(1:12,14:16)] <- NULL;colnames(dem_test)

# l8 + s1 + p2
train_list <- list(l8_selected_train,s1_selected_train,p2_selected_train)
train_total <- Reduce(function(x,y) merge(x,y,by="AveAGB",all=T),train_list)

test_list <- list(l8_selected_test,s1_selected_test,p2_selected_test)
test_total <- Reduce(function(x,y) merge(x,y,by="AveAGB",all=T),test_list)

featureTest(train_total,test_total)

# 4 LM test
# train model
lm_model <- lm(AveAGB~., data=train_total)
save(lm_model, file='lm_total.RData')

# train data
pred_train <- predict(lm_model, train_total)
err_train = train_total$AveAGB-pred_train
rmse_train <- rmse(err_train)  
rmse_train
r2_train <- r2(train_total$AveAGB, pred_train)
r2_train

# test data
pred_test <- predict(lm_model, test_total)
err_test = test_total$AveAGB-pred_test
rmse_test <- rmse(err_test)  
rmse_test 
r2_test <- r2(test_total$AveAGB, pred_test)
r2_test

# pred vs. field
plot(train_total$AveAGB,  pred_train, col="blue", pch=19) # train
points(test_total$AveAGB, pred_test, col="red", pch=15) # test

