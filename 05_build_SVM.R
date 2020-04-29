rm(list = ls())

library(e1071)

##### 3.1 test run using variable selected by VSURF ######
###### 3.1 turned svm model using all variables  #####
## 3.1.1 tune using default parameters #####
data <- read.csv(file="./merged_data.csv", head=T)
colnames(data)

# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24 
# p2 25:29
# l8 30:123 (vegetation index -> 116:123)

data <- na.omit(data)
data <- data[data$AveAGB<250,]
data[1:8] <- NULL
colnames(data)

load("sel.rf.Rdata")
data <- data[c("AveAGB",sel.rf)]

svm.tune <- tune(svm, AveAGB~. , data=data, 
                 range=list(epsilon=seq(0,1,0.01), cost=2^(2:9)))
print(svm.tune)
# Parameter tuning of ‘svm’:
#   
#   - sampling method: 10-fold cross validation 
# 
# - best parameters:
#   epsilon cost
# 0.61    4
# 
# - best performance: 2545.977 

plot(svm.tune)
model.svm<- svm.tune$best.model

## 10-fold
rmse <- function(x,y)
{
  sqrt(mean((x-y)^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

# 10-fold
df <- data.frame(rmse.train=numeric(), 
                 rmse.test=numeric(), 
                 r2.train=numeric(), 
                 r2.test=numeric()) 
ind <- sample(10, nrow(data), replace=T)
for (i in 1:10){
  data.train <- data[ind!=i,]
  data.test<- data[ind==i,]
  model.svm <- svm(AveAGB~.,data=data.train,epsilon=0.57,cost=4)
  pred.train <- predict(model.svm,data.train)
  pred.test <- predict(model.svm,data.test)
  r2.train <- r2(data.train$AveAGB,pred.train)
  r2.test <- r2(data.test$AveAGB,pred.test)
  rmse.train <- rmse(data.train$AveAGB,pred.train)
  rmse.test <- rmse(data.test$AveAGB,pred.test)
  df[nrow(df)+1,] <- c(rmse.train,rmse.test,r2.train,r2.test)
}
# result
result.svm <- c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r2.train),mean(df$r2.test))
result.svm
save(result.svm,file="result.svm.Rdata")
# model
model.svm <- svm(AveAGB~.,data=data,epsilon=0.57,cost=4)
save(model.svm,file="model.svm.Rdata")

