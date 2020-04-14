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

# 2 select features using vsurf and quick test using randomforest ####
library(VSURF)
library(randomForest)
colnames(data)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

# 2.1 look up table ####
# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24
# p2 25:29
# l8 30:126 (technical metric -> 116:118 vegetation index -> 119:126)

# 2.2 landsat-8 ####
# 2.2.1 selecting with VSURF 
xs <- data[c(30:126)]
y <- data[[9]]
vsurf.l8 <- VSURF(xs,y,parallel=TRUE)
# quick summary
summary(vsurf.l8)
plot(vsurf.l8)
# 2.2.2 testing with randomForest
sel.l8 <- names(xs[vsurf.l8$varselect.pred])
save(sel.l8, file="sel.l8.Rdata")
rf.model <- randomForest(x=data.train[sel.band], y=data.train$AveAGB, data=data.train, importance=TRUE,ntree=500,mtry=4)
# train data 
pred.train = rf.model$predicted
summary(pred.train);summary(data.train$AveAGB)
rmse.train <- rmse(data.train$AveAGB-pred.train); rmse.train
r2.train <- r2(data.train$AveAGB, pred.train); r2.train
plot(data.train$AveAGB, pred.train,xlim=c(0,200),ylim=c(0,200))
# test data
pred.test <- predict(rf.model, newdata=data.test)
summary(pred.test);summary(data.test$AveAGB)
rmse.test <- rmse(data.test$AveAGB-pred.test);rmse.test
r2.test <- r2(data.test$AveAGB, pred.test);r2.test
plot(data.test$AveAGB, pred.test,xlim=c(0,200),ylim=c(0,200))


# 2.3 sentinel-1 palsar-2 ####
# 2.3.1 selecting with VSURF 
xs <- data[c(13:29)]
y <- data[[9]]
vsurf.s1p2 <- VSURF(xs,y,parallel=TRUE)
# quick summary
summary(vsurf.s1p2)
plot(vsurf.s1p2)
# 2.3.2 testing with randomForest
sel.s1p2 <- names(xs[vsurf.s1p2$varselect.pred])
save(sel.s1p2, file="sel.s1p2.Rdata")
rf.model <- randomForest(x=data.train[sel.band], y=data.train$AveAGB, data=data.train, importance=TRUE,ntree=500,mtry=4)
# train data 
pred.train = rf.model$predicted
summary(pred.train);summary(data.train$AveAGB)
rmse.train <- rmse(data.train$AveAGB-pred.train); rmse.train
r2.train <- r2(data.train$AveAGB, pred.train); r2.train
plot(data.train$AveAGB, pred.train,xlim=c(0,200),ylim=c(0,200))
# test data
pred.test <- predict(rf.model, newdata=data.test)
summary(pred.test);summary(data.test$AveAGB)
rmse.test <- rmse(data.test$AveAGB-pred.test);rmse.test
r2.test <- r2(data.test$AveAGB, pred.test);r2.test
plot(data.test$AveAGB, pred.test,xlim=c(0,200),ylim=c(0,200))


# 2.4 selected l8 s1 p2 + dem ####
# 2.4.1 selecting with VSURF 
load("sel.l8.Rdata")
load("sel.s1p2.Rdata")
xs <- data[c(sel.l8,sel.s1p2,10:12)]
y <- data[[9]]
vsurf.l8s1p2dem <- VSURF(xs,y,parallel=TRUE)
# quick summary
summary(vsurf.l8s1p2dem)
plot(vsurf.l8s1p2dem)
# 2.4.2 testing with randomForest
l8s1p2dem.sel <- names(xs[vsurf.l8s1p2dem$varselect.pred])
save(l8s1p2dem.sel,"sel.l8s1p2dem.Rdata")
rf.model <- randomForest(x=data.train[sel.band], y=data.train$AveAGB, data=data.train, importance=TRUE,ntree=500,mtry=4)
# train data 
pred.train = rf.model$predicted
summary(pred.train);summary(data.train$AveAGB)
rmse.train <- rmse(data.train$AveAGB-pred.train); rmse.train
r2.train <- r2(data.train$AveAGB, pred.train); r2.train
plot(data.train$AveAGB, pred.train,xlim=c(0,200),ylim=c(0,200))
# test data
pred.test <- predict(rf.model, newdata=data.test)
summary(pred.test);summary(data.test$AveAGB)
rmse.test <- rmse(data.test$AveAGB-pred.test);rmse.test
r2.test <- r2(data.test$AveAGB, pred.test);r2.test
plot(data.test$AveAGB, pred.test,xlim=c(0,200),ylim=c(0,200))


# 2.5 all vars ####
# 2.5.1 selecting with VSURF 
xs <- data[c(10:126)]
y <- data[[9]]
vsurf.all <- VSURF(xs,y,parallel=TRUE)
save(vsurf.all, file="vsurf.all.Rdata")
# quick summary
summary(vsurf.all)
colnames(xs[vsurf.all$varselect.pred])
plot(all.vsurf)
# 2.5.2 testing with randomForest
sel.band <- names(xs[vsurf.all$varselect.pred])
rf.model <- randomForest(x=data.train[sel.band], y=data.train$AveAGB, data=data.train, importance=TRUE,ntree=500,mtry=4)
# train data 
pred.train = rf.model$predicted
summary(pred.train);summary(data.train$AveAGB)
rmse.train <- rmse(data.train$AveAGB-pred.train); rmse.train
r2.train <- r2(data.train$AveAGB, pred.train); r2.train
plot(data.train$AveAGB, pred.train,xlim=c(0,200),ylim=c(0,200))
# test data
pred.test <- predict(rf.model, newdata=data.test)
summary(pred.test);summary(data.test$AveAGB)
rmse.test <- rmse(data.test$AveAGB-pred.test);rmse.test
r2.test <- r2(data.test$AveAGB, pred.test);r2.test
plot(data.test$AveAGB, pred.test,xlim=c(0,200),ylim=c(0,200))

