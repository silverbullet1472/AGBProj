rm(list = ls())
library(VSURF)
library(randomForest)

# 1 divide train and test sets ####
set.seed(6666)
data <- read.csv(file="./merged_data.csv", head=T)
# remove possible outliners
summary(data$AveAGB)
hist(data$AveAGB,breaks=100)
data <- data[data$AveAGB<250,]
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
data.train <- data[ind==1,]
data.test<- data[ind==2,]

# 2 select features using vsurf and quick test using randomforest ####
colnames(data)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

# 2.1 rf test method ####
featureTest <- function(xs,y){
  trees = seq(500,2500,500)
  mtrys= 1:(length(xs)-1)
  df <- data.frame(ntree=numeric(),
                   mtry=numeric(), 
                   mse=numeric(), 
                   rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r2.train=numeric(), 
                   r2.test=numeric()) 
  for (i in trees) {
    for (j in mtrys) {
      rf.model = randomForest(x=data.train[xs], y=data.train[[y]], data=data.train, ntree=i, mtry=j,importance=TRUE)
      # train data 
      pred.train = rf.model$predicted
      summary(pred.train);summary(data.train[[y]])
      rmse.train <- rmse(data.train[[y]]-pred.train); rmse.train
      r2.train <- r2(data.train[[y]], pred.train); r2.train
      # plot(data.train[[y]], pred.train,xlim=c(0,200),ylim=c(0,200))
      # test data
      pred.test <- predict(rf.model, newdata=data.test)
      summary(pred.test);summary(data.test[[y]])
      rmse.test <- rmse(data.test[[y]]-pred.test);rmse.test
      r2.test <- r2(data.test[[y]], pred.test);r2.test
      # plot(data.test[[y]], pred.test,xlim=c(0,200),ylim=c(0,200))
      # add to df
      df[nrow(df)+1,] <- c(i,j,mean(rf.model$mse),rmse.train,rmse.test,r2.train,r2.test)
    }
  }
  df
}

# 2.2 look up table ####
# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24 
# p2 25:29
# l8 30:126 (technical metric -> 116:118 vegetation index -> 119:126)


# 2.3 landsat-8 ####
# 2.3.1 selecting with VSURF 
xs <- data[c(30:126)]
y <- data[[9]]
vsurf.l8 <- VSURF(xs,y,parallel=TRUE)
save(vsurf.l8,file = "vsurf.l8.Rdata")
# quick summary
summary(vsurf.l8)
names(xs[vsurf.l8$varselect.pred])
sel.l8 <- names(xs[vsurf.l8$varselect.pred])
save(sel.l8, file="sel.l8.Rdata")
plot(vsurf.l8)
# 2.3.2 testing with randomForest
result.l8 <- featureTest(sel.l8,"AveAGB")
summary(result.l8)
save(result.l8,file = "result.l8.Rdata")


# 2.4 sentinel-1 palsar-2 ####
# 2.4.1 selecting with VSURF 
xs <- data[c(13:29)]
y <- data[[9]]
vsurf.s1p2 <- VSURF(xs,y,parallel=TRUE)
save(vsurf.s1p2,file = "vsurf.s1p2.Rdata")
# quick summary
summary(vsurf.s1p2)
names(xs[vsurf.s1p2$varselect.pred])
sel.s1p2 <- names(xs[vsurf.s1p2$varselect.pred])
save(sel.s1p2, file="sel.s1p2.Rdata")
plot(vsurf.s1p2)
# 2.4.2 testing with randomForest
result.s1p2 <- featureTest(sel.s1p2,"AveAGB")
save(result.s1p2,file = "result.s1p2.Rdata")
summary(result.s1p2)

# 2.5 selected l8 s1 p2 + dem ####
# 2.5.1 selecting with VSURF 
load("sel.l8.Rdata")
load("sel.s1p2.Rdata")
xs <- data[c(sel.l8,sel.s1p2,"dem","slope","aspect")]
y <- data[[9]]
vsurf.l8s1p2dem <- VSURF(xs,y,parallel=TRUE)
save(vsurf.l8s1p2dem,file = "vsurf.l8s1p2dem.Rdata")
# quick summary
summary(vsurf.l8s1p2dem)
names(xs[vsurf.l8s1p2dem$varselect.pred])
sel.l8s1p2dem <- names(xs[vsurf.l8s1p2dem$varselect.pred])
save(sel.l8s1p2dem,file="sel.l8s1p2dem.Rdata")
plot(vsurf.l8s1p2dem)
# 2.5.2 testing with randomForest
result.l8s1p2dem <- featureTest(sel.l8s1p2dem,"AveAGB")
save(result.l8s1p2dem,file = "result.l8s1p2dem.Rdata")
summary(result.l8s1p2dem)

# 2.6 all vars ####
# 2.6.1 selecting with VSURF 
xs <- data[c(10:126)]
y <- data[[9]]
vsurf.all <- VSURF(xs,y,parallel=TRUE)
save(vsurf.all,file = "vsurf.all.Rdata")
# quick summary
summary(vsurf.all)
names(xs[vsurf.all$varselect.pred])
sel.all <- names(xs[vsurf.all$varselect.pred])
save(sel.all, file="sel.all.Rdata")
plot(vsurf.all)
# 2.6.2 testing with randomForest
result.all <- featureTest(sel.all,"AveAGB")
save(result.all,file="result.all.Rdata")
summary(result.all)

# 2.7 height ####
# 2.7.1 selecting with VSURF 
xs <- data[c(10:126)]
y <- data[[3]]
vsurf.ht <- VSURF(xs,y,parallel=TRUE)
save(vsurf.ht,file="vsurf.ht.Rdata")
# quick summary
summary(vsurf.ht)
names(xs[vsurf.ht$varselect.pred])
sel.ht <- names(xs[vsurf.ht$varselect.pred])
save(sel.ht, file="sel.ht.Rdata")
plot(vsurf.ht)
# 2.7.2 testing with randomForest
result.ht <- featureTest(sel.ht,"AveHt.1")
save(result.ht,file="result.ht.Rdata")
summary(result.ht)

