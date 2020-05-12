# select metrics(vsurf)
# select best model parameters and calculate model performance(ten-fold validation)
# build model and save as .Rdata for mapping(code in apply_rf.R)

rm(list = ls())
library(VSURF)
library(randomForest)

# set seed to same num for reproduceable results
set.seed(1234)

# read data ####
data <- read.csv(file="./merged_data.csv", head=T)
data <- na.omit(data)

# remove possible outliners
summary(data$AveAGB)
hist(data$AveAGB,breaks=100)
data <- data[data$AveAGB<250,]

# look up table ####
# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24 
# p2 25:29
# l8 30:123 (vegetation index -> 116:123)

# feature selection ####
xs <- data[c(10:123)]
y <- data[[9]]
vsurf <- VSURF(xs,y,parallel=TRUE)
save(vsurf,file = "vsurf.Rdata") 

# quick summary
load("vsurf.Rdata")
plot(vsurf)
summary(vsurf)
# save result
names(xs[vsurf$varselect.pred])
sel.rf <- names(xs[vsurf$varselect.pred])
save(sel.rf, file="sel.rf.Rdata")
load("sel.rf.Rdata")
sel.rf

# 10-fold validation ####
rmse <- function(x,y)
{
  sqrt(mean((x-y)^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}
tenfold <- function(ntree,mtry){
  df <- data.frame(rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r2.train=numeric(), 
                   r2.test=numeric()) 
  ind <- sample(10, nrow(data), replace=T)
  for (i in 1:10){
    data.train <- data[ind!=i,]
    data.test<- data[ind==i,]
    rf.model = randomForest(x=data.train[sel.rf], y=data.train[["AveAGB"]], ntree=ntree, mtry=mtry,importance=TRUE )
    pred.train <- predict(rf.model,data.train[sel.rf])
    pred.test <- predict(rf.model,data.test[sel.rf])
    rmse.train <- rmse(pred.train,data.train[["AveAGB"]])
    r2.train <- r2(pred.train,data.train[["AveAGB"]])
    rmse.test <- rmse(pred.test,data.test[["AveAGB"]])
    r2.test <- r2(pred.test,data.test[["AveAGB"]])
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r2.train,r2.test)
  }
  c(ntree,mtry,mean(df$rmse.train),mean(df$rmse.test),mean(df$r2.train),mean(df$r2.test))
}

# apply 10-fold
ntrees = c(500,2000)
mtrys= 1:(length(sel.rf)-1)
tune.rf <- data.frame(ntree=numeric(),
                 mtry=numeric(), 
                 rmse.train=numeric(), 
                 rmse.test=numeric(), 
                 r2.train=numeric(), 
                 r2.test=numeric()) 
for (i in ntrees) {
  for (j in mtrys) {
    tune.rf[nrow(tune.rf)+1,] <- tenfold(i,j)
  }
}
# check best parameters
tune.rf

# model
load("sel.rf.Rdata") 
sel.rf
model.rf = randomForest(x=data[sel.rf], y=data[["AveAGB"]], ntree=500, mtry=9,importance=TRUE )
model.rf
save(model.rf,file="model.rf.Rdata")

# result
result.rf <- tenfold(500,7)
save(result.rf, file="result.rf.Rdata")
