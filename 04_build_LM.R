# select metrics(MASS)
# calculate model performance(ten-fold validation)
# build model and save as .Rdata

rm(list = ls())

library(MASS)

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

fit<-lm(AveAGB~.,data = data)

step.back <- stepAIC(fit,direction = "backward",trace=F)
summary(step.back)

step.for <- stepAIC(fit,direction = "forward",trace=F)
summary(step.for)

step.both <- stepAIC(fit,direction = "both",trace=F)
summary(step.both)

# use metrics selected by backward stepwise regression
sel.lm <- names(step.back$coefficients)
sel.lm
sel.lm <- sel.lm[2:length(sel.lm)] 
sel.lm
save(sel.lm,file="sel.lm.Rdata")


# 10-fold validation ####
rmse <- function(x,y)
{
  sqrt(mean((x-y)^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

load("sel.lm.Rdata")
data <- data[c("AveAGB",sel.lm)]

# 10-fold
tenfold <- function(){
  df <- data.frame(rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r2.train=numeric(), 
                   r2.test=numeric()) 
  ind <- sample(10, nrow(data), replace=T)
  for (i in 1:10){
    data.train <- data[ind!=i,]
    data.test<- data[ind==i,]
    model.lm<-lm(AveAGB~.,data = data.train)
    pred.train <- predict(model.lm,data.train)
    pred.test <- predict(model.lm,data.test)
    r2.train <- r2(data.train$AveAGB,pred.train)
    r2.test <- r2(data.test$AveAGB,pred.test)
    rmse.train <- rmse(data.train$AveAGB,pred.train)
    rmse.test <- rmse(data.test$AveAGB,pred.test)
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r2.train,r2.test)
  }
  c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r2.train),mean(df$r2.test))
}

# result
result.lm <- tenfold()
result.lm
save(result.lm,file="result.lm.Rdata")

# model
model.lm<-lm(AveAGB~.,data = data)
save(model.lm,file="model.lm.Rdata")

