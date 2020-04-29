library(nnet)

data <-read.csv(file="./merged_data.csv", head=T)
data <- data[data$AveAGB<250,]
data <- data[,c(9,30:115,119:126)]

ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
data.train <- data[ind==1,]
data.test<- data[ind==2,]

n <- names(data)
f <- as.formula(paste("AveAGB ~", paste(n[!n %in% "AveAGB"], collapse = " + ")))

res <- nnet(AveAGB ~ .,
            data=data.train,
            size=30, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

pred.train <- res$fitted.values
pred.test <- predict(res, newdata=data.test)

rmse <- function(error)
{
  sqrt(mean(error^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

rmse(data.train$AveAGB-pred.train)
r2(data.train$AveAGB,pred.train)
rmse(data.test$AveAGB-pred.test)
r2(data.test$AveAGB,pred.test)



