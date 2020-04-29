rm(list = ls())

library(neuralnet)

data <- read.csv(file="./merged_data.csv", head=T)
colnames(data)
data <- na.omit(data)
data <- data[data$AveAGB<250,]
load("sel.all.Rdata")
data <- data[c("AveAGB",sel.all)]

m <- apply(data,2,mean)
s <- apply(data,2,sd)
data <- as.data.frame(scale(data))


ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
data.train <- data[ind==1,]
data.test<- data[ind==2,]

n = colnames(data)
f = as.formula(paste("AveAGB ~", paste(n[!n %in% "AveAGB"], collapse = " + ")))

?neuralnet
nn <- neuralnet(f, data=data.train, hidden=c(3,3), threshold=0.01,rep = 1,stepmax=1e+06)
pred.train <- nn$net.result[[1]][,1]
pred.test <- compute(nn,data.test)$net.result[,1]

plot(data.train[["AveAGB"]],pred.train)
abline(0,1,lwd=2)

plot(data.test[["AveAGB"]],pred.test)
abline(0,1,lwd=2)

r2(data.train[["AveAGB"]],pred.train)
r2(data.test[["AveAGB"]],pred.test)

plot(nn)

rmse <- function(x,y)
{
  sqrt(mean((x-y)^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}


####

rm(list = ls())

library(ANN2)

data <- read.csv(file="./merged_data.csv", head=T)
colnames(data)
data <- na.omit(data)
data <- data[data$AveAGB<250,]
load("sel.all.Rdata")
data <- data[c("AveAGB",sel.all)]

m <- apply(data,2,mean)
s <- apply(data,2,sd)
data <- as.data.frame(scale(data))


ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
data.train <- data[ind==1,]
data.test<- data[ind==2,]

# Train neural network
NN <- neuralnetwork(X = data.train[2:16], y = data.train[1], hidden.layers = 3,loss.type = "squared",n.epochs = 500, L2 =1,regression = T)
# Plot the loss during training
plot(NN)

# Make predictions
pred.test <- predict(NN, newdata = data.test[2:16])$predictions

plot(data.test[["AveAGB"]],pred.test)
abline(0,1,lwd=2)

r2(data.test[["AveAGB"]],pred.test)

rmse <- function(x,y)
{
  sqrt(mean((x-y)^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

