rm(list = ls())

rmse <- function(error)
{
  sqrt(mean(error^2))
}
r2 <- function(x,y)
{
  cor(x,y)^2
}

data <- read.csv(file="./merged_data.csv", head=T)
data <- data[data$AveAGB<250,]

attach(data)
plot((AveDBH/100)^2*AveHt.1,AveAGB)
r2((AveDBH/100)^2*AveHt.1,AveAGB)
summary(AveHt.1)
summary(AveDBH)
summary((AveDBH/100)^2*AveHt.1)

summary(pi*(AveDBH/200)^2)
summary(AveBasalAr)
