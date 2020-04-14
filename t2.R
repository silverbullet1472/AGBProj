library(Hmisc)
set.seed(1)
n <- 100
x1 <- runif(n)
x2 <- runif(n)
x3 <- x1 + x2 + runif(n)/10
x4 <- x1 + x2 + x3 + runif(n)/10
redun(~x1+x2+x3+x4, r2=.8)
redun(~x1+x2+x3+x4+x5+x6, r2=.8, minfreq=40)
redun(~x1+x2+x3+x4+x5+x6, r2=.8, allcat=TRUE)

library(randomForest)
rm(list=ls())
set.seed(131)
data("airquality")
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)

names(ozone.rf)

ozone.rf$mse
ozone.rf$rsq



head(airquality)
a <- data.frame("Solar.R"=190,"Wind"=7.4,"Temp"=67,"Month"=5,"Day"=1)

a <- data.frame("Solar.R"=190,"Wind"=7.4,"Temp"=67,"Month"=5)
predict(ozone.rf, a)
