rm(list = ls())

data <- read.csv(file="./merged_data.csv", head=T)
data <- as.data.frame(scale(data))
data <- data[data$AveAGB<250,]
data <- data[c(9,12:126)]
library(MASS)
# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24 
# p2 25:29
# l8 30:126 (technical metric -> 116:118 vegetation index -> 119:126)
fit<-lm(AveAGB~.,data = data)
step.back <- stepAIC(fit,direction = "backward")
summary(step.back)

step.for <- stepAIC(fit,direction = "forward")
summary(step.for)

step.both <- stepAIC(fit,direction = "both")
summary(step.both)
