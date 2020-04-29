rm(list=ls())

data<-read.csv(file="./merged_data.csv", head=T)
colnames(data)
data <- na.omit(data)
# remove possible outliners
summary(data$AveAGB)
hist(data$AveAGB,breaks=100)
data <- data[data$AveAGB<250,]
hist(data$AveAGB,main="",xlab="样地平均生物量（Mg/ha）",ylab="频数",breaks=50,xlim=c(1,250))


