rm(list=ls())

library(Hmisc)

d<-read.csv(file="./merged_data.csv", head=T)
load("sel.all.Rdata")
d <- d[c("AveAGB",sel.all)]
d <- as.matrix(d) 
corr <- rcorr(d)
barplot(corr$r[1,-1],names.arg = sel.all,las=2)

d<-read.csv(file="./merged_data.csv", head=T)
load("sel.all.Rdata")
d <- as.matrix(d[9:126]) 
corr <- rcorr(d)
barplot(corr$r[1,-1],las=2)

d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(3,10:126)]
d <- as.matrix(d) 
corr <- rcorr(d)
barplot(corr$r[1,-1],las=2)

d<-read.csv(file="./merged_data.csv", head=T)
load("sel.ht.Rdata")
