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
load("sel.ht.Rdata")
d <- d[c("AveHt.1",sel.ht)]
d <- as.matrix(d) 
rcorr(d)
