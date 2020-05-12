# calculate correlation coefficient

rm(list=ls())

library(Hmisc)

d<-read.csv(file="./merged_data.csv", head=T)
colnames(d)

# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24 
# p2 25:29
# l8 30:123 (vegetation index -> 116:123)
# agb - selected bands

# 
d <- d[c(9,10:123)]
d <- as.matrix(d) 
corr.matrix <- rcorr(d)
barplot(corr.matrix$r[1,-1],las=2)
corr <- corr.matrix$r[1,]
t <- names(corr[abs(corr) > 0.1]) 
p <- corr.matrix$P[1,-1]

# store vars which have corr > 0.15
topvars.name <- names(corr[abs(corr) > 0.15]) 
topvars.value <- corr[topvars.name]
topvars.pvalue <- p[topvars.name]

topvars.df <- data.frame(topvars.value,topvars.pvalue)
topvars.value
topvars.pvalue
topvars.df

write.csv(topvars.df, row.names=T, file="./topvarscorr.csv")
