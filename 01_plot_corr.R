rm(list = ls())

# plot correlations to from intuition about data
library(corrplot)

# 1 set up reqiured function

# mat : is a matrix of data 
# ... : further arguments to pass to the native R cor.test function ##### 
cor.mtest <- function(mat, conf.level=0.99) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]      
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  #p.mat
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

do_corrplot <- function(dd, color = "grey", type ="full"){
  cor <- cor(dd, method="pearson", use="complete.obs")
  # matrix of the p-value of the correlation
  res <- cor.mtest(dd,0.99)
  p.mat = res[[1]]
  corrplot(cor, col=col(200),
           type=type, method="color",
           addCoef.col = color, # "black" or NULL Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = 0.01, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag=FALSE 
  ) 
}

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

# 2 plot corr ####

d<-read.csv(file="./merged_data.csv", head=T)
colnames(d)
# 2.1 look up table
# FID 1
# plot parameters 2:8
# agb 9
# dem 10:12
# s1 13:24
# p2 25:29
# l8 30:126 (technical metric -> 116:118 vegetation index -> 119:126)

# 2.2 do plots

# all
# d<-read.csv(file="./merged_data.csv", head=T)
# d <- as.matrix(d) 
# do_corrplot(d,color = NULL) 

# plot parameters
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(2:9)] 
d <- as.matrix(d) 
do_corrplot(d) 

# plot - l8 index
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(2:9,119:126)] 
d <- as.matrix(d) 
do_corrplot(d,color = NULL) 

# plot - s1 p2
d <- read.csv(file="./merged_data.csv", head=T)
d <- d[c(2:9,13:29)] 
d <- as.matrix(d) 
do_corrplot(d,color = NULL) 

# plot - dem
d <- read.csv(file="./merged_data.csv", head=T)
d <- d[c(2:9,10:12)] 
d <- as.matrix(d) 
do_corrplot(d)

# agb - l8 index
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(9,119:126)] 
d <- as.matrix(d) 
do_corrplot(d) 

# agb - s1 p2
d <- read.csv(file="./merged_data.csv", head=T)
d <- d[c(9,13:29)] 
d <- as.matrix(d) 
do_corrplot(d) 

# agb - dem
d <- read.csv(file="./merged_data.csv", head=T)
d <- d[c(9,10:12)] 
d <- as.matrix(d) 
do_corrplot(d)

# agb - selected bands
d<-read.csv(file="./merged_data.csv", head=T)
load("sel.all.Rdata")
d <- d[c("AveAGB",sel.all)]
d <- as.matrix(d) 
do_corrplot(d) 

# ht.1 - selected bands
d<-read.csv(file="./merged_data.csv", head=T)
load("sel.ht.Rdata")
d <- d[c("AveHt.1",sel.ht)]
d <- as.matrix(d) 
do_corrplot(d) 

# explore data
d <- read.csv(file="./merged_data.csv", head=T)
summary(sort(d$AveHt.1)-sort(d$AveHt.2))
summary(d$dem)

