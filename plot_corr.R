# plot correlations
#if(!require(corrplot)) { install.packages("corrplot") }
library(corrplot)

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

do_corrplot <- function(dd){
  cor <- cor(dd, method="pearson", use="complete.obs")
  # matrix of the p-value of the correlation
  res <- cor.mtest(dd,0.99)
  p.mat = res[[1]]
  corrplot(cor, col=col(200),
           type="full", method="color",
           addCoef.col = "grey", # "black" or NULL Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = 0.01, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag=FALSE 
  ) 
}

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# plot 5:13
# agb 13
# dem 14:16
# s1 17:28
# p2 29:33
# l8 34:130(technical metric -> 120:122 vegetation index -> 123:130)

# agb - l8 index
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(13,123:130)] 
d <- as.matrix(d) 
do_corrplot(d) 

# plot - l8 index
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(5:13,123:130)] 
d <- as.matrix(d) 
do_corrplot(d) 

# agb - s1 p2
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(13,17:33)] 
d <- as.matrix(d) 
do_corrplot(d) 

# plot - s1 p2
d<-read.csv(file="./merged_data.csv", head=T)
d <- d[c(5:13,17:33)] 
d <- as.matrix(d) 
do_corrplot(d) 
 