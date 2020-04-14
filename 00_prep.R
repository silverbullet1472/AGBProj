rm(list = ls())
# 1 rename some metrics ####
plots <- read.csv(file="../ModelData/plots479_newAGBs_nocoor.csv", head=T)
l8 <- read.csv(file="../ModelData/plots479_l8_vars.csv", head=T)
s1 <- read.csv(file="../ModelData/plots479_s1_vars.csv", head=T)
p2 <- read.csv(file="../ModelData/plots479_p2_vars.csv", head=T)
dem <- read.csv(file="../ModelData/plots479_dem_vars.csv", head=T)

## 1.0 fix AveAGB
plots$AveAGB <- plots$AveAGB/4

## 1.1 plots file
t <- colnames(plots);t
t[2] <- "Radius"
t[5:13] <- c("AveDBH","AveHt.1","BasalArWtHt.1","CrownArWtHt","Den","AveBasalAr","AveHt.2","BasalArWtHt.2","AveAGB")
colnames(plots) <- t;t

## 1.2 landsat file
t <- colnames(l8);t
t[2] <- "Radius";t[5] <- "l8r"
colnames(l8) <- t;t

## 1.3 sentinel-1 file
t <- colnames(s1);t
t[2] <- "Radius";t[5] <- "s1vv";t[9:10] <- c("s1vvmd","s1vhmd")
colnames(s1) <- t;t

## 1.3 palsar-2 file
t <- colnames(p2);t
t[2] <- "Radius";t[5] <- "p2hh"
colnames(p2) <- t;t

## 1.4 dem file
t <- colnames(dem);t
t[2] <- "Radius"
colnames(dem) <- t;t

#  2 calculate sar indexs ####
## 2.1 convert sar to db
### 2.1.1 for sentinel-1
pow2db <- function(p){
  return (10*log10(p))
}
### 2.1.2 for palsar-2
int2db <- function(b1){
  b2=10*log10(b1^2)-83
  return (b2)
}

## 2.2 calculate index
calc_ndpi <- function(b1,b2){
  return ((b1-b2)/(b1+b2))
}

## 2.3 process s1 file
### convert to db
s1$s1vv=pow2db(s1$s1vv) 
s1$s1vvmd=pow2db(s1$s1vvmd)
s1$s1vvsd=pow2db(s1$s1vvsd)
s1$s1vh=pow2db(s1$s1vh)
s1$s1vhmd=pow2db(s1$s1vhmd)
s1$s1vhsd=pow2db(s1$s1vhsd)
### calculate new metrics
s1$s1vvrvh=s1$s1vv-s1$s1vh
s1$s1vvrvhmd=s1$s1vvmd-s1$s1vhmd 
s1$s1vhrvv=s1$s1vh-s1$s1vv
s1$s1vhrvvmd=s1$s1vhmd-s1$s1vvmd 
s1$s1npdi=calc_ndpi(s1$s1vv,s1$s1vh) 
s1$s1npdimd=calc_ndpi(s1$s1vvmd,s1$s1vhmd) 

## 2.4 process p2 file
### convert to db
p2$p2hh=int2db(p2$p2hh)
p2$p2hv=int2db(p2$p2hv)
### calculate new metrics
p2$p2hhrhv=p2$p2hh-p2$p2hv
p2$p2hvrhh=p2$p2hv-p2$p2hh
p2$p2npdi=calc_ndpi(p2$p2hh,p2$p2hv) 

#  3 calculate landsat indexs ####
## 3.1 index function ####
# Normalized Difference Vegetation Index
calc_ndvi <- function(b1,b2){
  return ((b1-b2)/(b1+b2))
}
# Enhanced Vegetation Index
calc_evi <- function(B,R,NIR){
  return (2.5*(NIR - R)/(1 + NIR + 6*R - 7.5*B))
  
}
# Enhanced Vegetation Index 2
calc_evi2 <- function(R,NIR){
  return (2.5*(NIR - R)/(1 + NIR + 2.4*R))
}

# Ratio Vegetation Index	
calc_rvi <- function(R,NIR){
  return (NIR/R)
}
# Tasseled Cap Indices - TCW
calc_tcw <- function(B, G, R, NIR, SWIR1, SWIR2){
  return (0.1509*B+0.1973*G+0.3279*R+0.3406*NIR+0.7112*SWIR1+0.4572*SWIR2)
}
# Tasseled Cap Indices - TCB
calc_tcb <- function(B, G, R, NIR, SWIR1, SWIR2){
  return (0.3037*B+0.2793*G+0.4734*R+0.5585*NIR+0.5082*SWIR1+0.1863*SWIR2)
}	
# Tasseled Cap Indices - TCG
calc_tcg <- function(B, G, R, NIR, SWIR1, SWIR2){
  return (-0.2848*B-0.2435*G-0.5436*R+0.7243*NIR+0.0840*SWIR1-0.1800*SWIR2)
}	
# TCW-TCG Difference - TCWGD
calc_tcwgd <- function(TCW,TCG){
  return (TCW - TCG)
}
### 3.2 process l8 file #####
colnames(l8)
l8$l8ndvi=calc_ndvi(l8$l8nir,l8$l8r)
l8$l8evi=calc_evi(l8$l8b,l8$l8r,l8$l8nir)
l8$l8evi2=calc_evi2(l8$l8r,l8$l8nir)
l8$l8rvi=calc_rvi(l8$l8r,l8$l8nir)
l8$l8tcw=calc_tcw(l8$l8b,l8$l8g,l8$l8r,l8$l8nir,l8$l8sw1,l8$l8sw2)
l8$l8tcb=calc_tcb(l8$l8b,l8$l8g,l8$l8r,l8$l8nir,l8$l8sw1,l8$l8sw2)
l8$l8tcg=calc_tcg(l8$l8b,l8$l8g,l8$l8r,l8$l8nir,l8$l8sw1,l8$l8sw2)
l8$l8tcwgd=calc_tcwgd(l8$l8tcw,l8$l8tcg)

# 4 save files ####
write.csv(plots, row.names=FALSE, file="./plots.csv")
write.csv(l8, row.names=FALSE, file="./l8.csv")
write.csv(s1, row.names=FALSE, file="./s1.csv")
write.csv(p2, row.names=FALSE, file="./p2.csv")
write.csv(dem, row.names=FALSE, file="./dem.csv")

# 5 merge files ####
merge_list <- list(plots,dem,s1,p2,l8)
merged_data <- Reduce(function(x,y) merge(x,y,by=c("FIDs","Radius","YDH","YDMZGUID")),merge_list)
write.csv(merged_data, row.names=FALSE, file="./merged_data.csv")

# 6 quick plot to form intuition about data ####
# 6.1 plots file
colnames(plots)
par(mfrow=(c(2,3)))
hist(plots$AveHt.1,freq = F,breaks = 100)
hist(plots$AveHt.2,freq = F,breaks = 100)
hist(plots$AveDBH,freq = F)
hist(plots$AveBasalAr,freq = F,breaks = 50)
hist(plots$Den,freq = F)
hist(plots$AveAGB,freq = F,breaks = 200)

par(mfrow=(c(2,3)))
plot(plots$AveAGB~plots$AveDBH, pch=19)
plot(plots$AveAGB~plots$Den, pch=19)
plot(plots$AveAGB~plots$AveHt.1, pch=19)
plot(plots$AveAGB~plots$AveHt.2, pch=19)
plot(plots$AveAGB~plots$BasalArWtHt.1, pch=19)
plot(plots$AveAGB~plots$BasalArWtHt.2, pch=19)

# 6.2 sar file
par(mfrow=(c(2,2)))
hist(s1$s1vv,freq = F,breaks = 100)
hist(s1$s1vh,freq = F,breaks = 100)
hist(p2$p2hh,freq = F,breaks = 100)
hist(p2$p2hv,freq = F,breaks = 100)
