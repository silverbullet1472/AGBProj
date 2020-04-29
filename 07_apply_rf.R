rm(list=ls()); gc();
library(randomForest)
#### apply rf model #####
##### 0. load rf models -- selband.rf.best ######
path.prj = 'C:/MyFiles/Research/GraduationDesign/Model/AGBProj'
path.map = 'C:/MyFiles/Research/GraduationDesign/Model/AGBProj/ModelData/RF/Map'
setwd(path.prj)


##### 1. load a 1x1 deg stacked tile (Rdata) ####
path.rdata = 'C:/MyFiles/Research/GraduationDesign/Model/AGBProj/ModelData/RF'

tlist=c('109E_28N','109E_29N',
        '110E_28N','110E_29N',
        '111E_28N','111E_29N')


# xyf = paste(path.rdata,itile,'xy.rf.RData',sep='/')
# rdataf = paste(path.rdata,itile,'vars.rf.RData',sep='/')
# load(xyf)  
# load(rdataf)
# colnames(vars)



# pow2db <- function(p){
#   return (10*log10(p))
# }
# 
# vars$IW_Gamma0_VV_mean <- pow2db(vars$IW_Gamma0_VV_mean)
# vars$IW_Gamma0_VH_mean <- pow2db(vars$IW_Gamma0_VH_mean)
# vars$s1vvrvh <- vars$IW_Gamma0_VV_mean - vars$IW_Gamma0_VH_mean
# vars$IW_Gamma0_VV_mean <- NULL
# vars$IW_Gamma0_VH_mean <- NULL
# 
# t <- colnames(vars);t
# sel.rf
# t[1:12] <- sel.rf[c(1:9,11:13)]
# colnames(vars) <- t;t

### make map
library(raster)
make_map <- function(itile){
  ### 1 load RData
  xyf = paste(path.rdata,itile,'xy.rf.RData',sep='/')
  rdataf = paste(path.rdata,itile,'vars.rf.RData',sep='/')
  self = paste(path.prj,'sel.rf.RData',sep='/')
  modelf = paste(path.prj,'model.rf.RData',sep='/')
  load(xyf)  
  load(rdataf)
  load(self)
  load(modelf)
  ### 2 prep vars
  pow2db <- function(p){
    return (10*log10(p))
  }
  vars$IW_Gamma0_VV_mean <- pow2db(vars$IW_Gamma0_VV_mean)
  vars$IW_Gamma0_VH_mean <- pow2db(vars$IW_Gamma0_VH_mean)
  vars$s1vvrvh <- vars$IW_Gamma0_VV_mean - vars$IW_Gamma0_VH_mean
  vars$IW_Gamma0_VV_mean <- NULL
  vars$IW_Gamma0_VH_mean <- NULL
  # rename
  t <- colnames(vars);t
  t[1:12] <- sel.rf[c(1:9,11:13)]
  colnames(vars) <- t;t
  
  ### 3 make prediction
  pred_name=paste0('agb_',itile)
  pred_out <- rep(NA,nrow(vars))
  pred_out[complete.cases(vars)] <- predict(model.rf, newdata=na.omit(vars))
  pred_out_df=as.data.frame(cbind(x, y, pred_out))
  names(pred_out_df)[1]="x"
  names(pred_out_df)[2]="y"
  names(pred_out_df)[3]="pred"
  pred_rst=rasterFromXYZ(pred_out_df)
  #projection(pred_rst)=proj49N
  setwd(path.map)
  writeRaster(pred_rst,file=paste(pred_name,".tif",sep=""),overwrite=TRUE)
  rm(pred_out,pred_out_df,pred_rst)
  gc()
} 


### test one
i=1; itile=tlist[i]
make_map(itile)

for(i in 1:length(tlist)){
  itile=tlist[i]   #'109E_28N'
  print(paste0(' start: #', i,': tile ',itile))
  make_map(itile)
  print(paste0(' done:  #', i,': tile ',itile))
}
