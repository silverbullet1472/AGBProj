# Randomforest
rm(list=ls()); gc();
#LOAD the packages
library('raster')
library('rgdal')

### 1a. pre-setting ####
path.prj = 'C:/MyFiles/Research/GraduationDesign/Model/AGBProj'
path.s1 = paste(path.prj,'ModelData','Sentinel',sep='/') 
path.l8 = paste(path.prj,'ModelData','Landsat',sep='/') 
path.dem = paste(path.prj,'ModelData','DEM',sep='/')
path.rf = paste(path.prj,'ModelData','RF',sep='/')
prj0 <- '+proj=longlat +datum=WGS84 +no_defs'

#### 1b. tile look-up table ####
tlist=c('109E_28N','109E_29N',
        '110E_28N','110E_29N',
        '111E_28N','111E_29N')

#### 1c. variable look-up table ####
load("sel.rf.Rdata")
sel.rf
fls.s1=c('IW_Gamma0_VV_mean', 'IW_Gamma0_VH_mean')
fls.l8=c('2017_red_avsmin25', '2017_green_min_SVVI', 
         '2017_green_avsmin25', '2017_red_min', 
         '2017_red_av2575', '2017_blue_avsmin25', 
         '2017_swir1_max_RN','2017_green_max_SVVI',
         '2017_red_max_SVVI','2017_swir2_avsmin25',
         '2017_swir1_min','2017_SVVI_av2575'
         )
fls.dem=c('dem')

### function to check file exists #####
check_file_exist<-function(fname){
  if (!file.exists(fname)) {
    print(paste0('file does not exist! please check: ',fname)) 
  }
}
### function to crop raster #####
crop_raster<-function(inrst, refrst){
  e <- as(extent(refrst), 'SpatialPolygons')
  crs(e) <- prj0
  outr <- crop(inrst, e)
  return(outr)
}

#### 2. read tiles #####
##'N29E109'(p2) == '109E_28N' (s1/l8/tdm) 
### fucntion to read and stack tile #####
read_stack_tile<-function(stile){
  print(paste0('  tile ',stile))
  
  rsts <- list()
  n.s1 <- length(fls.s1)
  n.l8 <- length(fls.l8)
  n.dem <- length(fls.dem)
  ### 2a. sentinel-1 #####
  #### 109E_28N: IW_Gamma0_VV_mean.tif IW_Gamma0_VH_mean.tif
  print('   reading sentinel-1 variables ...')
  setwd(path.s1)
  # read in for crop landsat
  s1vvf=paste0(path.s1, '/', stile, '/','IW_Gamma0_VV_mean.tif');check_file_exist(s1vvf); s1vv=raster(s1vvf)
  ##
  for(i in 1:n.s1){
    fname = paste0(fls.s1[i],".tif")
    fpath = paste(path.s1, stile, fname , sep='/')
    check_file_exist(fpath)
    rsts[i] <- raster(fpath)
    names(rsts)[i] <- fls.s1[i]
  }
  ### 2b. landsat #####
  ### '109E_28N' red.tif
  print('   reading landsat variables ...')
  setwd(path.l8)
  ##
  for(i in 1:n.l8){
    fname = paste0(fls.l8[i],".tif")
    fpath = paste(path.l8, stile, fname , sep='/')
    check_file_exist(fpath)
    tmprst = raster(fpath)
    rsts[i+n.s1] <- crop_raster(tmprst, s1vv)
    names(rsts)[i+n.s1] <- fls.l8[i]
  }
  
  ### 2c. dem #####
  print('   reading DEM variables ...')
  setwd(path.dem)
  ##
  for(i in 1:n.dem){
    fname = paste0(fls.dem[i],".tif")
    fpath = paste(path.dem, stile, fname , sep='/')
    check_file_exist(fpath)
    tmprst = raster(fpath)
    rsts[i+n.s1+n.l8] <- crop_raster(tmprst, s1vv)
    names(rsts)[i+n.s1+n.l8] <- fls.dem[i]
  }

  ### 3. stack variables #####
  print('   stacking variables ...')
  rsts.stack <- stack(rsts)
  vars <- as.data.frame(rsts.stack)
  names(vars) = names(rsts)
  
  ### 4. save vars
  path.out=paste(path.rf,stile,sep='/')
  if(!file.exists(path.out)) dir.create(path.out) 
  varf=paste(path.out,'vars.rf.RData',sep='/')
  save(vars,file=varf)
  
  ### 5 save coordinates: xy
  print('    stacking x y coordinates ...')
  s1obj = new('GDALDataset',s1vvf)
  y=getRasterTable(s1obj)$y
  x=getRasterTable(s1obj)$x
  xyf=paste(path.out,'xy.rf.RData',sep='/')
  save(x, y, file=xyf)
  print('    clean up variables ...')
  rm(rsts.stack,rsts,vars, x, y); gc();
}

# test one
i=1; stile=tlist[i]
read_stack_tile(stile)

for(i in 1:length(tlist)){
  stile=tlist[i]   #'109E_28N'
  print(paste0(' start: #', i,': tile ',stile))
  read_stack_tile(stile)
  print(paste0(' done:  #', i,': tile ',stile))
}
 
