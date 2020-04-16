rm(list=ls()); gc();
#LOAD the packages
library('raster')
library('rgdal')
library('glcm')

### 1a. pre-setting ####
path_prj = 'C:/MyFiles/Research/GraduationDesign/Model/DataProcessing'
path_l8 = paste(path_prj,'L8',sep='/') # path to PALSAR-2
path_out = paste(path_prj,'out',sep='/') # path to PALSAR-2
prj0 <- '+proj=longlat +datum=WGS84 +no_defs'

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

calc_texture <- function(ltile){
  ### '109E_28N' red.tif
  files = list.files(paste(path_l8, ltile, sep='/'))
  for(i in 1:length(files)){
    # read raster input
    gc()
    print(paste0('   reading landsat file:',files[i]))
    band_name <- paste0("l8",substr(files[i],6,6))
    f=paste(path_l8, ltile, files[i], sep='/')
    check_file_exist(f) 
    r=raster(f)
    # mkdir
    folder_out=paste(path_out,ltile,sep='/')
    if(!file.exists(folder_out)) dir.create(folder_out,recursive = T) 
    # cal texture
    stats <- c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy","second_moment", "correlation")
    for(j in 1:length(stats)){
      print(paste0('    calculating stats:',stats[j]))
      t <- glcm(r,n_grey = 64,window = c(7,7),statistics = stats[j])
      # glcm(x, n_grey = 32, window = c(3, 3), shift = c(1, 1), statistics = 
      #        c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
      #          "second_moment", "correlation"), min_x=NULL, max_x=NULL, na_opt="any", 
      #      na_val=NA, scale_factor=1, asinteger=FALSE)
      stats_name = substr(stats[j],1,4)
      file_name = paste0(band_name,stats_name,".tif")
      writeRaster(t,file=paste(folder_out,file_name,sep="/"),overwrite=TRUE)
      print('    end this stats, clean up variables ...')
      gc()
    }
  }
}

tlist = list.files(path_l8)
for(i in 1:length(tlist)){
  ltile=tlist[i]   #'109E_28N'
  print(paste0(' start: #', i))
  calc_texture(ltile)
  print(paste0(' done:  #', i,': tile ',ltile))
}
