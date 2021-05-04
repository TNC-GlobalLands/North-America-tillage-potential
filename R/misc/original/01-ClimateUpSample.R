#######################################################################################
#######################################################################################
# RUN THIS FIRST - CREATES A STATISTICAL UP-SAMPLE OF CLIMATE VARIABLES FOR EACH
# COUNTY. ASSUMES THAT THERE ARE CLIMATE RASTERS COVERING ENTIRE STUDY AREA AND
# DEM RASTERS SUB-SET TO EACH COUNTY. PLEASE SOURCE FUNCTIONS IN FUNCTION SECTION. 
#######################################################################################
#######################################################################################
source("D:/CROP/CODE/FUNCTIONS.R")       # ADDS REQUIRED FUNCTIONS
path="D:/CROP/COLORADO"                  # PATH TO STATE DATABASE
cpath=paste(path, "CLIMATE", sep="/")         # PATH TO 1K CLIMATE RASTERS
dirs <- list.files(path, pattern = "CO") # PREFIX FOR STATE/COUNTY
ylist <- c("adi", "dd5", "map", "mat") 
xraster="elev.img"

  for (d in dirs) {                
    setwd(paste(paste(path, d, sep="/"), "RASTER", sep="/"))
      x <- paste(getwd(), xraster, sep="/") 
    for(r in ylist) { 
      RasterUpSample(x=x, y=paste(cpath, paste(r, "img", sep="."), sep="/"), p=0.02, 
  	   sample.type="random", filename=paste(getwd(), paste(r, "img", sep="."), sep="/") )
	gc()
  	} 
  }
