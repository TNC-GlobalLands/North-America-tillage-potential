library(raster)
library(terra)
library(cdlTools)

#dir.create("C:/temp/raster", showWarnings = FALSE)
#terraOptions(memfrac=0.75, tempdir = "C:/temp/raster")
cdl.prj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

root = "C:/evans/tillage"

rclmat <- as.matrix(read.csv(file.path(root, "nass", 
                   "NASS_attributes.csv"))[,c(1,3)])
################################################# 
states = sort(list.dirs(path = file.path(root, "model"),
              full.names = TRUE, recursive = FALSE))
  for(st in states) {
    setwd(st)
    state = basename(st)
	cat("Processing CDL for", state, "\n")
    dir.create(file.path(getwd(), "tmp"), 
               showWarnings = FALSE)
      tmp.dir = file.path(getwd(), "tmp")
    cdlTools::getCDL(cdlTools::fips(basename(st)), c(2008:2019), 
	                 location = tmp.dir)	
    r <- terra::rast(list.files(tmp.dir, "tif$", full.names = TRUE))
	  r <- terra::classify(x=r, rcl=rclmat)
  	    r <- stack(r)
          r <- calc(r, sum)
    writeRaster(r, filename=file.path(st, "nass.tif"), 
	            overwrite = TRUE, options = "COMPRESS=LZW")
        rm(r)
	  gc()
    unlink(tmp.dir, recursive = TRUE, force = TRUE)
  }
  