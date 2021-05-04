library(raster)
library(rgdal)
library(sp)

setwd("C:/evans/tillage/climate")
  data.dir <- file.path(getwd(), "raw")

geo.prj = "+proj=longlat +datum=WGS84 +no_defs"  
usgs.prj = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

e <- extent(-127.8868, -65.34596, 22.87203, 48.24511)
m <- raster("mask.tif")
  
zfiles <- list.files(data.dir, ".zip$", full.names = TRUE)
  for(i in zfiles) {
    rname <- spatialEco::rm.ext(basename(i))
    unzip(i)
    file.rename(paste0(rname, ".txt"),paste0(rname, ".asc")) 
    r <- raster(paste0(rname, ".asc"))
	  r <- crop(r, e)
        proj4string(r) <- geo.prj
	  r <- projectRaster(r, m, method="bilinear")
    writeRaster(r, paste0(rname, ".tif"), 
                overwrite=TRUE, 
				options="COMPRESS=LZW")
    unlink(paste0(rname, ".asc"))
  }  
