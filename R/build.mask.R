#################################################
#################################################
# Build county-levels mask based on impervious surface
# and landcover
#################################################
#################################################

root = "C:/evans/tillage"

# Call package management 
library(sp)
library(terra)
library(raster)
library(sf)
library(FedData)
source(file.path(root, "code", "fetch_nlcd.R"))

usgs.prj = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

rc <- as.matrix(read.csv(file.path(root, "nass", 
                "NLCD_attributes.csv"))[,c(3,4)])

#################################################
# Start loop 
states = sort(list.dirs(path = file.path(root,"model"),
              full.names = TRUE, recursive = FALSE))

#### Start of state(st) - county(ct) model loop
for(st in states) {
  state = basename(st)
  counties = sort(list.dirs(path = st, full.names = TRUE, 
                  recursive = FALSE)) 
  for(ct in counties) {
    if(file.exists(file.path(ct, "cmask.tif"))) {
      next
    }	  
    setwd(ct)
	cat("Creating mask for", ct, "\n")
    dir.create(file.path(getwd(), "tmp"), 
               showWarnings = FALSE)
    tmp.dir <- file.path(getwd(), "tmp")			   	
	cbdy <- sf::st_read(getwd(), "bdy")
	  sf::st_crs(cbdy) <- sf::st_crs(usgs.prj)
        m <- rast("mask.tif")

    #************************************************
    # Download and process NLCD and impervious surface
    #************************************************
	suppressWarnings({
      fetch_nlcd(cbdy, yr = 2016,
                 data.set = "Impervious",	
                 out.dir = tmp.dir)
      cat("Reprojecting and Reclassifying", "impervious surface", "\n")
      imperv <- terra::rast(list.files(tmp.dir, "tif$", full.names = TRUE))
	    imperv[is.na(imperv)] <- 0
	      imperv[imperv > 0] <- NA
            imperv[imperv < 1] <- 1     						 
      imperv <- terra::project(imperv, m, method="near", mask=TRUE)			 
	    imperv <- terra::mask(imperv, m)
      unlink(list.files(tmp.dir, full.names = TRUE))	  
	  cat("Reprojecting and Reclassifying", "landcover", "\n")
      fetch_nlcd(cbdy, yr = 2016,
                 data.set = "Land_Cover",	
                 out.dir = tmp.dir)				   
      nlcd <- terra::rast(list.files(tmp.dir, "tif$", full.names = TRUE))
	    nlcd <- terra::classify(x=nlcd, rcl=rc)				   
      nlcd <- terra::project(nlcd, m, method="near", mask=TRUE)
	    nlcd <- terra::mask(nlcd, m)
      unlink(list.files(tmp.dir, full.names = TRUE))	 	  
	  r <- mask(nlcd, imperv)
    })
    if(!exists("r")) {
	  write(ct, file="C:/evans/tillage/mask_fail.txt", append=TRUE)
	} else {  
	  writeRaster(r, "cmask.tif",overwrite=TRUE, 
    	  		  wopt=list(gdal=c("COMPRESS=LZW")) )
	}			  
    remove(imperv, nlcd, r, m, cbdy, tmp.dir)
      gc()	
  unlink(file.path(getwd(), "tmp"), recursive = TRUE)
  } # end county loop
} # end state loop  
