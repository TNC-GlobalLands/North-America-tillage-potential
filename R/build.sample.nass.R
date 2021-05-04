#################################################
#################################################
# Create point sample for SSURGO soil types 
#################################################
#################################################
root = "C:/evans/tillage"
 
library(sp)
library(raster)
library(sf)
library(spatialEco)
library(terra)

usgs.prj = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

min.sample = 1
pct = 0.05

#*********************************************
# Start loop for sampling soil data
#*********************************************	 
states = sort(list.dirs(path = file.path(root,"model"),
              full.names = TRUE, recursive = FALSE))
#### Start of state(st) and county(ct) model loops
for(st in states) {
  counties = sort(list.dirs(path = st, full.names = TRUE, 
                  recursive = FALSE)) 
  for(ct in counties) {
    nass <- rast(file.path(st,"nass.tif")) 
    setwd(ct)
	cat("\n", "Creating sample for", ct, "\n") 
    m <- rast("mask.tif")
	  soil <- crop(nass, ext(m))
        soil <- project(soil, m, method="near")
          soil <- mask(soil,m)
		    soil[soil > 1] <- 1
    ids <- as.numeric(unique(soil))
	samples <- list()
	j=0
      for(i in rev(ids)) {
	    j=j+1
        xy <- xyFromCell(soil, which(soil[] == i))
          if(nrow(xy) < 100) {
            next
          }	 		
	      n = round( (nrow(xy) * 30^2 / 4046.86 * pct), 0)
            if(n < min.sample) n = min.sample
            if(i == 0) n = dim(samples[[1]])*2 
		xy <- as.data.frame(matrix(xy[sample(1:nrow(xy), n),],ncol=2))
		  xy <- data.frame(i, xy)
		    names(xy) <- c("ID","x","y")
		samples[[j]] <- xy 
	  }	
        samples <- do.call("rbind", samples)
	      coordinates(samples) <- ~x+y
	        samples <- as(samples,"sf")
	      suppressWarnings({sf::st_crs(samples) <- usgs.prj}) 
        st_write(samples, "sample.shp", append=FALSE, overwrite=TRUE)
	  remove(samples, xy, m, soil, nass)
	gc()
  } # end county loop
} # end state loop  
