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

nsatgo <- rast(file.path(root,"model","NATSGO_UNITS.tif"))

min.sample = 1
max.sample = 1000
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
    if(file.exists(file.path(ct, "sample.shp"))) {
      next
    }	  
    setwd(ct)
	cat("\n", "Creating sample for", ct, "\n") 
    m <- rast("mask.tif")
      soil <- project(crop(nsatgo, m), m, method="near", mask=TRUE)
        soil <- mask(soil,m)
    ids <- as.numeric(unique(soil))		
    pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,
                               max = length(ids), width = 300)
	samples <- list()					
      for(i in 1:length(ids)) {
	    Sys.sleep(0.1)		
        xy <- xyFromCell(soil, which(soil[] == ids[i]))
          if(nrow(xy) < 100) {
            next
          }	 		
	      n = round( (nrow(xy) * 30^2 / 4046.86 * pct), 0)
            if(n < min.sample) n = min.sample
            if(n > max.sample) n = max.sample
		xy <- as.data.frame(matrix(xy[sample(1:nrow(xy), n),],ncol=2))
		  xy <- data.frame(ids[i], xy)
		    names(xy) <- c("ID","x","y")
		samples[[i]] <- xy 
		tcltk::setTkProgressBar(pb, i, label=paste(round(i/length(ids)*100, 0), "% done"))
	  }	
	  close(pb)
        samples <- do.call("rbind", samples)
	      coordinates(samples) <- ~x+y
	      samples <- as(samples,"sf")
	    suppressWarnings({sf::st_crs(samples) <- usgs.prj}) 
      st_write(samples, "sample.shp", append=FALSE)
  } # end county loop
} # end state loop  


## alternate approach
# library(raster)
# r <- rast(nrows=100, ncols=100)
# r[] <- sample(c(1,3,5,8), ncell(r), replace=TRUE)
# ids = unique(r[])
# n = c(3,5,2)
# 
# sampleNfromC = function(r,N,C){
#   d <- subset(data.frame(sampleStratified(r == C, N)),layer == 1)
#     d$layer <- C
#   return(d)
# }
# strat = do.call(rbind, lapply(1:length(cats), function(i){sampleNfromC(r,n[i],ids[i])}))
