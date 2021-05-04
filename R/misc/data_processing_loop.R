library(sp)
library(raster)
library(sf)

root = "C:/evans/tillage"
usgs.prj = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

#################################################
#################################################
# Call package management 
source(file.path(root, "code", "AddPackages.R")

#################################################
#################################################
# Start loop 
states = list.dirs(path = file.path(root,"model"),
                   full.names = TRUE, 
                   recursive = FALSE)

#### Start of state(st) - county(ct) model loop
for(st in states) {
  state = basename(st)
  counties = list.dirs(path = st, full.names = TRUE, 
                       recursive = FALSE) 
  for(ct in counties) {
    setwd(ct)
	dir.create(file.path(getwd(), "data"),
               showWarnings = FALSE)
    dir.create(file.path(getwd(), "tmp"), 
               showWarnings = FALSE)		   
	  cbdy <- sf::st_read(getwd(), "bdy")
	  m <- raster("mask.tif")

       }				   
	#...run data prep and model here
	# check NASS for any tillage in county, if no
	#   create raster of zeros and apply mask
	# check SSURGO for polygons
	# start processing raster data 
	
  } # end county loop
} # end state loop  

#################################################
#################################################
# Data acquisition and processing 
#   (insert into ct for loop)
#################################################
#################################################


#************************************************
#************************************************
# Download and process NED elevation and 
# geomorphometric variables
#************************************************
#************************************************

sucess <- tryCatch({
  elev <- fetch_ned(as(cbdy,"Spatial"), out.dir = file.path(getwd(),"tmp"))
    elev <- raster(file.path(getwd(),"tmp", "elev_NED_13.tif"))
	  elev <- projectRaster(elev, m, method="bilinear")	
	    elev <- raster::mask(raster::crop(elev,extent(m)),m)
	writeRaster(elev, file.path(getwd(), "data", "elev.tif"),
                overwrite=TRUE, options="COMPRESS=LZW" )
})

unlink(file.path(getwd(), "tmp"), recursive = TRUE)


#************************************************
#************************************************
# process climate data
#************************************************
#************************************************
dir.create(file.path(getwd(), "climate"), 
           showWarnings = FALSE)




#************************************************
#************************************************
# SSURGO and sample
#************************************************
#************************************************
dir.create(file.path(getwd(), "tmp"), 
           showWarnings = FALSE)
# Download SSURGO polygons and create point sample
ssurgo <- get_ssurgo(
  template=cbdy,
  label="soil",
  raw.dir = file.path(getwd(), "tmp"), 
  extraction.dir = file.path(getwd(), "tmp"),
  force.redo = TRUE)

ssurgo <- merge(ssurgo$spatial, ssurgo$tabular$mapunit,
                by.x = "MUKEY", by.y="mukey")  
  ssurgo <- sp::spTransform(ssurgo, proj4string(county))
    writeOGR(ssurgo, file.path(getwd(),"data"), "ssurgo", 
	         driver="ESRI Shapefile",
             check_exists=TRUE, 
			 overwrite_layer=TRUE)
unlink(file.path(getwd(), "tmp"), recursive = TRUE)

# proportional-area random sample
train <- parea.sample(ssurgo, pct = 0.1, stype = "random")
  writeOGR(train, file.path(getwd(),"data"), "train", 
           driver="ESRI Shapefile",
           check_exists=TRUE, 
		   overwrite_layer=TRUE)


