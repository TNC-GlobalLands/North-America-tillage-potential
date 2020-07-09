root = "C:/evans/tillage"
usgs.prj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

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
	dir.create(file.path(getwd(), "data"),h
           showWarnings = FALSE)
	  cbdy <- rgdal::readOGR(getwd(), "bdy")
	    cbdy <- spTransform(cbdy, usgs.prj)
		
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
#################################################
#################################################

#************************************************
# Download and process NASS tillage
nass.att <- read.csv(file.path(root, "nass", "NASS_attributes.csv"))
   rc <- data.frame(id = as.numeric(nass.att$value), 
                    v=as.numeric(nass.att$reclass)) 
dir.create(file.path(getwd(), "nass"), 
           showWarnings = FALSE)
  for(i in 2008:2019){
    nass <- fetch_cdl(cbdy, 
	                 lab = "nass", yr = i, 
                     out.dir = file.path(getwd(), "nass"))
	cat("Reclassifying", "nass", i, "\n")				 
	nass <- raster(file.path(getwd(), "nass", paste0("cdl_", i, ".tif")))
	  nass <- mask(crop(nass,extent(cbdy)),cbdy)
        nass <- subs(nass, rc)
	writeRaster(nass, file.path(getwd(), "nass", 
	            paste0("cdl_", i, ".tiff")),
                overwrite=TRUE, options="COMPRESS=LZW" )
  }
nass <- stack(list.files(file.path(getwd(), "nass"), "tif$", 
              full.names=TRUE))
	writeRaster(nass, file.path(getwd(), "data", "nass.tif"),
                overwrite=TRUE, 
				options="COMPRESS=LZW" )
unlink(file.path(getwd(), "nass"), recursive = TRUE)

calc(nass, sum) 

#************************************************
# SSURGO and sample
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
