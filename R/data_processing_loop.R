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
# Download and process NASS Crop Data Layer 
# (tillage), check need for model
#************************************************
#************************************************
nass.att <- read.csv(file.path(root, "nass", "NASS_attributes.csv"))
   rc <- data.frame(id = as.numeric(nass.att$value), 
                    v=as.numeric(nass.att$reclass)) 
dir.create(file.path(getwd(), "tmp"), 
           showWarnings = FALSE)
  for(i in 2008:2019){
    tryCatch({
    nass <- fetch_cdl(cbdy, yr = i, 
                      out.dir = file.path(getwd(), "tmp"))
	cat("Reclassifying", "nass", i, "\n")				 
	nass <- raster(file.path(getwd(), "tmp", paste0("cdl_", i, ".tif")))
	  nass <- setValues(raster(nass), nass[])
	    nass <- raster::mask(raster::crop(nass,extent(cbdy)),cbdy)
          nass <- raster::subs(nass, rc)
	writeRaster(nass, file.path(getwd(), "tmp", 
	            paste0("cdl_", i, ".tiff")),
                overwrite=TRUE, options="COMPRESS=LZW" )
    })
  }
nass <- stack(list.files(file.path(getwd(), "tmp"), "tif$", 
              full.names=TRUE))
	writeRaster(nass, file.path(getwd(), "data", "nass.tif"),
                overwrite=TRUE, 
				options="COMPRESS=LZW" )
  crop_freq <- calc(nass, fun=sum, na.rm=TRUE)
    crop_freq <- raster::mask(raster::crop(crop_freq,extent(cbdy)),cbdy)
	writeRaster(crop_freq, file.path(getwd(), "data", "tillage_freq.tif"),
                overwrite=TRUE, 
				options="COMPRESS=LZW")
				
unlink(file.path(getwd(), "tmp"), recursive = TRUE)

#### evaluation clause for next in loop if no tillage 
pct.crop <- length(crop_freq[crop_freq >= 1]) / length(crop_freq[!is.na(crop_freq)]) 
  if(pct.crop < 0.02) { 
    prob <- m
      prob[] <- 0
        prob <- mask(crop(prob, extent(m)),m)	  
  next()
  }
  
#************************************************
#************************************************
# Download and process NLCD impervious surface
#************************************************
#************************************************
nass <- raster(file.path(getwd(), "data", "tillage_freq.tif"))
dir.create(file.path(getwd(), "nlcd"), 
           showWarnings = FALSE)
sucess <- tryCatch({
    nlcd <- fetch_nlcd(cbdy, yr = 2016,
                       data.set = "Impervious",	
                       out.dir = file.path(getwd(), "nlcd"))

	cat("Reprojecting and Reclassifying", "impervious surface", "\n")					   
	  nlcd <- raster(list.files(file.path(getwd(),"nlcd"), "tif$", 
	                 full.names = TRUE))			 
	    nlcd <- projectRaster(nlcd, nass, res=res(nlcd)[1], 
		                      crs, method="ngb")
	      nlcd[is.na(nlcd)] <- 0
	        nlcd[nlcd > 0] <- 1  
              nlcd <- raster::mask(raster::crop(nlcd,extent(cbdy)),cbdy)
	writeRaster(nlcd, file.path(getwd(), "data", "impervious.tif"),
                overwrite=TRUE, 
				options="COMPRESS=LZW" )		  
})
unlink(file.path(getwd(), "nlcd"), recursive = TRUE)
				
#************************************************
#************************************************
# Download and process NLCD Landcover
#************************************************
#************************************************
nlcd.att <- read.csv(file.path(root, "nass", "NLCD_attributes.csv"))
   rc <- data.frame(id = as.numeric(nlcd.att$value), 
                    v=as.numeric(nlcd.att$reclass)) 
nass <- raster(file.path(getwd(), "data", "tillage_freq.tif"))
  dir.create(file.path(getwd(), "nlcd"), 
             showWarnings = FALSE)
sucess <- tryCatch({
    nlcd <- fetch_nlcd(cbdy, yr = 2016,
                       data.set = "Land_Cover",	
                       out.dir = file.path(getwd(), "nlcd"))
	  nlcd <- raster(list.files(file.path(getwd(),"nlcd"), "tif$", 
	                 full.names = TRUE))
	  nlcd[nlcd > 100] <- NA	
	    nlcd <- projectRaster(nlcd, nass, res=res(nlcd)[1], 
		                      crs, method="ngb")					  
	  nlcd <- setValues(raster(nlcd), nlcd[])
	    nlcd <- raster::mask(raster::crop(nlcd,extent(cbdy)),cbdy)
          nlcd <- raster::subs(nlcd, rc)
	writeRaster(nlcd, file.path(getwd(), "data", "water.tif"),
                overwrite=TRUE, 
				options="COMPRESS=LZW" )
})
unlink(file.path(getwd(), "nlcd"), recursive = TRUE)

#************************************************
#************************************************
# Download and process NED elevation and 
# geomorphometric variables
#************************************************
#************************************************
dir.create(file.path(getwd(), "tmp"), 
           showWarnings = FALSE)
sucess <- tryCatch({
  elev <- fetch_ned(cbdy, out.dir = file.path(getwd(),"tmp"))
    elev <- raster(file.path(getwd(),"tmp", "elev_NED_1.tif"))
	  elev <- projectRaster(elev, m, method="bilinear")					  
	    elev <- raster::mask(raster::crop(elev,extent(m)),m)
	writeRaster(elev, file.path(getwd(), "data", "elev.tif"),
                overwrite=TRUE, options="COMPRESS=LZW" )
})
unlink(file.path(getwd(), "tmp"), recursive = TRUE)

#************************************************
#************************************************
# Create mask
#************************************************
#************************************************
water <- raster(file.path(getwd(), "data", "water.tif")) 
nass <- raster(file.path(getwd(), "data", "tillage_freq.tif"))
rds <- raster(file.path(getwd(), "data", "impervious.tif")) 
  rds <- raster::subs(rds, data.frame(id = c(0,1), v=c(1,NA))) 
  
sa.mask <- mask(water, rds)
  writeRaster(sa.mask, file.path(getwd(), "data", "feature_mask.tif"),
              overwrite=TRUE, options="COMPRESS=LZW" )
nass <- mask(nass, sa.mask) 
  writeRaster(nass, file.path(getwd(), "data", "tillage_freq.tif"),
              overwrite=TRUE, options="COMPRESS=LZW")

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


