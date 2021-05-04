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
