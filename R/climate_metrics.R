###########################################################################
###########################################################################
# Code block 2 - Derive yearly and seasonal climate metrics
###########################################################################
# Climate metrics: 
#  "prcp.max", "prcp.sum", prcp.days, 
#  "tmin_min", "tmin_med", "tmin_max", "tmin_p05",  
#  "tmin_p95", "tmin_cv", "tmin_sum",    
#  "tmax_min", "tmax_med", "tmax_max", "tmax_p05", 
#  "tmax_p95", "tmax_cv", "tmax_sum", 
#  "tdiff", "ffp", "dd5", "dd30", "snow"
#
#  Expected directory structure: 
#  ".../climate_sim/"                             # root directory
#      ".../climate_sim/results"                  # stores model objects
#      ".../climate_sim/wetlands"                 # stores wetland point sample data used in models
#        ".../climate_sim/SA.../"                 # Directory for study areas
#        ".../climate_sim/SA.../climate/netcdf/"  # stores raw cdf tiles for SA..
#        ".../climate_sim/SA.../climate/data/"    # stores processed years for SA.. (tiff format)
###########################################################################
library(raster)
library(sp)
library(rgdal)
library(spatialEco)

# Set working directory, can set for loop here for processing SA blocks
drive = "F:"
main = paste0(drive, "/climate_sim") 

# Create a directory in C:/tmp and set temp file directory to it
dir.create(paste("C:/tmp", "climate", sep="/"))
  rasterOptions(tmpdir=paste("C:/tmp", "climate", sep="/"))

# Define years, seasions and sample areas
year.seq <- c(1983,1984,1987,1988,1992,1993,1998,1999,2001,2002,2010,2011)
year.splits <- split(year.seq, cut(year.seq, 8, labels = FALSE))
#sample.areas = c("SA1","SA2","SA3","SA4","SA5","SA6") 
sample.areas = c("SA3") 

##################################################
###### Climate metric(s) functions
################################################## 
create.date <- function(year, l = 365) seq(as.Date(paste0(year,"/1/1")), by = "day", length.out = l)

##################################################
# Function to return index of season breaks	
# Index for:
#    until snow-off ("sf"), Nov 1 preceding year - May 30 of focal 
#      c("-01-","-02-","-03-","-04-","-05-")
#    mid-season ("ms"), Nov 1 preceding focal year - July 30 of focal year
#      c("-01-","-02-","-03-","-04-","-05-","-06-", "-07-")
#    until senescence ("ss"), Nov 1 preceding focal year - Sept 30 of focal year 
#      c("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-")
##################################################
sf.seq <- c("-01-","-02-","-03-","-04-","-05-") 
ms.seq <- c("-01-","-02-","-03-","-04-","-05-","-06-", "-07-") 
ss.seq <- c("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-")
idx.fun <- function(dates, years, date.seq) {	
  idx <- vector()
    for(i in years) {
      if(i == years[1] ) {
        idx <- append( idx, 
  	    grep(i, dates)[which(grep(i, dates) %in% grep(paste(c("-11-","-12-"),
  	    collapse="|"), dates))] )	
  	} else {
  	  idx <- append( idx, 
  	    grep(i, dates)[which(grep(i, dates) %in% grep(paste(date.seq,
  	    collapse="|"), dates))] )
  	}
  }
  return( idx )  
}

# FFP, frost free period as number of days > 0C
ffp.fun <- function(x) { length(x[x > 0]) }	 
  
# Snow proportion function
snow.proportion <- function(tmax, prcp, p = 0){
  prcp.df <- as.data.frame(prcp)
  tmax.df <- as.data.frame(tmax)
  if( !ncol(tmax.df) == ncol(prcp.df) ) stop("Columns are not equal")
    if( !nrow(tmax.df) == nrow(prcp.df) ) stop("Rows are not equal")
  prcp.sum <- apply(prcp.df, MARGIN=1, FUN=sum)
    for(i in 1:ncol(tmax.df)) { tmax.df[,i] <- ifelse(tmax.df[,i] > p, 0, 1) }
    for(i in 1:ncol(prcp.df)) { prcp.df[,i] <- prcp.df[,i] * tmax.df[,i] }
      snow <- apply(prcp.df, MARGIN=1, FUN=sum) / prcp.sum
      snow[is.nan(snow)] <- 0
  return( snow )
}            

# Sum of degree days >= 5
degree.days <- function(x, p = 5) {
  x.size <- length(x) / 2
  idx <- which(x[1:x.size] >= p) + x.size
    if( length(idx) > 0 ) {
      return( sum(x[idx]) )
    } else {
      return( 0 )
    }        
  }

# Sum of degree days >=30
degree.days.30 <- function(x, p = 30) { sum(x[x>= p], na.rm=TRUE ) }

# Prcp days  
prcp.days <- function(x, p=0.01) { 
	length(x[x > p]) / length(x) 
	}
	
na.max <- function(x) {
  options(warn = -1)
  if(!is.infinite( max(x, na.rm = TRUE) ) ) {
    return( max(x, na.rm = TRUE) )
  } else {
    return( NA )
  }
}
	
na.min <- function(x) {
  options(warn = -1)
  if(!is.infinite( min(x, na.rm = TRUE) ) ) {
    return( max(x, na.rm = TRUE) )
  } else {
    return( NA )
  }
}	   

# data.frame class methods for is.nan and is.infinite generics
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan)) 
is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite)) 

######################################################################  
# Start sample area and time-period for loop
###################################################################### 
for( d in sample.areas) {
  root <- paste(main, d, sep="/") 
  croot <- paste(root, "climate", sep="/")
  setwd(root)
  
  climate.rasters <- list.files(paste0(croot,"/data"), "tif$", full.names = TRUE)
    prcp <- grep("ppt", climate.rasters, value = TRUE) 
	tmin <- grep("tmin", climate.rasters, value = TRUE) 
	tmax <- grep("tmax", climate.rasters, value = TRUE) 
    	
  # for loop for calculating year splits 
  for(y in 1:length(year.splits)) { 
    years <- year.splits[[y]]
	dates <- c(create.date(years[1]),create.date(years[2])) 
	idx.list <- list(sf.idx = idx.fun(dates, years, sf.seq), 
                     ms.idx = idx.fun(dates, years, ms.seq), 
                     ss.idx = idx.fun(dates, years, ss.seq)) 					   
    prcp.stack <- stack(prcp[grep(paste(c(years[1],years[2]), collapse="|"), prcp)])
    tmin.stack <- stack(tmin[grep(paste(c(years[1],years[2]), collapse="|"), tmin)])
    tmax.stack <- stack(tmax[grep(paste(c(years[1],years[2]), collapse="|"), tmax)])
 
	# for loop for calculating time-period based climate metrics 
    for(s in names(idx.list) ) { 
      cat("Processing:", d , unlist(strsplit(s, "[.]"))[1], years[2], "\n")
	  climate.metrics <- data.frame(ID=1:ncell(tmin.stack[[1]]))
            
      # prcp metrics
      prcp.sub <- prcp.stack[[idx.list[[s]]]]
       prcp.sub <- as.data.frame(getValues(prcp.sub))
	    climate.metrics <- data.frame(climate.metrics, prcp.max = apply(prcp.sub, MARGIN = 1, 
	                                  FUN = na.max))
	    climate.metrics <- data.frame(climate.metrics, prcp.sum = apply(prcp.sub, MARGIN = 1, 
	                                  FUN = sum, na.rm=TRUE))
		climate.metrics <- data.frame(climate.metrics, prcp.days = apply(prcp.sub, MARGIN = 1,
                              FUN = function(x, p=0.01) { length(x[x > p]) / length(x) } )) 

      # tmin metrics	
      tmin.sub <- tmin.stack[[idx.list[[s]]]]
        tmin.sub <- as.data.frame(getValues(tmin.sub))
	    climate.metrics <- data.frame(climate.metrics, tmin.min = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = na.min))
	    climate.metrics <- data.frame(climate.metrics, tmin.med = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = median, na.rm=TRUE))
	    climate.metrics <- data.frame(climate.metrics, tmin.max = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = na.max))
	    climate.metrics <- data.frame(climate.metrics, tmin.05 = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = quantile, p=0.05, na.rm = TRUE))
	    climate.metrics <- data.frame(climate.metrics, tmin.95 = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = quantile, p=0.95, na.rm = TRUE))								
        climate.metrics <- data.frame(climate.metrics, tmin.cv = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = cv, na.rm=TRUE))
	    climate.metrics <- data.frame(climate.metrics, tmin.sum = apply(tmin.sub, MARGIN = 1, 
	                                  FUN = sum, na.rm=TRUE))
      
	  # tmax metrics		
      tmax.sub <- tmax.stack[[idx.list[[s]]]]
       tmax.sub <- as.data.frame(getValues(tmax.sub))
        climate.metrics <- data.frame(climate.metrics, tmax.min = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = na.min))
	    climate.metrics <- data.frame(climate.metrics, tmax.med = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = median, na.rm=TRUE))
	    climate.metrics <- data.frame(climate.metrics, tmax.max = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = na.max))
	    climate.metrics <- data.frame(climate.metrics, tmax.05 = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = quantile, p=0.05, na.rm = TRUE))
	    climate.metrics <- data.frame(climate.metrics, tmax.95 = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = quantile, p=0.95, na.rm = TRUE))				
        climate.metrics <- data.frame(climate.metrics, tmax.cv = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = cv, na.rm=TRUE))
	    climate.metrics <- data.frame(climate.metrics, tmax.sum = apply(tmax.sub, MARGIN = 1, 
	                                  FUN = sum, na.rm=TRUE))
	
    # TDIFF, FFP, SNOW, dd5, dd30
        climate.metrics <- data.frame(climate.metrics, tdiff = apply( (tmax.sub - tmin.sub), 
		                              MARGIN=1, FUN=mean, na.rm=TRUE))
        climate.metrics <- data.frame(climate.metrics, ffp = apply(tmax.sub, MARGIN = 1, 
									  FUN = ffp.fun))									  
	    climate.metrics <- data.frame(climate.metrics, dd5 = apply( cbind(tmin.sub, tmax.sub), 
		                              MARGIN=1, FUN=degree.days))									  
	    climate.metrics <- data.frame(climate.metrics, dd30 = apply(tmax.sub, 
		                              MARGIN=1, FUN=degree.days.30))
        climate.metrics <- data.frame(climate.metrics, snow = snow.proportion(tmax.sub, prcp.sub))
	
    # Coerce SpatialPixelsDataFrame to raster stack and export to tiff
    climate.metrics[is.nan(climate.metrics)] <- NA
      daymet <- tmin.stack[[1]]
      daymet[is.na(daymet)] <- -9999	  
	  daymet <- as(daymet, "SpatialPixelsDataFrame")
	    daymet@data <- data.frame(daymet@data, climate.metrics[,-1])
	      daymet@data <- daymet@data[,-1] 
	  rname <- paste(unlist(strsplit(s, "[.]"))[1], years[2], sep="_" )
	daymet@data[daymet@data == -9999] <- NA
	daymet <- stack(daymet)
	
	#### apply bilinear resampling to 1000m and write results	
	e <- raster(extent(tmin.stack[[1]]), res=c(1000,1000))	
	daymet <- resample(daymet, e, method="bilinear")				 
	  writeRaster(daymet, paste(croot, paste0(rname, ".tif"),sep="/"), 
                  overwrite=TRUE, options="COMPRESS=LZW")
      rm(list=c("climate.metrics", "tmax.sub", "tmin.sub", "prcp.sub", "daymet"))
        gc()	
    } 
  }   
} # end sample area loop
unlink(paste("C:/tmp", "climate", sep="/"))


# #### Calculate degree days (TopoWx) ####
# # Calculates degree days for each day of year, sums results through Dec. 31, & averages all Dec. 31 sums across specified list of years
# #Pass in a list of paths to all min/max temp files for all years of interest, list of years of interest, & county polygon
# # Arguments:
# # x                    Polygon defining download extent
# # yrs                  Years of interest
# degree_days <- function(tempdata, years, county) {
#   countyproj <- sp::spTransform(county, crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")) #Reproject county to same CRS as TopoWx data has when downloaded
#   i <- 1
#   dds_sum_list <- vector("list", length(years)) #Empty list to contain rasters of degree days by Dec. 31 for each year (to be averaged later)
#   
#   for (year in years) {
#     print(paste0("Now calculating degree days for year: ", year, " ... Current time is: ", Sys.time()))
#     topomax <- raster::brick(tempdata[grepl(paste0("_max_", year), tempdata)]) #Read in all dailies for temp. maxima for given year as raster brick
#     topomin <- raster::brick(tempdata[grepl(paste0("_min_", year), tempdata)]) #Read in all dailies for temp. minima for given year as raster brick
#     
#     topomax_masked <- raster::mask(raster::crop(topomax, raster::extent(countyproj)), countyproj) #Mask to actual county outline
#     topomin_masked <- raster::mask(raster::crop(topomin, raster::extent(countyproj)), countyproj)
#     
#     dds_eq <- ((topomax_masked + topomin_masked) / 2) - 5 #DDs for base temp. of 5 Celsius = (Tmax + Tmin) / 2 - 5
#     dds_reclass <- reclassify(dds_eq, c(-Inf, 0, 0)) #Treat anything w/ a 0 or negative result for DDs as 0, i.e. no degree-day accumulation
#     dds_sum <- raster::calc(dds_reclass, sum) #Add all bands in brick, i.e. days of year, together, to get total degree days by Dec. 31 for each pixel
#     dds_sum_list[[i]] <- dds_sum #Add dds_sum result for given year to list
#     i <- i + 1
#     
#     raster::writeRaster(dds_sum, paste0("ddssum_", year, ".tif"), overwrite = TRUE, options = "COMPRESS=LZW")
#   }
#   
#   dds_sum_brick <- raster::brick(dds_sum_list)
#   dds_avg <- raster::calc(dds_sum_brick, mean) #Average together degree days by Dec. 31 across all years
#   
#   common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#   dds_avg_proj <- raster::projectRaster(dds_avg, crs = common_crs, res = 800, method = "bilinear") #Set output to common projection
#   raster::writeRaster(dds_avg_proj, "dds_avg.tif", overwrite = TRUE, options = "COMPRESS=LZW")
# }
# 
# 
# #### Calculate mean annual precipitation (Daymet) ####
# # Note: All Daymet years have 365 days, including leap years
# # Arguments:
# # x                    Polygon defining county of interest (should have "FIPS" attribute)
# # yrs                  Vector of years of interest
# # indata               Should be the written-to-disk result of fetch_daymet(), which is a RasterBrick of Daymet precip dailies over the years of interest
# #"indata" argument should be the returned result of fetch_daymet, which is a RasterBrick of Daymet precip dailies over the years of interest
# annual_precip <- function(x, yrs, indata) {
#   if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
#     stop("x must be an sp or sf polygon object")
#   
#   if (!class(indata)[1] == "RasterBrick")
#     stop("indata must be a RasterBrick object")
#   
#   if (length(yrs) != raster::nbands(indata) / 365)
#     stop("Length of your years vector does not seem to match the number of available years in your Daymet download")
#   
#   tryCatch({
#     usgs.prj = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")					    
#     if(any(class(x)[1] == c("SpatialPolygonsDataFrame", "SpatialPolygons"))){ 
#       if(!sf::st_crs(as(x, "sf")) == usgs.prj)
#         x <- as(sf::st_transform(as(x, "sf"), usgs.prj),"Spatial")
#     } else if(any(class(x)[1] == "sf")) {
#       if(!sf::st_crs(x) == usgs.prj)
#         x <- as(sf::st_transform(x, usgs.prj),"Spatial")  
#     }
#     if(class(x)[1] == "sf") x <- as(x, "Spatial")
#   })
#   
#   sumvector <- rep(1:length(years), each = 365)
#   
#   days_sum <- raster::stackApply(indata, sumvector, fun = sum) #Sum total precip. for year 1 (days 1-365), year 2 (days 366-730), & so on, through year x
#   years_mean <- raster::calc(days_sum, mean) #Mean of total annual precips across full year range
#   years_mean_masked <- raster::mask(raster::crop(years_mean, raster::extent(x)), x)
#   
#   common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#   
#   if (!sp::identicalCRS(years_mean_masked, x))
#     years_mean_masked_proj <- raster::projectRaster(years_mean_masked, crs = common_crs, res = 1000, method = "bilinear") #Set output to common projection
#   
#   raster::writeRaster(years_mean_masked_proj, paste0("mean_annual_precip_", x$FIPS, ".tif"), overwrite = TRUE, options = "COMPRESS=LZW")
# }
