# devtools::install_github("ropensci/FedData")
library(rgdal)
library(raster)
library(FedData)
library(geoknife)
library(purrr)

library(spatialEco)
library(sp)

###Fetching TopoWx temperature dailies###
#Gets TopoWx temperature maxima & minima dailies for specified vector of years & specified county's extent box (technically it just needs the FIPS code)
#NOTE: Supplied county feature should have "FIPS" attribute w/ 5-digit FIPS code
fetch_topowx <- function(years, county) {
  all_webdata <- query("webdata")
  airtemp_title <- "TopoWx: Topoclimatic Daily Air Temperature Dataset for the Conterminous United States"
  airtemp_url <- url(all_webdata[airtemp_title])
  
  #Create Geoknife stencil -- i.e. for what area to get the data
  airtemp_stencil <- webgeom(geom = "sample:Counties", attribute = "FIPS", values = as.character(county$FIPS)) #This queries copy of counties shapefile on USGS server
  
  #Create Geoknife knife -- i.e. the parameters for the processing algorithm
  #This pauses R while USGS server runs job
  airtemp_knife <- webprocess(wait = TRUE, sleep.time = 10) 
  airtemp_algorithms <- query(airtemp_knife, "algorithms")
  algorithm(airtemp_knife) <- airtemp_algorithms[grep("OPeNDAP Subset", names(airtemp_algorithms))]
  inputs(airtemp_knife) <- list(OUTPUT_TYPE = "netcdf")
  for (year in years) {
    print(paste0("Now getting ", year, "... current time is: ", Sys.time()))
    begstring <- paste0(year, "-01-01 12:00") #Beginning of time period to query
    endstring <- paste0(year, "-12-31 12:00") #End of time period to query
    
	#Filename to write to disk
    filestring_max <- paste0("topowx_max_", year, ".nc") 
    
    # Create Geoknife fabric -- i.e. what data to get 
	# temperature MAXIMA for specified years)
    airtemp_fabric <- webdata(list(
      url = airtemp_url,
      variables = "tmax",
      times = as.POSIXct(c(begstring, endstring), tz = "UTC")
    ))
    
    #Run job -- if there's an error (server frequently fails to respond), keep 
	# retrying in while loop until no error
    #(this rarely requires more than 2 attempts), then download result; if no 
	# error, proceed straight to download
    airtemp_job <- geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric,
	                        knife = airtemp_knife)

#**************************************************#
#**** need to change destination to be dynamic ****# 
#**************************************************#   
    if (error(airtemp_job) == TRUE) {
      while (error(airtemp_job) == TRUE) {
        airtemp_job <- geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, 
		                        knife = airtemp_knife)
      }
      download(airtemp_job, destination = paste0("E:/Documents/Cropland suitability analysis/Climate/TopoWx/", filestring_max))
    } else {
      download(airtemp_job, destination = paste0("E:/Documents/Cropland suitability analysis/Climate/TopoWx/", filestring_max)) 
    }
    
    filestring_min <- paste0("topowx_min_", year, ".nc")
    
    #Create Geoknife fabric -- i.e. what data to get 
	# (temperature MINIMA for specified years)
    airtemp_fabric <- webdata(list(
      url = airtemp_url,
      variables = "tmin",
      times = as.POSIXct(c(begstring, endstring), tz = "UTC")
    ))

#**************************************************#
#**** need to change destination to be dynamic ****# 
#**************************************************#       
    airtemp_job <- geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
    if (error(airtemp_job) == TRUE) {
      while (error(airtemp_job) == TRUE) {
        airtemp_job <- geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
      }
      download(airtemp_job, destination = paste0("E:/Documents/Cropland suitability analysis/Climate/TopoWx/", filestring_min)) 
    } else {
      download(airtemp_job, destination = paste0("E:/Documents/Cropland suitability analysis/Climate/TopoWx/", filestring_min)) 
    }
  }
}

###Fetching Daymet precipitation dailies###
# Gets Daymet precipitation dailies. Arguments:
#   1) Vector of years for which to get daily data
#   2) Polygon of county (feature should include "CONAME" attribute w/ name of county and "STNAME" attribute with name of state). Names w/o spaces & special characters.
#   3) Number of attempts, in case server fails to respond
#   4) Downtime between attempts (in seconds)
fetch_daymet <- function(years, county, retrynum, sleepnum) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME))) #Filename for .tif written to disk
  countyproj <- spTransform(county, crs("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  get_data <- function() {
    daymet <- get_daymet(template = countyproj,
                         label = labelstring,
                         elements = "prcp",
                         years = years,
                         region = "na",
                         tempo = "day",
                         extraction.dir = getwd(),
                         progress = TRUE)
    return(daymet)
  }
  
  # Function that will attempt data download & return NULL if it fails
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL) 
    result <- NULL
      try_number <- 1
  
  # Attempt data download for set number of times or until function returns a proper result
  while(is.null(result) && try_number <= retrynum) {
    print(paste0("Attempt: ", try_number))
    try_number <- try_number + 1
    result <- get_data_attempt()
    Sys.sleep(sleepnum)
  } 
  if (try_number > retrynum) {
    warning("Could not get data!")
  }  
  return(result)
}

### Degree day calculation (TopoWx) ###
# Calculates degree days for each day of year, sums results through 
# Dec. 31, & averages all Dec. 31 sums across specified list of years
# Pass in a list of paths to all min/max temp files for all years of interest, 
# list of years of interest, & county polygon
degree_days <- function(tempdata, years, county) {
  countyproj <- spTransform(county, crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")) #Reproject county to same CRS as TopoWx data has when downloaded
  i <- 1
  dds_sum_list <- vector("list", length(years))  
  for (year in years) {
    print(paste0("Now calculating degree days for year: ", year, " ... Current time is: ", Sys.time()))
    # Read in all dailies for temp. minima and maxima for given year as raster brick
	topomax <- brick(tempdata[grepl(paste0("_max_", year), tempdata)]) 
    topomin <- brick(tempdata[grepl(paste0("_min_", year), tempdata)])    
    topomax_masked <- raster::mask(topomax, countyproj) #Mask to actual county outline
    topomin_masked <- raster::mask(topomin, countyproj)

    # DDs for base temp. of 5 Celsius = (Tmax + Tmin) / 2 - 5
    dds_eq <- ((topomax_masked + topomin_masked) / 2) - 5 
	# Treat anything w/ a 0 or negative result for DDs as 0, i.e. no degree-day accumulation
    dds_reclass <- reclassify(dds_eq, c(-Inf, 0, 0)) 
    # Add all bands in brick, i.e. days of year, together, to get total degree days 
	# by Dec. 31 for each pixel
	dds_sum <- calc(dds_reclass, sum)
	# Add dds_sum result for given year to list
    dds_sum_list[[i]] <- dds_sum 
    i <- i + 1
    writeRaster(dds_sum, paste0("ddssum_", year, ".tif"), overwrite = TRUE)
  } 
  dds_sum_brick <- brick(dds_sum_list)
  # Average together degree days by Dec. 31 across all years
  dds_avg <- calc(dds_sum_brick, mean) 
  writeRaster(dds_avg, "dds_avg.tif")
}

### Mean annual precipitation calculation (Daymet) ###
# Note: All Daymet years have 365 days, including leap years
# Note: County feature should include "CONAME" attribute w/ name of county & 
# "STNAME" attribute w/ name of state. Names w/o spaces & special characters.
annual_temp <- function(county, years) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME)))
  countyproj <- spTransform(county, crs("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  #Read in downloaded data, which will contain band for each day of year range ... e.g. 
  #  30 years = 10,950 bands
  countydata <- brick(paste0(labelstring, "_prcp_day.tif")) 
  sumvector <- rep(1:length(years), each = 365)
  days_sum <- stackApply(countydata_masked, sumvector, fun = sum) #Sum total precip. for year 1 (days 1-365), year 2 (days 366-730), & so on, through year x
  years_mean <- calc(days_sum, mean) #Mean of total annual precips across full year range
  years_mean_masked <- raster::mask(years_mean, countyproj)
  
  return(years_mean_masked)
}

### Fetching NED data ###
# Gets National Elevation Dataset. Arguments:
#   1) Polygon of county (feature should include "CONAME" attribute w/ name of 
#        county and "STNAME" attribute with name of state). Names w/o spaces & 
#        special characters.
#   2) Number of attempts, in case server fails to respond
#   3) Downtime between attempts (in seconds)
fetch_ned <- function(county, retrynum, sleepnum) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME))) #Filename for .tif written to disk
  countyproj <- spTransform(county, crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  get_data <- function() {
    NED <- get_ned(template = county, label = labelstring) 
    return(NED)
  }
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL)
    result <- NULL
      try_number <- 1  
  while(is.null(result) && try_number <= retrynum) {
    print(paste0("Attempt: ", try_number))
    try_number <- try_number + 1
    result <- get_data_attempt()
    Sys.sleep(sleepnum)
  }
  if (try_number > retrynum) {
    warning("Could not get data!")
  }
  return(result)
}



#### Fetch NASS CDL data####
# Gets NASS Cropland Data Layer 2008-2019. 
# Arguments:
# x          Polygon defining download extent
# yr         Year of download (2008=2019)
# out.dir    Directory will output will be written
# retrynum   Number of times to retry download
# sleepnum   seconds between  tries 
fetch_cdl <- function(x, yr = 2019, out.dir = getwd(), 
                      retrynum = 10, sleepnum = 5) {
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  if(yr < 2008 | yr > 2019)
    stop("Not a valid year 2008-2019")  	
  tryCatch({
  usgs.prj = sf::st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")					    
    if(any(class(x)[1] == c("SpatialPolygonsDataFrame", "SpatialPolygons"))){ 
      if(!sf::st_crs(as(x, "sf")) == usgs.prj)
    	  x <- as(sf::st_transform(as(x, "sf"), usgs.prj),"Spatial")
    } else if(any(class(x)[1] == "sf")) {
      if(!sf::st_crs(x) == usgs.prj)
    	  x <- as(sf::st_transform(x, usgs.prj),"Spatial")  
    }
    if(class(x)[1] == "sf") x <- as(x, "Spatial")
  })
  get_data <- function() {
    NASS <- FedData::get_nass(
      template = x,
      label = "nass",
      year = yr,
      extraction.dir = out.dir,
      force.redo = TRUE,
      progress = TRUE)
    return(NASS)
  }
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL) 
    result <- NULL
      try_number <- 1 
  while(is.null(result) && try_number <= retrynum) {
    cat("Downloading", "NASS-CDL", yr, "attempt:", try_number, "of", retrynum, "\n")
      try_number <- try_number + 1
      result <- get_data_attempt()
    Sys.sleep(sleepnum)
  }
  if (try_number > retrynum) {
    warning("Could not get data!")
  }
  return(result)
}


### Fetching NLCD data ###
# Gets NLCD land cover data. Arguments:
#   1) Polygon of county (feature should include "CONAME" attribute w/ name of county 
#      and "STNAME" attribute with name of state). Names w/o spaces & special characters.
#   2) Year of NLCD data -- valid years are 2001, 2004, 2006, 2008, 2011, & 2016
#   3) Number of attempts, in case server fails to respond
#   4) Downtime between attempts (in seconds)
fetch_nlcd <- function(county, yearnum, retrynum, sleepnum) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME))) 
  get_data <- function() {
    NLCD <- get_nlcd(
      template = county,
      label = labelstring,
      year = yearnum,
      dataset = "Land_Cover",
      landmass = "L48")   
    return(NLCD)
  }
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL) 
    result <- NULL
      try_number <- 1  
  while(is.null(result) && try_number <= retrynum) {
    print(paste0("Attempt: ", try_number))
    try_number <- try_number + 1
    result <- get_data_attempt()
    Sys.sleep(sleepnum)
  }
  if (try_number > retrynum) {
    warning("Could not get data!")
  }
  return(result)
}
