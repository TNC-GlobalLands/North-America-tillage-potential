devtools::install_github("ropensci/FedData")
library(rgdal)
library(raster)
library(FedData)
library(geoknife)
library(purrr)

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
  airtemp_knife <- webprocess(wait = TRUE, sleep.time = 10) #This pauses R while USGS server runs job
  airtemp_algorithms <- query(airtemp_knife, "algorithms")
  algorithm(airtemp_knife) <- airtemp_algorithms[grep("OPeNDAP Subset", names(airtemp_algorithms))]
  inputs(airtemp_knife) <- list(OUTPUT_TYPE = "netcdf")
  
  for (year in years) {
    print(paste0("Now getting ", year, "... current time is: ", Sys.time()))
    
    begstring <- paste0(year, "-01-01 12:00") #Beginning of time period to query
    endstring <- paste0(year, "-12-31 12:00") #End of time period to query
    
    filestring_max <- paste0("topowx_max_", year, ".nc") #Filename to write to disk
    
    #Create Geoknife fabric -- i.e. what data to get (temperature MAXIMA for specified years)
    airtemp_fabric <- webdata(list(
      url = airtemp_url,
      variables = "tmax",
      times = as.POSIXct(c(begstring, endstring), tz = "UTC")
    ))
    
    #Run job -- if there's an error (server frequently fails to respond), keep retrying in while loop until no error
    #(this rarely requires more than 2 attempts), then download result; if no error, proceed straight to download
    airtemp_job <- geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
    
    if (error(airtemp_job) == TRUE) {
      while (error(airtemp_job) == TRUE) {
        airtemp_job <- geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
      }
      download(airtemp_job, destination = paste0("E:/Documents/Cropland suitability analysis/Climate/TopoWx/", filestring_max))
    } else {
      download(airtemp_job, destination = paste0("E:/Documents/Cropland suitability analysis/Climate/TopoWx/", filestring_max)) 
    }
    
    filestring_min <- paste0("topowx_min_", year, ".nc")
    
    #Create Geoknife fabric -- i.e. what data to get (temperature MINIMA for specified years)
    airtemp_fabric <- webdata(list(
      url = airtemp_url,
      variables = "tmin",
      times = as.POSIXct(c(begstring, endstring), tz = "UTC")
    ))
    
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
#Gets Daymet precipitation dailies. Arguments:
#1). Vector of years for which to get daily data
#2). Polygon of county (feature should include "CONAME" attribute w/ name of county and "STNAME" attribute with name of state). Names w/o spaces & special characters.
#3). Number of attempts, in case server fails to respond
#4). Downtime between attempts (in seconds)
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
  
  #Function that will attempt data download & return NULL if it fails
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL)
  
  result <- NULL
  try_number <- 1
  
  #Attempt data download for set number of times or until function returns a proper result
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

###Degree day calculation (TopoWx)###
#Calculates degree days for each day of year, sums results through Dec. 31, & averages all Dec. 31 sums across specified list of years
#Pass in a list of paths to all min/max temp files for all years of interest, list of years of interest, & county polygon
degree_days <- function(tempdata, years, county) {
  countyproj <- spTransform(county, crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")) #Reproject county to same CRS as TopoWx data has when downloaded
  i <- 1
  dds_sum_list <- vector("list", length(years)) #Empty list to contain rasters of degree days by Dec. 31 for each year (to be averaged later)
  
  for (year in years) {
    print(paste0("Now calculating degree days for year: ", year, " ... Current time is: ", Sys.time()))
    topomax <- brick(tempdata[grepl(paste0("_max_", year), tempdata)]) #Read in all dailies for temp. maxima for given year as raster brick
    topomin <- brick(tempdata[grepl(paste0("_min_", year), tempdata)]) #Read in all dailies for temp. minima for given year as raster brick
    
    topomax_masked <- raster::mask(topomax, countyproj) #Mask to actual county outline
    topomin_masked <- raster::mask(topomin, countyproj)
    
    dds_eq <- ((topomax_masked + topomin_masked) / 2) - 5 #DDs for base temp. of 5 Celsius = (Tmax + Tmin) / 2 - 5
    dds_reclass <- reclassify(dds_eq, c(-Inf, 0, 0)) #Treat anything w/ a 0 or negative result for DDs as 0, i.e. no degree-day accumulation
    dds_sum <- calc(dds_reclass, sum) #Add all bands in brick, i.e. days of year, together, to get total degree days by Dec. 31 for each pixel
    dds_sum_list[[i]] <- dds_sum #Add dds_sum result for given year to list
    i <- i + 1
    
    writeRaster(dds_sum, paste0("ddssum_", year, ".tif"), overwrite = TRUE)
  }
  
  dds_sum_brick <- brick(dds_sum_list)
  dds_avg <- calc(dds_sum_brick, mean) #Average together degree days by Dec. 31 across all years
  writeRaster(dds_avg, "dds_avg.tif")
}

###Mean annual precipitation calculation (Daymet)###
#Note: All Daymet years have 365 days, including leap years
#NOte: County feature should include "CONAME" attribute w/ name of county & "STNAME" attribute w/ name of state. Names w/o spaces & special characters.
annual_temp <- function(county, years) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME)))
  countyproj <- spTransform(county, crs("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  
  countydata <- brick(paste0(labelstring, "_prcp_day.tif")) #Read in downloaded data, which will contain band for each day of year range ... e.g. 30 years = 10,950 bands
  sumvector <- rep(1:length(years), each = 365)
  
  days_sum <- stackApply(countydata_masked, sumvector, fun = sum) #Sum total precip. for year 1 (days 1-365), year 2 (days 366-730), & so on, through year x
  years_mean <- calc(days_sum, mean) #Mean of total annual precips across full year range
  years_mean_masked <- raster::mask(years_mean, countyproj)
  
  return(years_mean_masked)
}

###Fetching NED data###
#Gets National Elevation Dataset. Arguments:
#1). Polygon of county (feature should include "CONAME" attribute w/ name of county and "STNAME" attribute with name of state). Names w/o spaces & special characters.
#2). Number of attempts, in case server fails to respond
#3). Downtime between attempts (in seconds)
fetch_ned <- function(county, retrynum, sleepnum) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME))) #Filename for .tif written to disk
  countyproj <- spTransform(county, crs("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  
  get_data <- function() {
    NED <- get_ned(template = county, label = labelstring)
    
    return(NED)
  }
  
  #Function that will attempt data download & return NULL if it fails
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL)
  
  result <- NULL
  try_number <- 1
  
  #Attempt data download for set number of times or until function returns a proper result
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

###Fetching NASS CDL data###
#Gets NASS Cropland Data Layer. Arguments:
#1). Polygon of county (feature should include "CONAME" attribute w/ name of county and "STNAME" attribute with name of state). Names w/o spaces & special characters.
#2). Number of attempts, in case server fails to respond
#3). Downtime between attempts (in seconds)
fetch_cdl <- function(county, retrynum, sleepnum) {
  labelstring <- paste0(gsub(" ", "", as.character(county$CONAME)), "_Co_", gsub(" ", "", as.character(county$STNAME))) #Filename for .tif written to disk
  
  get_data <- function() {
    NED <- get_nass(
      template = county,
      label = labelstring,
      year = 2015,
      extraction.dir = getwd(),
      force.redo = TRUE,
      progress = TRUE)
    
    return(NED)
  }
  
  #Function that will attempt data download & return NULL if it fails
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL)
  
  result <- NULL
  try_number <- 1
  
  #Attempt data download for set number of times or until function returns a proper result
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