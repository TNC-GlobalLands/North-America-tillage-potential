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
#2). Polygon of county (feature should include "STATE_NAME" attribute with name of state)
#3). Number of attempts, in case server fails to respond
#4). Downtime between attempts (in seconds)
fetch_daymet <- function(years, county, retrynum, sleepnum) {
  labelstring <- paste0(gsub(" ", "", as.character(county$NAME)), "_Co_", gsub(" ", "", as.character(county$STATE_NAME))) #Filename for .tif written to disk
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