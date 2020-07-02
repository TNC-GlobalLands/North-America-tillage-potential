#### Fetch TopoWx temperature dailies ####
# Gets TopoWx temperature maxima & minima dailies for specified vector of years & specified county's extent box (technically it just needs the FIPS code)
# NOTE: Supplied county feature should have "FIPS" attribute w/ 5-digit FIPS code
# Arguments:
# FIPS                    FIPS code of county of interest
# yrs                     Vector of years for which to download (valid range: 1948-2016)
fetch_topowx <- function(FIPS, yrs) {
  all_webdata <- geoknife::query("webdata")
  airtemp_title <- "TopoWx: Topoclimatic Daily Air Temperature Dataset for the Conterminous United States"
  airtemp_url <- geoknife::url(all_webdata[airtemp_title])
  
  #Create Geoknife stencil -- i.e. for what area to get the data
  airtemp_stencil <- geoknife::webgeom(geom = "sample:Counties", attribute = "FIPS", values = FIPS) #This queries copy of counties shapefile on USGS server
  
  #Create Geoknife knife -- i.e. the parameters for the processing algorithm
  airtemp_knife <- geoknife::webprocess(wait = TRUE, sleep.time = 10) #This pauses R while USGS server runs job
  airtemp_algorithms <- geoknife::query(airtemp_knife, "algorithms")
  geoknife::algorithm(airtemp_knife) <- airtemp_algorithms[grep("OPeNDAP Subset", names(airtemp_algorithms))]
  geoknife::inputs(airtemp_knife) <- list(OUTPUT_TYPE = "netcdf")
  
  for (yr in yrs) {
    if(yr < 1948 | yr > 2016)
      stop("Not a valid year -- needs to be 1948-2016")
    
    print(paste0("Now getting ", yr, "... current time is: ", Sys.time()))
    
    begstring <- paste0(yr, "-01-01 12:00") #Beginning of time period to query
    endstring <- paste0(yr, "-12-31 12:00") #End of time period to query
    
    filestring_max <- paste0("topowx_max_", yr, ".nc") #Filename to write to disk
    
    #Create Geoknife fabric -- i.e. what data to get (temperature MAXIMA for specified years)
    airtemp_fabric <- geoknife::webdata(list(
      url = airtemp_url,
      variables = "tmax",
      times = as.POSIXct(c(begstring, endstring), tz = "UTC")
    ))
    
    #Run job -- if there's an error (server frequently fails to respond), keep retrying in while loop until no error
    #(this rarely requires more than 2 attempts), then download result; if no error, proceed straight to download
    airtemp_job <- geoknife::geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
    
    if (geoknife::error(airtemp_job) == TRUE) {
      while (geoknife::error(airtemp_job) == TRUE) {
        airtemp_job <- geoknife::geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
      }
      geoknife::download(airtemp_job, destination = paste0(getwd(), "/", filestring_max))
    } else {
      geoknife::download(airtemp_job, destination = paste0(getwd(), "/", filestring_max)) 
    }
    
    filestring_min <- paste0("topowx_min_", yr, ".nc")
    
    #Create Geoknife fabric -- i.e. what data to get (temperature MINIMA for specified years)
    airtemp_fabric <- geoknife::webdata(list(
      url = airtemp_url,
      variables = "tmin",
      times = as.POSIXct(c(begstring, endstring), tz = "UTC")
    ))
    
    airtemp_job <- geoknife::geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
    if (geoknife::error(airtemp_job) == TRUE) {
      while (geoknife::error(airtemp_job) == TRUE) {
        airtemp_job <- geoknife::geoknife(stencil = airtemp_stencil, fabric = airtemp_fabric, knife = airtemp_knife)
      }
      geoknife::download(airtemp_job, destination = paste0(getwd(), "/", filestring_min)) 
    } else {
      geoknife::download(airtemp_job, destination = paste0(getwd(), "/", filestring_min)) 
    }
  }
  
  #The TopoWx files come in with CRS +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 and resolution 0.008333333
  #Loop through downloaded files & project to common CRS
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  downloaded_files <- list.files(pattern = "topowx_*")
  
  for (i in 1:length(downloaded_files)) {
    downloaded_file <- raster::brick(downloaded_files[i])
    projected_file <- raster::projectRaster(downloaded_file, crs = common_crs, res = 800, method = "bilinear")
    
    projected_file_name <- paste0(substr(downloaded_files[i], 1, 15), "_proj", ".tif")
    raster::writeRaster(projected_file, projected_file_name, overwrite = TRUE, options = "compress=LZW")
    
    file.remove(downloaded_files[i]) #Delete originally downloaded file, leaving only projected version
  }
  
  rm(downloaded_file)
}


#### Fetch Daymet precipitation dailies ####
# Arguments:
# x                    Polygon defining data extent (should have "FIPS" attribute)
# yrs                  Vector of years for which to download (valid range: 1980-2019)
# out.dir              Directory output will be written
# retrynum             Number of times to retry download
# sleepnum             Seconds between tries
fetch_daymet <- function(x, yrs, out.dir = getwd(), retrynum = 10, sleepnum = 5) {
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  
  if (sum(yrs < 1980) > 0 | sum(yrs > 2019) > 0)
    stop("Vector of years contains a year not in range -- all years must be between 1980-2019") 
  
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
    daymet <- FedData::get_daymet(template = x,
                                  label = paste0("daymet_", x$FIPS),
                                  elements = "prcp",
                                  years = yrs,
                                  region = "na",
                                  tempo = "day",
                                  extraction.dir = out.dir,
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
    stop("Could not get data!")
  }
  
  #Project returned result to common CRS & mask by county boundary
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  result <- raster::brick(result)
  result <- raster::projectRaster(result, crs = common_crs, res = 1000, method = "bilinear")
  result <- raster::mask(raster::crop(result, raster::extent(x)), x)
  
  #Result is written to disk automatically by FedData::get_daymet(), so now need to overwrite it with result that has the desired common CRS & compression
  result_name <- paste0("daymet_", x$FIPS, "_prcp_day.tif")
  raster::writeRaster(result, result_name, overwrite = TRUE, options = "COMPRESS=LZW")
  
  return(result)
}


#### Calculate degree days (TopoWx) ####
# Calculates degree days for each day of year, sums results through Dec. 31, & averages all Dec. 31 sums across specified list of years
#Pass in a list of paths to all min/max temp files for all years of interest, list of years of interest, & county polygon
# Arguments:
# x                    Polygon defining download extent
# yrs                  Years of interest
degree_days <- function(tempdata, years, county) {
  countyproj <- sp::spTransform(county, crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 ")) #Reproject county to same CRS as TopoWx data has when downloaded
  i <- 1
  dds_sum_list <- vector("list", length(years)) #Empty list to contain rasters of degree days by Dec. 31 for each year (to be averaged later)
  
  for (year in years) {
    print(paste0("Now calculating degree days for year: ", year, " ... Current time is: ", Sys.time()))
    topomax <- raster::brick(tempdata[grepl(paste0("_max_", year), tempdata)]) #Read in all dailies for temp. maxima for given year as raster brick
    topomin <- raster::brick(tempdata[grepl(paste0("_min_", year), tempdata)]) #Read in all dailies for temp. minima for given year as raster brick
    
    topomax_masked <- raster::mask(raster::crop(topomax, raster::extent(countyproj)), countyproj) #Mask to actual county outline
    topomin_masked <- raster::mask(raster::crop(topomin, raster::extent(countyproj)), countyproj)
    
    dds_eq <- ((topomax_masked + topomin_masked) / 2) - 5 #DDs for base temp. of 5 Celsius = (Tmax + Tmin) / 2 - 5
    dds_reclass <- reclassify(dds_eq, c(-Inf, 0, 0)) #Treat anything w/ a 0 or negative result for DDs as 0, i.e. no degree-day accumulation
    dds_sum <- raster::calc(dds_reclass, sum) #Add all bands in brick, i.e. days of year, together, to get total degree days by Dec. 31 for each pixel
    dds_sum_list[[i]] <- dds_sum #Add dds_sum result for given year to list
    i <- i + 1
    
    raster::writeRaster(dds_sum, paste0("ddssum_", year, ".tif"), overwrite = TRUE, options = "COMPRESS=LZW")
  }
  
  dds_sum_brick <- raster::brick(dds_sum_list)
  dds_avg <- raster::calc(dds_sum_brick, mean) #Average together degree days by Dec. 31 across all years
  
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  dds_avg_proj <- raster::projectRaster(dds_avg, crs = common_crs, res = 800, method = "bilinear") #Set output to common projection
  raster::writeRaster(dds_avg_proj, "dds_avg.tif", overwrite = TRUE, options = "COMPRESS=LZW")
}


#### Calculate mean annual precipitation (Daymet) ####
# Note: All Daymet years have 365 days, including leap years
# Arguments:
# x                    Polygon defining county of interest (should have "FIPS" attribute)
# yrs                  Vector of years of interest
# indata               Should be the written-to-disk result of fetch_daymet(), which is a RasterBrick of Daymet precip dailies over the years of interest
#"indata" argument should be the returned result of fetch_daymet, which is a RasterBrick of Daymet precip dailies over the years of interest
annual_precip <- function(x, yrs, indata) {
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  
  if (!class(indata)[1] == "RasterBrick")
    stop("indata must be a RasterBrick object")
  
  if (length(yrs) != raster::nbands(indata) / 365)
    stop("Length of your years vector does not seem to match the number of available years in your Daymet download")
  
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
  
  sumvector <- rep(1:length(years), each = 365)
  
  days_sum <- raster::stackApply(indata, sumvector, fun = sum) #Sum total precip. for year 1 (days 1-365), year 2 (days 366-730), & so on, through year x
  years_mean <- raster::calc(days_sum, mean) #Mean of total annual precips across full year range
  years_mean_masked <- raster::mask(raster::crop(years_mean, raster::extent(x)), x)
  
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  if (!sp::identicalCRS(years_mean_masked, x))
    years_mean_masked_proj <- raster::projectRaster(years_mean_masked, crs = common_crs, res = 1000, method = "bilinear") #Set output to common projection
  
  raster::writeRaster(years_mean_masked_proj, paste0("mean_annual_precip_", x$FIPS, ".tif"), overwrite = TRUE, options = "COMPRESS=LZW")
}


#### Fetch NED data ####
# Arguments:
# x                    Polygon defining data extent (should have "FIPS" attribute)
# out.dir              Directory output will be written
# retrynum             Number of times to retry download
# sleepnum             Seconds between tries
fetch_ned <- function(x, out.dir = getwd(), retrynum = 10, sleepnum = 5) {
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  
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
    NED <- FedData::get_ned(
      template = x,
      label = paste0("NED_", x$FIPS),
      res = 1,
      extraction.dir = out.dir,
      force.redo = TRUE)
    
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
    stop("Could not get data!")
  }
  
  #Project returned result to common CRS and mask by county
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  result <- raster::projectRaster(result, crs = common_crs, res = 30, method = "bilinear")
  result <- raster::mask(raster::crop(result, raster::extent(x)), x)
  
  #Result is written to disk automatically by FedData::get_ned(), so now need to overwrite it with result that has the desired common CRS, compression, extent, & name
  result_name <- paste0("NED_", x$FIPS, ".tif")
  raster::writeRaster(result, result_name, overwrite = TRUE, options = "COMPRESS=LZW")
  
  #Get rid of the initial automatically written output
  file_to_remove <- list.files(pattern = "*_NED_1.tif")
  file.remove(file_to_remove)
  
  return(result)
}


#### Fetch NASS CDL data ####
# Arguments:
# x                    Polygon defining data extent (should have "FIPS" attribute)
# yr                   Year of download (2008-2019)
# out.dir              Directory output will be written
# retrynum             Number of times to retry download
# sleepnum             Seconds between tries

fetch_cdl <- function(x, yr = 2019, out.dir = getwd(), retrynum = 10, sleepnum = 5) {
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  
  if(yr < 2008 | yr > 2019)
    stop("Not a valid year -- needs to be 2008-2019") 
  
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
    CDL <- FedData::get_nass(
      template = x,
      label = paste0("CDL_", x$FIPS),
      year = yr,
      extraction.dir = out.dir,
      force.redo = TRUE,
      progress = TRUE)
    
    return(CDL)
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
    stop("Could not get data!")
  }
  
  #Project returned result to common CRS and mask by county
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  if (!sp::identicalCRS(result, x))
    result <- raster::projectRaster(result, crs = common_crs, res = 30, method = "ngb")
  result <- raster::mask(raster::crop(result, raster::extent(x)), x)
  
  #Result is written to disk automatically by FedData::get_nass(), so now need to overwrite it with result that has the desired common CRS, compression, extent, & name
  result_name <- paste0("CDL_", yr, "_", x$FIPS, ".tif")
  raster::writeRaster(result, result_name, overwrite = TRUE, options = "COMPRESS=LZW")
  
  #Get rid of the initial automatically written output
  files_to_remove <- list.files(pattern = "cdl_2019.tif")
  file.remove(files_to_remove)
  
  return(result)
}


#### Fetch NLCD data ####
# Arguments:
# x                    Polygon defining data extent (should have "FIPS" attribute)
# yr                   Year of download (acceptable values: 2001, 2004, 2006, 2008, 2011, 2016)
# out.dir              Directory output will be written
# retrynum             Number of times to retry download
# sleepnum             Seconds between tries

fetch_nlcd <- function(x, yr = 2016, out.dir = getwd(), retrynum = 10, sleepnum = 5) {
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  
  yearlist <- c(2001, 2004, 2006, 2008, 2011, 2016)
  if (!(yr %in% yearlist))
    stop("Not a valid year -- needs to be 2008-2019") 
  
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
    NLCD <- FedData::get_nlcd(
      template = x,
      label = paste0("NLCD_", x$FIPS),
      year = yr,
      extraction.dir = out.dir,
      dataset = "Land_Cover",
      landmass = "L48",
      force.redo = TRUE)
    
    return(NLCD)
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
    stop("Could not get data!")
  }
  
  #Project returned result to common CRS and mask by county
  common_crs <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  if (!sp::identicalCRS(result, x))
    result <- raster::projectRaster(result, crs = common_crs, res = 30, method = "ngb")
  result <- raster::mask(raster::crop(result, raster::extent(x)), x)
  
  #Result is written to disk automatically by FedData::get_nlcd(), so now need to overwrite it with result that has the desired common CRS, compression, extent, & name
  result_name <- paste0("NLCD_", yr, "_", x$FIPS, ".tif")
  raster::writeRaster(result, result_name, overwrite = TRUE, options = "COMPRESS=LZW")
  
  #Get rid of the initial automatically written output
  files_to_remove <- list.files(pattern = "*Land_Cover*")
  file.remove(files_to_remove)
  
  return(result)
}