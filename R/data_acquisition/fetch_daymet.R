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
