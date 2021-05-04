#' @title Download NED
#' @description National Elevation Dataset 
#' 
#' @param x          Polygon defining download extent
#' @param yrs        Year(s) to download eg 2000 or 2000:2005
#' @param type       Climate metric c("prcp", "tmax", "tmin", "dayl", "srad", 
#'                                    "swe", "vp")
#' @param out.dir    Directory will output will be written
#' @param retrynum   Number of times to retry download
#' @param sleepnum   seconds between  tries 
#'
#' @return raster object, 
#'
#' @note depends; sf, sp, FedData > 3.0, purrr 
#' Data is in the +proj=longlat +datum=NAD83 +no_defs projection 
#' res 1 is 0.0002777778 (30m) 
#' res 13 is 0.0002777778 (10m) 
#' 
#' @author Eugene Yacobson <eyacobson@@tnc.org> and Jeffrey S. Evans  
#'                                                              
#' @examples
#' library(raster) 
#' # cbdy is polygon of study area
#'  fetch_daymet(cbdy, 1990:2019) 
#' 
#'@export fetch_daymet
fetch_daymet <- function(x, yrs, type = c("prcp", "tmax", "tmin", "dayl", "srad", "swe", "vp"),
                         out.dir = getwd(), retrynum = 10, sleepnum = 5) {
  p <- as.data.frame(installed.packages())[,c("Package", "Version")]
    if(!"FedData" %in% p$Package)
     stop("Version 3.0.0.9000 of FedData not installed, 
	        install from github ropensci/FedData")
    if(packageVersion("FedData") < "3.0.0.9000")
      stop("Version 3.0.0.9000 of FedData not installed, 
	        install from github ropensci/FedData") 					  
  if(!any(class(x)[1] == c("sf", "SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object") 
  if (any(yrs %in% 1980:2019 == FALSE))
    stop("yrs contain a year not in range of 1980-2019") 
  tryCatch({
  geo.prj = sf::st_crs("+proj=longlat +datum=NAD83 +no_defs")
    if(any(class(x)[1] == c("SpatialPolygonsDataFrame", "SpatialPolygons"))){ 
      if(!sf::st_crs(as(x, "sf")) == geo.prj)
    	  x <- as(sf::st_transform(as(x, "sf"), geo.prj),"Spatial")
    } else if(any(class(x)[1] == "sf")) {
      if(!sf::st_crs(x) == geo.prj)
    	  x <- as(sf::st_transform(x, geo.prj),"Spatial")  
    }
    if(class(x)[1] == "sf") x <- as(x, "Spatial")
  })
  get_data <- function() {
    daymet <- FedData::get_daymet(template = x,
                                  label = "daymet",
                                  elements = type[1],
                                  years = yrs,
                                  region = "na",
                                  tempo = "day",
                                  extraction.dir = out.dir,
                                  progress = TRUE)
    return(daymet)
  }
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL) 
    result <- NULL
      try_number <- 1 
  while(is.null(result) && try_number <= retrynum) {
    cat("Downloading Daymet Precipitation", yrs, "attempt:", try_number, "of", retrynum, "\n")
      try_number <- try_number + 1
      result <- get_data_attempt()
    Sys.sleep(sleepnum)
  }
  if (try_number > retrynum) {
    warning("Could not get data!")
  }
  return(daymet)
}

