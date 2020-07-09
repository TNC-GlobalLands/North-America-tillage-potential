#' @title Download NASS-CDL
#' @description NASS Cropland Data Layer 2008-2019
#' 
#' @param x          Polygon defining download extent
#' @param yr         Year of download (2008=2019)
#' @param out.dir    Directory will output will be written
#' @param retrynum   Number of times to retry download
#' @param sleepnum   seconds between  tries 
#'
#' @return raster object
#'
#' @note depends; sf, sp, FedData > 3.0, purrr 
#' 
#' @author Eugene Yacobson <eyacobson@@tnc.org> and Jeffrey S. Evans  
#'                                                              
#' @examples
#' library(raster) 
#' # cbdy is polygon of study area
#' nass.att <- read.csv(file.path(root, "nass", "NASS_attributes.csv"))
#'    rc <- data.frame(id = as.numeric(nass.att$value), 
#'                     v=as.numeric(nass.att$reclass)) 
#' dir.create(file.path(getwd(), "nass"), 
#'            showWarnings = FALSE)
#'   for(i in 2008:2009){
#'     tryCatch({
#'     nass <- fetch_cdl(cbdy, yr = i, out.dir = file.path(getwd(), "nass"))
#' 	cat("Reclassifying", "nass", i, "\n")				 
#' 	nass <- raster(file.path(getwd(), "nass", paste0("cdl_", i, ".tif")))
#' 	  nass <- raster::mask(raster::crop(nass,extent(cbdy)),cbdy)
#'         nass <- raster::subs(nass, rc)
#' 	writeRaster(nass, file.path(getwd(), "nass", 
#' 	            paste0("cdl_", i, ".tiff")),
#'                 overwrite=TRUE, options="COMPRESS=LZW" )
#'     })
#'   }
#' nass <- stack(list.files(file.path(getwd(), "nass"), "tif$", 
#'               full.names=TRUE))
#' 
#'@export fetch_cdl
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
