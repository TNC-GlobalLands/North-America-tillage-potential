#' @title Download NLCD
#' @description National Landcover Dataset 2008-2019
#' 
#' @param x          Polygon defining download extent
#' @param yr         Year of download c(2001, 2004, 2006, 2008, 2011, 2016)
#'                   2016 is default, 2008 is for landcover only
#' @param data.set   data product c("Land_Cover", "Impervious", "Tree_Canopy")
#' @param land.mass  Content landmass c("L48", "AK", "HI", "PR") representing  
#'                   'L48' (lower 48 US states, the default), 'AK' (Alaska), 
#'                   'HI' (Hawaii), and 'PR' (Puerto Rico)
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
#'   for(i in c(2001, 2004, 2006, 2008, 2011, 2016)){
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
#'@export fetch_nlcd
fetch_nlcd <- function(x, yr = c(2001, 2004, 2006, 2008, 2011, 2016),
                       data.set = c("Land_Cover", "Impervious", "Tree_Canopy"), 
                       land.mass = c("L48", "AK", "HI", "PR"), 					   
                       out.dir = getwd(), 
                       retrynum = 10, sleepnum = 5) {
  p <- as.data.frame(installed.packages())[,c("Package", "Version")]
    if(!"FedData" %in% p$Package)
     stop("Version 3.0.0.9000 of FedData not installed, 
	        install from github ropensci/FedData")
    if(packageVersion("FedData") < "3.0.0.9000")
      stop("Version 3.0.0.9000 of FedData not installed, 
	        install from github ropensci/FedData") 
  if(!any(class(x)[1] == c("sf","SpatialPolygonsDataFrame", "SpatialPolygons")))
    stop("x must be an sp or sf polygon object")
  if(length(yr) > 1)
    yr = yr[6]	
  if(!yr %in% c(2001, 2004, 2006, 2008, 2011, 2016))
    stop("Not a valid year") 
  if(yr == 2008 & data.set != "Land_Cover")
    stop("Only Landcover is avalibie for 2008")
  land.mass = land.mass[1]
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
      label = "nlcd",
      year = yr,
	  dataset = data.set[1], 
	  landmass = land.mass[1],
      extraction.dir = out.dir,
      force.redo = TRUE)  
    return(NLCD)
  }
  get_data_attempt <- purrr::possibly(get_data, otherwise = NULL) 
    result <- NULL
      try_number <- 1 
  while(is.null(result) && try_number <= retrynum) {
    cat("Downloading", "NLCD-", data.set, yr, "attempt:", try_number, "of", retrynum, "\n")
      try_number <- try_number + 1
      result <- get_data_attempt()
    Sys.sleep(sleepnum)
  }
  if (try_number > retrynum) {
    warning("Could not get data!")
  }
  return(result)
}
