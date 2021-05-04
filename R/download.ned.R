#' @title Download the US National Elevation Dataset
#' @description Download, mosaic and mask 30m NED data 
#'
#' @param x          An sp or sf polygon feature class
#' @param m          An optional raster or rast object to provide 
#'                   projection reference and mask
#' @param index      A tile index sf object
#' @param data.dir   Directory to download tiles
#' @param out.file   Name of a raster written to disk
#' @param ...        Additional arguments passed to terra:writeRaster
#'
download.ned <- function(x, m = NULL, index = NULL, data.dir = NULL, 
                         out.file = NULL, ...) {
 ned.url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/"
  if(is.null(data.dir)) data.dir = tempdir()
  if(class(m)[1] == "SpatRaster") m <- raster::raster(m)
    x <- sf::st_transform(x, sf::st_crs(index))	  
    ned.query <- spatialEco::spatial.select(x, index)
    file.names = paste0("USGS_13_", ned.query$label, ".tif")
    urls <- paste0(ned.url, "TIFF/", ned.query$label, "/", file.names)
    raster.files <- vector()
      for(i in 1:length(urls)) {
        message("downloading ", i, " of ", length(urls), " tiles")
        raster.files[i] <- file.path(data.dir, file.names[i])
        if(!file.exists(file.path(data.dir, file.names[i]))){
            try(utils::download.file(urls[i], file.path(data.dir, file.names[i]),
    	      mode="wb") )
         if(!file.exists(file.path(data.dir, file.names[i])))
           message(file.names[i], " failed to download")
        } 
    }
	tiles <- lapply(raster.files, function(x) raster::raster(x))    
      if(length(tiles) > 1) {
        tiles$fun <- mean
        tiles$na.rm <- TRUE
        r <- do.call(raster::mosaic, tiles)
          names(r) <- "elev"
      } else {
	    r <- tiles[[1]]
        message("Not performing mosaic because either not requested or
	            only one tile was downloaded")
      }	  
    # project and mask
    r <- terra::project(rast(r), rast(m), method="bilinear")
    r <- raster::mask(raster::crop(raster(r), raster::extent(m)), m)
      names(r) <- "elev"
	if(file.exists(out.file)){
	  message(out.file, " already exists and will be overwritten")
	}
	r <- raster::writeRaster(r, filename=out.file, overwrite=TRUE, 
                             options="COMPRESS=LZW", ...)
	    if(file.exists(out.file)) {
	      message(out.file, " Successfully written")
		} else {
		  warning("Failed to write ", out.file) 
		}
  return(r)
}
