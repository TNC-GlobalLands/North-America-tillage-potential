### Fetch TopoWx temperature dailies ####
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

