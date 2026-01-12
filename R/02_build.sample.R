#################################################
# Create response (Y) point data from SSURGO soil data
#
#  Assumes:
#  1) CONUS-level gNATSGO units raster ("NATSGO_UNITS.tif")
#      https://www.nrcs.usda.gov/resources/data-and-reports/gridded-national-soil-survey-geographic-database-gnatsgo
#      download GeoPackage: https://nrcs.app.box.com/v/soils/folder/233393842838
#  2) State-level NASS tillage raster(s) ("nass.tif")
#  Have already run build.states.R first resulting in;
#    3) a directory structure of root/state/county
#    4) A mask raster representing tillage data (0,1,NA) areas 
#
# Contact:
# Jeffrey S. Evans, Ph.D.,
# Landscape Ecologist & Biometrician 
# SAGE Insights (Spatial Analysis and Global Ecology)
# Visiting Professor | University of Wyoming | Ecosystem Sciences
# Office, Agriculture - C6, Laramie, WY  82070
# sage_insights@outlook.com  
#################################################
pkg <- c("sf", "terra", "spatialEco", "tcltk")
  install.packages(setdiff(pkg, rownames(installed.packages())))  		 
    lapply(pkg, require, character.only = TRUE)

root = "D:/tillage"
setwd(root)

# SURGO soil units raster
soil.raster = file.path(root, "soil", "NATSGO_UNITS.tif")

usgs.prj = sf::st_crs("EPSG:5070")
min.sample = 1   # minimum number of samples per soil unit
max.sample = c(NA, 1000)[2]
pct = 0.05       # fractional sample per soil unit

#*********************************************
# Create state/county list for iteration
#*********************************************	 
states <- basename(list.dirs(path = file.path(root, "model"),
               full.names = TRUE, recursive = FALSE))
d <- list.dirs(path = file.path(root, "model"),
          full.names = TRUE, recursive = TRUE)
  d <- d[-which(stringr::str_detect(d, "results"))]
  
  # this removes base directory and is dependent on original root path
  d = d[unlist(lapply(strsplit(d, "/"), \(i) {
      ifelse(length(i) == 5, TRUE, FALSE)
    }))]

#*********************************************
# Start loop for sampling soil data
#*********************************************	 
  for(j in 1:length(d)) {
    ct = d[j]
    state = unlist(strsplit(ct, "/"))[4]
    county = unlist(strsplit(ct, "/"))[5]
  	setwd(ct)
	cat("\n", "Creating sample for", ct, j, "in", length(d), "\n")
      flush.console(); Sys.sleep(0.01)			
    
	nass.raster <- file.path(dirname(ct), "nass.tif")
	m <- rast("mask.tif")
      if(st_crs(m) != usgs.prj) {
        bdy <- st_read("data.gpkg", "bdy")	
	    m <- project(m, vect(bdy), method = "near", res = 30)  
	  }
	soil <- project(crop(rast(soil.raster), m), m, method = "near")
	  soil <- mask(soil, m)
	nass <- project(crop(rast(nass.raster), m), m, method = "near")
	  nass <- ifel(nass >= 2, 1, 0)
	
    ids <- na.omit(as.numeric(unique(soil[])))		
    pb <- tcltk::tkProgressBar(title = "progress bar", min = 0,
                               max = length(ids), width = 300)
	samples <- list()					
      for(i in 1:length(ids)) {
	    Sys.sleep(0.1)		
        xy <- xyFromCell(soil, which(soil[] == ids[i]))
          if(nrow(xy) < 100) {
            next
          }	 		
	      n = round( (nrow(xy) * 30^2 / 4046.86 * pct), 0)
            if(n < min.sample) n = min.sample
            if(n > max.sample & !is.na(max.sample)) n = max.sample
		xy <- as.data.frame(matrix(xy[sample(1:nrow(xy), n),],ncol=2))
		  xy <- data.frame(ids[i], xy)
		    names(xy) <- c("ID","x","y")
		samples[[i]] <- xy 
		tcltk::setTkProgressBar(pb, i, label=paste(round(i/length(ids)*100, 0), "% done"))
	  }	
	  close(pb)
        samples <- do.call("rbind", samples)
		  samples <- st_as_sf(samples, coords = c("x", "y"), crs = usgs.prj, agr = "constant")
	        samples$ID <- extract(nass, samples)[,2]
			  na.idx <- which(is.na(samples$ID))
			    if(length(na.idx) > 0) samples <- samples[-na.idx,]
      st_write(samples, "data.gpkg", "sample", append = FALSE)
  } # end loop
