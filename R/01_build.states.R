#################################################
# 1) Builds state/county directory structure, 
# 2) Creates county-level "nass.tiff" data
# 3) Creates county-level "data.gpkg" contaning state boundary "bdy"
# 4) Creates mask object using NLCD and NASS
#
# Contact:
# Jeffrey S. Evans, Ph.D.,
# Landscape Ecologist & Biometrician 
# SAGE Insights (Spatial Analysis and Global Ecology)
# Visiting Professor | University of Wyoming | Ecosystem Sciences
# Office, Agriculture - C6, Laramie, WY  82070
# sage_insights@outlook.com  
#################################################
pkg <- c("terra", "sf", "FedData", "tigris")
  install.packages(setdiff(pkg[-5], rownames(installed.packages())))  		 
    lapply(pkg, require, character.only = TRUE)

# set root directory where state/sounty model directories will be created
root = "D:/tillage_test/model"
  setwd(root)

usgs.prj <- st_crs("EPSG:5070")
nass.years <- 2007:2024
			   
#************************************************ 

# Create states list
rm.st <- c("Alaska", "American Samoa", "Commonwealth of the Northern Mariana Islands",
           "Guam", "Hawaii", "Puerto Rico", "United States Virgin Islands")
states.bdy <- states() 
  states.bdy <- states.bdy[-which(states.bdy$NAME %in% rm.st),]
    states <- sort(unique(states.bdy$NAME))
# plot(st_geometry(states.bdy))

	
  for(j in 1:length(states)) {
    st <- states[j]
      dir.create(file.path(root, st), 
                 showWarnings = FALSE)
	setwd(file.path(root, st))
	  stbdy <- states.bdy[which(states.bdy$NAME %in% st),]	    
	    cty.geo <- counties(state = st, cb = FALSE)
	      cty <- st_transform(cty.geo, usgs.prj) 
	        cty.names <- sort(unique(cty$NAME))	
	  for(i in 1:length(cty.names)) {
	    dir.create(file.path(root, st, cty.names[i]), 
                   showWarnings = FALSE)
	    setwd(file.path(root, st, cty.names[i]))
		cat("\n", "Processing", file.path(root, st, cty.names[i]), i, "in", length(cty.names), "\n")
          flush.console(); Sys.sleep(0.01)			
		sub.geo <- cty.geo[which(cty.geo$NAME %in% cty.names[i]),] 	    
		ct.sub <- cty[which(cty$NAME %in% cty.names[i]),] 	   
		st_write(ct.sub, "data.gpkg", "bdy")
	    
		nlcd <- get_nlcd(template = sub.geo, landmass = "L48", label = "nlcd", year = 2021)
		  nlcd <- mask( project(crop(nlcd, sub.geo), ct.sub, res=30), ct.sub) 
	  	# nass <- get_nass_cdl(sub.geo, label = "nass", year = 2021)
		#   nass <- mask( project(crop(nass, sub.geo), ct.sub, res=30), ct.sub) 
		  
		# recalss nlcd to create mask
        mc <- cbind(c(11, 12, 21, 22, 23, 24, 31, 41,
                  42, 43, 51, 52, 71, 72, 73, 74, 81,
                  82, 90, 95), c(NA,NA,NA,NA,NA,NA,rep(1,14)))
		m <- classify(nlcd, mc)
            writeRaster(m, filename = "mask.tif", overwrite = TRUE)
          rm(r)
	    gc()
	  }
  }
