#******************************************
# Mosaic county-level  probability rasters
# to state-level
library(terra)
library(sf)

dir.create("D:/tmp/",showWarnings = FALSE)
  terraOptions(tempdir="D:/tmp/", datatype="FLT4S", tolerance = 1)

root = "D:/tillage/model"
cty <- st_read(file.path(root, "CONUScounties.shp"))

d <- list.dirs(path = root, full.names = TRUE, 
               recursive = FALSE)
done <-  unlist(lapply(strsplit(list.files(root, pattern = "_probs.tif$", 
              recursive = TRUE, full.names=TRUE),split = '/'), 
			  function(x) x[4]))
dirs <- unlist(lapply(strsplit(d,split = '/'), function(x) x[4]))
  missing.idx <- which(!dirs %in% done)
    if(length(missing.idx) > 0)
      d <- d[missing.idx]	

f <- list.files(d, "probs.tif", full.names = TRUE, 
                recursive = TRUE)
  for(i in 1:length(d)){
    s <- basename(d[i])
      sf <- f[which(stringr::str_detect(f, s))]
	    sf <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\1", sf)
          state <- cty[cty$STNAME == s,]		
    if(!file.exists(file.path(d[i], paste0(s, "_probs.tif")))) {
      Sys.sleep(0.1)
        cat("merging and exporting", s, "\n")	
	r <- lapply(sf, function(j){ 
	  rast(file.path(j, "probs.tif"), lyrs=1)
	})		
  	r <- sprc(r)  
  	  probs <- mosaic(r)
	    writeRaster(probs, file.path(d[i], paste0(s, "_probs.tif")))
	  
    flush.console()
    } else {
      cat(file.path(d[i], paste0(s, "_probs.tif")), "already exists", "\n")
    }
  }

# bdy <- st_transform(bdy, st_crs(crs(r[[1]])))
# exts <- lapply(r, function(x) {
#           y <- st_as_sf(max_extent(x))
# 		    st_set_crs(y, st_crs(bdy))
#         })
# plot(st_geometry(bdy))  
#   lapply(exts, function(x) plot(st_geometry(x), col="red", add=TRUE))
# lapply(exts, function(x) { st_intersects(x, bdy) })

