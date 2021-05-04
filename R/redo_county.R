#***********************************************
#***********************************************
# Check pixel consistency for missing DEM tiles 
#***********************************************
#***********************************************
library(sp)
library(sf)
library(terra)

root = "C:/evans/tillage"
model.dir = "missing_states"

#***********************************************
# Start of state(st) - county(ct) model loop
#***********************************************
states = list.dirs(path = file.path(root,model.dir),
                   full.names = TRUE, 
                   recursive = FALSE)
for(st in states) {
  state = basename(st)
  counties = list.dirs(path = st, full.names = TRUE, 
                       recursive = FALSE) 
  for(ct in counties) {
    county = basename(ct)  
    if(file.exists(file.path(ct,"probs.tif"))) {
      p <- terra::rast(file.path(ct,"probs.tif"))
      m <- terra::rast(file.path(ct,"mask.tif"))
        expected <- as.numeric(m[[1]][])
          expected <- length(expected[!is.na(expected)])
        p <- as.numeric(p[[1]][])
          p <- length(p[!is.na(p)])
	        if(abs(expected-p) > 250000) {
	          write(paste0(ct, ",", abs(expected-p)), 
			        file.path(root,"model_redo.csv"), 
	                append = TRUE)
	        }
        remove(p, m, expected)
	  gc()
    } else {
      next
    }
  }
}
