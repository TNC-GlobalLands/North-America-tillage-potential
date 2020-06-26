#### Create directory structure based on root/states/counties 
####   writes shapefile of county boundary "bdy" into each
####   state/county directory. Parses by county name
#### For counties with more than one polygon, the data is
####   filtered to retain the largest polygon
#### Data is in EPSG:102003 USA_Contiguous_Albers_Equal_Area_Conic
#### https://epsg.io/102003
library(sp)
library(rgdal)
library(spatialEco)

setwd("C:/evans/tillage/model")

counties <- readOGR(getwd(), "CONUScounties")
  counties <- explode(counties, sp = TRUE)

root.dir = getwd()
  for(s in unique(counties$STATE_NAME)) {
    dir.create(file.path(root.dir, s), 
               showWarnings = FALSE)
    state <- counties[counties$STATE_NAME == s,]
      state.dir <- file.path(root.dir, s)  
    for(i in unique(state$CONAME)){
      dir.create(file.path(state.dir, i), 
               showWarnings = FALSE)
	  cty <- state[state$CONAME == i,]
        if(nrow(cty) > 1) {
	      cty <- cty[which.max(rgeos::gArea(cty, byid=TRUE)),]
	        cat(s, "-", i, "had more than one polygon", "\n")
	    }	  
  	  writeOGR(cty, file.path(state.dir, i), "bdy",
  	  		   driver="ESRI Shapefile",
	  		   check_exists=TRUE,
               overwrite_layer=TRUE)
    }
  }
