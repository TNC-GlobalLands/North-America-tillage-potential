#######################################################################################
#######################################################################################
# RUN THIS SECOND - CREATES A RANDOM SAMPLE FOR EACH SSURGO POLYGON IN EACH COUNTY. 
# THE SAMPLE IS THE USED TO PULL VALUES FROM THE NASS RASTERS AND CREATE A RESPONSE (y)
# VARIABLE FOR USE IN THE CROP MODEL. PLEASE SOURCE FUNCTIONS IN FUNCTION SECTION.
#######################################################################################
#######################################################################################
source("D:/CROP/CODE/Functions.R")
#######################################################
# PERCENT AREA RANDOM POINT SAMPLE OF SSURGO POLYGONS #
#######################################################	

# SHAPEFILE NAME
inshape="SSURGO"

# PERCENT SUBSAMPLE
p=0.02

# STATE NAME AND ABBREVIATION
states=c("COLORADO", "IOWA", "KANSAS","MINNESOTA","NEBRASKA","NORTHDAKOTA",
         "SOUTHDAKOTA", "WYOMING")
  states.abr=c("CO","IA","KS","MN","NE","ND","SD","WY")

#for(s in 1:length(states) ) {
#  sn=states[s]
#  sa=states.abr[s]
#  path=paste("D:/CROP/", sn, sep="")
#    dirs <- list.files(path, pattern=sa)
#  for (d in dirs) {                
#    setwd(paste(paste("D:/CROP/", sn, sep=""), d, sep="/"))	
#      spath=paste(getwd(), "VECTOR", sep="/") 
#       print(paste("PROCESSING", spath, sep=" : "))
#     shape <- readOGR(dsn=spath, layer=inshape) 
#     csamp <- psample(shape, pct=p, join=FALSE, msamp=2, sf=4046.85, 
#                      stype="random", iter=100)
#     writeOGR(csamp, spath, "sample", driver="ESRI Shapefile")
#  }
#} 

# FIXED RANDOM SAMPLE
n=2
for(s in 1:length(states) ) {
  sn=states[s]
  sa=states.abr[s]
  path=paste("D:/NACR/CROP/", sn, sep="")
    dirs <- list.files(path, pattern=sa)
  for (d in dirs) {                
    setwd(paste(paste("D:/NACR/CROP/", sn, sep=""), d, sep="/"))	
      spath=paste(getwd(), "VECTOR", sep="/") 
    print(paste("PROCESSING", spath, sep=" : "))
     shape <- readOGR(dsn=spath, layer=inshape) 
       sdat <- sapply(slot(shape, 'polygons'), function(i) spsample(i, n=n, type="random"))
         bad.rec <- vector() 
           for(ij in 1:length(sdat)) { if( class(sdat[[ij]]) == "NULL" ) bad.rec <- append(bad.rec,ij) }
             sdat <- sdat[-bad.rec]
	           csamp <- do.call("rbind", sdat)
			csamp <- SpatialPointsDataFrame(csamp, data.frame(ID=1:length(csamp)) )   
     writeOGR(csamp, spath, "sample", driver="ESRI Shapefile",
	          check_exists=TRUE, overwrite_layer=TRUE)
  }
} 


psample <- function(y, pct=0.10, msamp=5, sf=4046.86, stype="random", ...) {
  if (!require (sp)) stop("sp PACKAGE MISSING")
    if (!inherits(y, "SpatialPolygonsDataFrame")) stop("MUST BE SP SpatialPolygonsDataFrame OBJECT")
	  sarea <- function(x, pct, msamp, sf, stype, ...) {
	  #hole.check <- sapply(slot(x, "Polygons"), function(x) slot(x, "hole"))
      #  hole.check <- which(hole.check == FALSE)
	    ac <- sapply(slot(x, 'area'), function(a) x@area / sf )
          ns <- round((ac * pct), digits=0)
            if(ns < msamp) {ns = msamp}
              psamp <- spsample(x, n=ns, type=stype, ...)
        	    samples <- SpatialPointsDataFrame(psamp, data.frame(ID=rep(x@ID, length(psamp))))					  
	  	  return(samples)
        }	
	sdat <- sapply(slot(y, 'polygons'), function(j) sarea(x=j, pct=pct, msamp=msamp, 
	               sf=sf, stype=stype, ...))	
		bad.rec <- vector() 
          for(ij in 1:length(sdat)) { if( class(sdat[[ij]]) == "NULL" ) bad.rec <- append(bad.rec,ij) }
            sdat <- sdat[-bad.rec]
	          samples <- do.call("rbind", sdat)		
	return(samples)  
  }

sample.dat <- psample(s)   
slot(s, "polygons") <- lapply(slot(s, "polygons"), checkPolygonsHoles)   
   
samples <- sapply(slot(s, 'polygons'), function(j) sarea(x=j, pct=10, msamp=1, 
	              sf=4046.86, stype="random") )	  

samples <- sapply(s[1:dim(s)[1]), function(j) sarea(x=j, pct=10, msamp=1, 
	              sf=4046.86, stype="random") )	    
  
test <- sapply(slot(s, 'polygons'), function(j) {spsample(j, n=10, type="random")} )
  
require(sp)
require(rgdal)
setwd("D:/CROP/TEST")
shape <- readOGR(getwd(), "SSURGO")
  s <- shape[1:20,]
    samp <- psample(s)








