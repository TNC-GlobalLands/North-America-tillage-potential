###############################################################
#   CREATE Y VARIABLE (TILLED/NOTTILLED) BASED ON FREQUENCY   #
###############################################################
require(raster)
require(sp)
require(rgdal)
t=1 #TILLAGE THRESHOLD 
st.abb <- "KS"
state <- "KANSAS"

nass.path <- paste(paste("D:/NACR/CROP", state, sep="/"), "RASTER/NASS", sep="/")
  setwd(nass.path)
#    nass2008 <- raster(paste(getwd(), "nass2008.img", sep="/"))
#    nass2009 <- raster(paste(getwd(), "nass2009.img", sep="/"))
#    nass2010 <- raster(paste(getwd(), "nass2010.img", sep="/"))
#    nass2011 <- raster(paste(getwd(), "nass2011.img", sep="/"))
# nass <- c(nass2008, nass2009, nass2010, nass2011)

nass2006 <- raster(paste(getwd(), "nass2008.img", sep="/"))
nass2007 <- raster(paste(getwd(), "nass2009.img", sep="/"))
  nass <- c(nass2006, nass2007)   
	
path=paste("D:/NACR/CROP", state, sep="/")
dirs <- list.files(path, pattern=st.abb)  
  for (d in dirs) {                
    setwd(paste(path, d, sep="/"))	
      spath = paste(getwd(), "VECTOR", sep="/") 
     shape <- readOGR(dsn=spath, layer="sample") 
	 if ( is.na(match("y", names(shape@data))) == "FALSE") 
       { shape@data <- shape@data[-match("y", names(shape@data))] }	 
    crop <- as.data.frame(array(0, dim=c( dim(shape)[1], 0 )))  
	  for(i in nass) {
        r <- extract(i, shape)
          crop <- cbind(crop, r)
	   }  
    crop <- data.frame(crop, y=apply(crop, 1, function(x) 
	           ifelse(sum(x) >= t, 1, 0) ) )
  	shape@data <- data.frame(shape@data, y=crop$y)
      writeOGR(shape, spath, "sample", driver="ESRI Shapefile", check_exists=TRUE, 
	           overwrite_layer=TRUE)
  }
