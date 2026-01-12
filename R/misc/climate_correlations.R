library(terra)
library(rfUtilities)
library(spatialEco)

setwd("C:/evans/tillage/climate")
r <- rast(list.files(getwd(), "tif$"))

csamp <- as.data.frame(spatSample(r, 500000, method="random"))
  csamp <- csamp[,-9]
  csamp <- csamp[-which(is.nan(csamp[,1])),]

# Multicollinearity  
mcl <- multi.collinear(csamp, perm = TRUE,, p=0.05, n = 99)
  mcl[mcl$frequency < 5,]$variables

# Pairwise collinearity
cor.names <- collinear(csamp, p=0.75)
  names(csamp)[-which(cor.names %in% names(csamp))]

# Pariwise nonlinearity 
nl.cor.names <- collinear(csamp, nonlinear = TRUE)  
  names(csamp)[-which(nl.cor.names %in% names(csamp))]
 