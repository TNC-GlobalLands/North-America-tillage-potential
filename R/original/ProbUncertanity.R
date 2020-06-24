# Evaluate spatial uncertainty of estimates

load("C:/evans/India/Bear/Himalayan_bear_sdm.RData")

library(ranger)
library(sp)
library(raster)
library(spatialEco)

xvars <- stack(list.files(file.path(getwd(),"data"), pattern="tif$", 
               full.names=TRUE))

if(length(idx) > 10) {
  cat("Prediction variance optimization removing -", length(idx), 
      "observations", "\n")
  x.opt <- x[-idx,]
  y.opt <- y[-idx]  
( mdl <- ranger(x=x.opt[,sel.vars], y=as.factor(y.opt), probability = TRUE, 
                num.trees = b, importance="permutation", 
				write.forest = TRUE, keep.inbag = TRUE) )
  } else {
( mdl <- ranger(x=x[,sel.vars], y=as.factor(y), probability = TRUE, 
                num.trees = b, importance="permutation", 
				write.forest = TRUE, keep.inbag = TRUE) )
  }
 
 
predict.se <- function(model, data) { 
  predict(model, data, type = "se")$se  
}
sdm.se <- predict(xvars[[sel.vars]], mdl, fun=predict.se, 
                  filename = "sdm_se.tif", index=1:2)
  
# Entropy function
entropy.empirical <- function(y){
  entropy <- function(freqs){
    freqs <- freqs / sum(freqs)
    H = -sum( ifelse(freqs > 0, freqs*log(freqs), 0) )
    return(H)
  }
  return( entropy(y / sum(y)) )
}

sdm <- brick("BrownBear_SDM.tif") 
seu <- calc(sdm[[c(2,1)]], entropy.empirical)
  plot(seu)
    plot(sdata, pch=20, cex=0.75)
  