##########################################
##########################################
# SDM for Himalayan brown bear
##########################################
##########################################

#**** Only run once
p = as.data.frame(utils::installed.packages()[,c("Package", "Built")])
  if(("rfUtilities" %in% p$Package)) {
    if(p[p$Package == "rfUtilities",]$Built != "3.6.3")
      install.packages("rfUtilities")
 } else {
   install.packages("rfUtilities") 
 }  
#****

library(randomForest)
library(sp)
library(raster)
library(rgdal)
library(pdp)
library(classInt)
library(ggplot2)
library(rfUtilities)
library(spatialEco)

#set working directory
setwd("C:/evans/India/Bear")

##########################################
# stack all independent (x) variables
xvars <- stack(list.files(file.path(getwd(),"data"), pattern="tif$", 
               full.names=TRUE))

# load occurrence points
sdata <- readOGR(file.path(getwd(),"data"), "bb_pres_v3")

# Isotropic kernel density function to create region of interest
#   for creating absence points
sample.size = nrow(sdata)*3
absence <- pseudo.absence(sdata, sample.size, Mask = xvars[[1]], 
	                      sigma = "Scott", gradient = 0.6, KDE=TRUE)
  sample.kde <- absence$kde 
    absence <- absence$sample
      proj4string(absence) <- proj4string(sdata)
	  
  #### Combine null and sdata data and extract raster values
  presence <- sdata
    presence@data <- data.frame(id=1:nrow(presence), y = 1)
      absence@data <- data.frame(id = nrow(presence)+1:nrow(absence)+(nrow(presence)+1), y = 0)	  
    pres.abs <- rbind(presence, absence)		
      pres.abs@data <- data.frame(pres.abs@data, extract(xvars, pres.abs)  )
        na.idx <- unique(which(is.na(pres.abs@data), arr.ind=TRUE)[,1])
          if(length(na.idx) > 0) pres.abs <- pres.abs[-na.idx,]

######
# test multcollinearity using permutation
#   with leave-one-out	
( cl.test <- multi.collinear(pres.abs@data[,3:ncol(pres.abs)], perm = TRUE, 
                             leave.out = TRUE, n = 999, p = 0.05) )
  rm.vars <- cl.test[cl.test$frequency > 0,]$variables
   		       
# Remove identified variable(s)
if(length(rm.vars) > 0) 
  pres.abs@data <- pres.abs@data[,-which(names(pres.abs@data) %in% rm.vars)]

######
# test collinearity between biobclim variables
cl.vars <- collinear(pres.abs@data[,3:ncol(pres.abs)], p = 0.85, 
                     nonlinear = FALSE, p.value = 0.001)
# Remove collinear identified variable(s)
pres.abs@data <- pres.abs@data[,-which(names(pres.abs@data) %in% cl.vars)]

##############################################
##############################################
####### RANDOM FORESTS MODEL
##############################################
##############################################

# Number of Bootstrap replicates
b = 1001 

# Out SDM probability prediction tif
out.pred = "HimalayanBear_SDM.tif"

# Out plots
out.plots <- "HimalayanBear_SDM_plots.pdf"

# Out shapefile of training data with,
#  y, covariates and probabilities
out.shp <- "HimalayanBear_pa"  

##############################################
#### Specify probability random forest model 
####   with variable importance permutation 
y <- factor(pres.abs@data[,"y"])  
x <- pres.abs@data[,3:ncol(pres.abs)]  
 
( rf.sel <- rf.modelSel(xdata = x, ydata = y, imp.scale = "mir",
                      r = seq(0,1,0.2)[-c(1,6)], seed = 43278,
					  ntree = b) ) 
						
# ( sel.vars <- rf.sel$selvars )         # selected model	
( sel.vars <- rf.sel$parameters[[3]] )   # competing model 

# Importance
 imp <- rf.sel$importance[which(rownames(rf.sel$importance) %in% sel.vars),]
   imp.names <- rownames(rf.sel$importance)[which(rownames(rf.sel$importance) %in% sel.vars)]  
     idx <- order(imp)				
       imp <- imp[idx]
       imp.names <- imp.names[idx] 	
	
( rf.fit <- randomForest(x=x[,sel.vars], y=as.factor(y), ntree = b,
                         importance = TRUE, proximity = TRUE) )

##############################################
#### Optimize fit on prediction variance
probs <- data.frame(pres.abs@data[,c("id","y")], 
                    probs = predict(object=rf.fit, type="prob",  
					data=x[,sel.vars])[,2])
p = c(0.25, 0.50)  # -/+ probability thresholds
  idx <- vector()
    for(i in unique(probs$y)) {
      site <- probs[probs$y == i,] 
        if(unique(site$y) == 1) {
  	      rn <- which(rownames(probs) %in% rownames(site[which(site$probs <= p[2]),]))
        } else if(unique(site$y) == 0) {
  	      rn <- which(rownames(probs) %in% rownames(site[which(site$probs > p[1]),]))
  	    }
      if(length(rn) > 0) idx <- append(idx, as.numeric(rn))
    }	  

# Fit final optimized model and estimate probabilities
if(length(idx) > 10) {
  cat("Prediction variance optimization removing -", length(idx), 
      "observations", "\n")
  x.opt <- x[-idx,]
  y.opt <- y[-idx]  
  rf.fit <- randomForest(x=x.opt[,sel.vars], y=as.factor(y.opt), ntree = b,
                         importance = TRUE, proximity = TRUE)
    probs <- data.frame(pres.abs@data[-idx,][c(1,2)], 
                        probs = predict(object=rf.fit, type="prob", 
					    data=x.opt[,sel.vars])[,2])
  } else {
    cat("\n", "No optimization needed", "\n")
  }
  
##############################################  
##### Model validation of fit and performance
( validation <- accuracy(table(y.opt, rf.fit$predicted)) )
( mcv <- rf.crossValidation(rf.fit, xdata = x.opt[,sel.vars],
                            p=0.10, n = 99) )

# Probabilities and  global/local (observation-level) log loss  
#   and log likelihood loss  	
probs <- data.frame(pres.abs@data[,c("id","y")], 
                    probs = predict(object = rf.fit, type="prob",  
					                newdata = x[,sel.vars])[,2])							
  ll <- data.frame(log.loss=logLoss(y = probs[,"y"], 
				   p = probs[,"probs"], 
				   global = FALSE)$log.loss, 
                   log.like=logLoss(y = probs[,"y"], 
				   p = probs[,"probs"], 
				   likelihood = TRUE))
probs <- data.frame(probs, ll)

cat("Global log loss:", logLoss(y = probs[,"y"], p = probs[,"probs"]), "\n")

##############################################
# Output spatial data with y, probs, log loss,
#   and selected parameter set, write shapefile  	
pres.abs@data <- data.frame(probs, x[,sel.vars])

rgdal::writeOGR(pres.abs, getwd(), out.shp, driver="ESRI Shapefile",
                check_exists=TRUE, overwrite_layer=TRUE)

##############################################
# Make spatial predictions 	
sdm <- predict(xvars[[sel.vars]], rf.fit, filename = out.pred, 
               type="prob", index=1:2, overwrite=TRUE, 
			   options="COMPRESS=LZW", progress="window")

##############################################
##############################################  
#### Calculate standard error as measure of
####   spatial and estimate uncertainty
##############################################
##############################################
if(length(idx) > 10) {
  cat("Prediction variance optimization removing -", length(idx), 
      "observations", "\n")
  x.opt <- x[-idx,]
  y.opt <- y[-idx]  
( mdl <- ranger(x=x.opt[,sel.vars], y=as.factor(y.opt), probability = TRUE, 
                num.trees = 65, importance="permutation", 
				write.forest = TRUE, keep.inbag = TRUE) )
  } else {
( mdl <- ranger(x=x[,sel.vars], y=as.factor(y), probability = TRUE, 
                num.trees = 65, importance="permutation", 
				write.forest = TRUE, keep.inbag = TRUE) )
  }

sdm <- brick("HimalayanBear_SDM.tif")
  sdm.mask <- sdm[[2]]
    sdm.mask[sdm.mask < 0.05] <- NA

#### Standard error
predict.se <- function(model, data) {
  as.numeric(ranger:::predict.ranger(model, data = data,
    type = "se", se.method = "infjack")$se[,2])
}
sdm.se <- predict(xvars[[sel.vars]], mdl, fun=predict.se, 
                  filename = "sdm_se.tif")
	sdm.se <- mask(sdm.se, sdm.mask)			  
				  
# Upper and Lower Confidence interval
ci95 <- stack( sdm[[2]] - (sdm.se * 1.96),
               sdm[[2]] + (sdm.se * 1.96) )
  ci95 <- mask(ci95, sdm.mask)				   
    names(ci95) <- c("lower.ci", "upper.ci")

#### Entropy
# Entropy function
entropy.empirical <- function(y){
  entropy <- function(freqs){
    freqs <- freqs / sum(freqs)
    H = -sum( ifelse(freqs > 0, freqs*log(freqs), 0) )
    return(H)
  }
  return( entropy(y / sum(y)) )
}
estimate.entropy <- calc(sdm[[c(2,1)]], entropy.empirical)
  estimate.entropy <- mask(estimate.entropy, sdm.mask)

####**** Plots ****####
pdf("SDM_uncertanity.pdf", height=8.5, width=11)
  # 90% volume of probabilities
  sdm.pres <- mask(sdm[[2]], sdm.mask)
    sdm.vol <- raster.vol(sdm.pres, p = 0.70)
  plot(sdm.vol, col = c("azure4", "azure3"),
      main="80% habitat volume",
      legend=FALSE)
  cuts=seq(0,0.7,0.1) #set breaks
    pal <- colorRampPalette(c("darkcyan","yellow","red"))
      plot(sdm.se, breaks=cuts, col = pal(8),
        main="Standard Error (spatial uncertainty)") 
          points(sdata, pch=20, cex=0.5)
  plot(estimate.entropy, breaks=cuts, col = pal(8),
       main="Spatial uncertainty entropy") 
    points(sdata, pch=20, cex=0.5)
  plot(ci95[[2]], col = pal(10), 
       main="Upper 95% confidence interval")
    points(sdata, pch=20, cex=0.5)
  plot(ci95[[1]], col = rev(pal(10)), 
       main="Lower 95% confidence interval")
    points(sdata, pch=20, cex=0.5)
dev.off() 
 
##############################################  
#### Plot probabilities and partial plots for 
####   each parameter

pdf(out.plots, height=10, width=10)
  options(warn=-1)	 
  plot(sample.kde, main="Isotropic density of bear presence and pseudo-absence")
    plot(sdata, pch=19, cex=0.6, col="red", add=TRUE)
      plot(absence, pch=19, cex=0.60, add=TRUE)	
        legend("bottomright", bg="white", 
		       legend=c("Presence", "Pseudo-absence"),
               pch=c(19,19),col=c("black","red"))
  plot(rf.fit, main="Model error convergence")
  varImpPlot(rf.fit, type = 1, scale=TRUE, pch=20, 
             main="Variable Importance")		   
    for(r in rownames(importance(rf.fit))) {
      #cat("Calculating partial plot for parameter: ", r, "\n")  
	  pp <- partial(rf.fit, pred.var = r, plot = TRUE, which.class = 2, 
	          train = data.frame(y=y.opt, x.opt[,sel.vars]), 
			  levelplot = TRUE, chull = TRUE, quantiles = TRUE, 
			  smooth=TRUE, plot.engine = "ggplot2")
      print(pp)			  
    }
  MDSplot(rf.fit, y.opt, 
    main="Multidimensional scaling (MDS) of prediction space")	
  mypal <- c("cyan", "blue", "red3")
  hc <- classIntervals(probs[,"probs"], n=20, style="hclust", 
                         method="complete")
  h.col <- findColours(hc, mypal)
    plot(hc, mypal, main="Random Forests Prediction Function")
      legend(c(95, 155), c(0.12, 0.4), fill=attr(h.col, "palette"),
         legend=names(attr(h.col, "table")), bg="white")
    plot(sdm, main="Himalayan bear probabilities")
      plot(sdata, pch=19, cex=0.6, col="red", add=TRUE)
  options(warn=0)	  
dev.off()
	 
save.image(paste(getwd(), "Himalayan_bear_sdm.RData", sep="/"))

##############################################
#            **** EXAMPLES ****
##############################################

#### Optional 3D bivariate (variable interaction) 
####   partial plot using interactive rgl device
####   (rotate plot with mouse) 
library(rgl)
bvpd <- bivariate.partialDependence(rf.fit, x.opt[,sel.vars], 
              v1 = "rugosity27", v2 = "bio10_3", shade = 0.6, 
 			  which.class="1", plot = FALSE )
 nbcol = 100
 color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
 zcol  = cut(bvpd$estimate, nbcol)			   
   rgl::persp3d(x = bvpd$p1, y = bvpd$p2, z = bvpd$estimate, 
                col = color[zcol], xlab="rugosity27", 
				ylab="Isothermality", zlab="p")	 
 rgl::rgl.snapshot("bvpartial_Isothermality_rugosity27.png")
