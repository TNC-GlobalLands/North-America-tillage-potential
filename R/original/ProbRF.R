##########################################
# SDM for Himalayan brown bear

#**** Only run once
p = utils::installed.packages()[,1]
if(!"devtools" %in% p)
  install.packages("devtools")
remotes::install_github("jeffreyevans/rfUtilities", force = TRUE)
#****

library(rfUtilities)
library(randomForest)
library(ranger)
library(sp)
library(raster)
library(rgdal)
library(spatialEco)
library(pdp)
library(classInt)
library(ggplot2)

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
	                      sigma = "Stoyan", gradient = 0.6, KDE=TRUE)
  sample.kde <- absence$kde 
    absence <- absence$sample
      proj4string(absence) <- proj4string(sdata)
	  
    # Plot kde sample weights 
    plot(sample.kde, main="Isotropic density of Brown bear presence and pseudo-absence")
      plot(sdata, pch=19, cex=0.5, col="red", add=TRUE)
        plot(absence, pch=19, cex=0.40, add=TRUE)	

    #### Combine null and sdata data and extract raster values
	presence <- sdata
	  presence@data <- data.frame(id=1:nrow(presence), y = 1)
    absence@data <- data.frame(id = nrow(presence)+1:nrow(absence)+(nrow(presence)+1), 
	                           y = 0)	  
    pres.abs <- rbind(presence, absence)		
      pres.abs@data <- data.frame(pres.abs@data, extract(xvars, pres.abs)  )
        na.idx <- unique(which(is.na(pres.abs@data), arr.ind=TRUE)[,1])
          if(length(na.idx) > 0) pres.abs <- pres.abs[-na.idx,]
    dim(pres.abs)
	str(pres.abs@data) 

######
# test collinearity between biobclim variables
cl.vars <- collinear(pres.abs@data[,3:ncol(pres.abs)], p = 0.85, 
                     nonlinear = FALSE, p.value = 0.001)
# Remove collinear identified variable(s)
pres.abs@data <- pres.abs@data[,-which(names(pres.abs@data) %in% cl.vars)]

######
# test multcollinearity using permutation
#   with leave-one-out	
( cl.test <- multi.collinear(pres.abs@data[,3:ncol(pres.abs)], perm = TRUE, 
                             leave.out = TRUE, n = 999, p = 0.05) )
  rm.vars <- cl.test[cl.test$frequency > 0,]$variables
   		       
# Remove identified variable(s)
if(length(rm.vars) > 0) 
  pres.abs@data <- pres.abs@data[,-which(names(pres.abs@data) %in% rm.vars)]

################################
####### RANDOM FORESTS MODEL

b = 1001 # Number of Bootstrap replicates

##############################################
#### Specify probability random forest model 
####   with variable importance permutation 
y <- factor(pres.abs@data[,"y"])  
x <- pres.abs@data[,3:ncol(pres.abs)]  
 
rf.sel <- rf.modelSel(xdata = x, ydata = y, imp.scale = "mir", pvalue=0.05, 
                      r = seq(0,1,0.2)[-c(1,6)], seed = 43278,
					  probability = TRUE, num.trees = b,  
					  method = "Wright") 
						
( sel.vars <- rf.sel$selvars )             # selected model	
# ( sel.vars <- rf.sel$parameters[[9]] )   # competing model for m = 1

# Importance
 imp <- rf.sel$importance[which(rownames(rf.sel$importance) %in% sel.vars),]
   imp.names <- rownames(rf.sel$importance)[which(rownames(rf.sel$importance) %in% sel.vars)]  
     idx <- order(imp)				
       imp <- imp[idx]
       imp.names <- imp.names[idx] 	
	
( rf.final <- ranger(x=x[,sel.vars], y=as.factor(y), probability = TRUE, 
                     num.trees = b, importance="permutation") )

##############################################
#### Optimize fit on prediction variance
probs <- data.frame(pres.abs@data[,c("id","y")], 
                    probs = predict(object=rf.final, 
					data=x[,sel.vars])$predictions[,2])

p=c(0.25, 0.50)  # probability thresholds
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
if(length(idx) > 0) {
  cat("Prediction variance optimization removing -", length(idx), 
      "observations", "\n")
  x.opt <- x[-idx,]
  y.opt <- y[-idx]  
( rf.final <- ranger(x=x.opt[,sel.vars], y=as.factor(y.opt), probability = TRUE, 
                     num.trees = 1001, importance="permutation") )
    probs <- data.frame(pres.abs@data[-idx,][c(1,2)], 
                        probs = predict(object=rf.final, 
					    data=x.opt[,sel.vars])$predictions[,2])
  } else {
    x.opt <- x[,sel.vars]
	y.opt <- y
    cat("\n", "No optimization needed", "\n")
  }
  
##############################################  
##### Model validation of fit and performance
( validation <- accuracy(table(y.opt, ifelse(rf.final$predictions[,2] < 0.50, 0, 1))))
( mcv <- rf.crossValidation(rf.final,  p=0.10, n = 99) )
							
# Global and local log loss, log likelihood loss  	
  logLoss(y = y.opt, p = probs[,"probs"])    
  ll <- data.frame(p = probs[,"probs"], 
                   log.loss=logLoss(y = y.opt, p = probs[,"probs"], global = FALSE)$log.loss, 
                   log.like=logLoss(y = y.opt, p = probs[,"probs"], likelihood = TRUE))
  ll <- ll[order(ll$p),] 							

##############################################
# Join with spatial data and create plots 	
pres.abs <- merge(pres.abs, probs, by = "id")
  pres.abs@data <- pres.abs@data[,-which(names(pres.abs) %in% "y.y")]

# Write shapefile
rgdal::writeOGR(plots, getwd(), out.shp, driver="ESRI Shapefile",
                check_exists=TRUE, overwrite_layer=TRUE)

##############################################
# Make spatial predictions 	
rf.pred <- randomForest(x=x.opt[,sel.vars], y=factor(y.opt),  
                        ntree = b, importance=TRUE,
						proximity = TRUE) 
sdm <- predict(xvars[[sel.vars]], rf.pred, 
               filename="BrownBear_SDM.tif", 
               type="prob", index=2, overwrite=TRUE, 
			   options="COMPRESS=LZW", progress="window")

##############################################  
#### Plot probabilities and partial plots for 
####   each parameter
pdf("sdm_plots.pdf", height=10, width=10)	
  plot(rf.pred, main="Model error convergence")
  idx <- order(rf.final$variable.importance)
  dotchart(rf.final$variable.importance[idx], 
           names(rf.final$variable.importance)[idx],
           pch=19, main="Variable Importance")
    for(r in names(rf.final$variable.importance)) {
      cat("Calculating partial plot for parameter: ", r, "\n")  
	  pp <- partial(rf.final, pred.var = r, plot = TRUE, which.class=2, 
	          train = data.frame(y=y.opt, x.opt), levelplot = TRUE,
			  chull = TRUE, quantiles = TRUE, smooth=TRUE,
			  plot.engine = "ggplot2")
      print(pp)			  
    }
  mypal <- c("cyan", "blue", "red3")
    hc <- classIntervals(probs[,"probs"], n=20, style="hclust", 
                         method="complete")
  h.col <- findColours(hc, mypal)
    plot(hc, mypal, main="Random Forests Logistic Function")
      legend(c(95, 155), c(0.12, 0.4), fill=attr(h.col, "palette"),
         legend=names(attr(h.col, "table")), bg="white")	  
dev.off()
	 
save.image(paste(getwd(), "brown_bear_sdm.RData", sep="/"))

##############################################
##############################################
#### Optional 3D bivariate (interaction) partial 
####   plot using rgl device
library(rgl)

( bvpd <- bivariate.partialDependence(rf.pred, x.opt[,sel.vars], 
              v1 = "bio10_1", v2 = "distcrop_natrveg", shade = 0.6, 
 			  which.class="1", plot = FALSE ) )
 nbcol = 100
 color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
 zcol  = cut(bvpd$estimate, nbcol)			   
   rgl::persp3d(x=bvpd$p1, y=bvpd$p2, z=bvpd$estimate, 
                col = color[zcol], xlab="bio01", ylab="distcrop_natrveg", zlab="p")	 
 rgl::rgl.snapshot("bvpartial_cv_mode.png")
