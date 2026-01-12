############################################################
############################################################
# Tillage Model - probabilistic estimate of
# tillage suitability using NASS fro training data
# SSURGO soil data for stratification and climatic,
# geomorphometric and soil for model parameters.
#
# Output is a tiff stack with LZW compression contaning 4 rasters:
# (1) probabilities, (2) standard errors, (3 & 4) upper and 
# lower confidence intervals
# Projection is USGS Albers :
# "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 
#  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#
# Contact:
# Jeffrey S. Evans, Ph.D., (He, Him)
# Senior Landscape Ecologist & Biometrician 
# The Nature Conservancy | Protected Lands Science 
# Visiting Professor | University of Wyoming | Ecosystem Sciences
# Office, Agriculture - C6, Laramie, WY  82070
# jeffrey_evans@tnc.org | (970) 672-6766 
############################################################
############################################################
library(terra)
library(sp)
library(sf)
library(raster)
library(rgdal)
library(spatialEco)
library(exactextractr)
library(fasterRaster)
library(randomForest)
library(ranger)
library(pdp)
library(classInt)
library(ggplot2)
library(rfUtilities)	

# set environmnet
dir.create("D:/tmp/terra",showWarnings = FALSE)
  terraOptions(memfrac=0.25, tempdir="D:/tmp/terra")
dir.create("D:/tmp/raster",showWarnings = FALSE)
  rasterOptions(memfrac=0.25, tmpdir="D:/tmp/raster")
grassDir = "P:/Program Files/GRASS GIS 7.8"

ncpu = round(parallel::detectCores()/4,0)

root = "C:/evans/tillage"
code.path = file.path(root, "code")

source(file.path(root, "code", "fetch_ned.R"))
source(file.path(root, "code", "collinear.R"))
source(file.path(root, "code", "accuracy.R"))

usgs.prj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

climate.path = file.path(root, "climate")
soil.path = file.path(root, "soil")

#***********************************************
#***********************************************
# Start of state(st) - county(ct) model loop
#***********************************************
#***********************************************
states = list.dirs(path = file.path(root,"model"),
                   full.names = TRUE, 
                   recursive = FALSE)

#for(st in states[1:10]) {
#  states.list = states[1:10] 

#for(st in states[11:20]) {
#  states.list = states[11:20] 

#for(st in states[21:30]) {
#  states.list = states[21:30] 

for(st in states[31:40]) {
  states.list = states[31:40] 

#for(st in states[41:49]) { 
#  states.list = states[41:49] 

  state = basename(st)
  counties = list.dirs(path = st, full.names = TRUE, 
                       recursive = FALSE) 
  for(ct in counties) {
  	cat("\n", "Processing data for", ct, "\n") 
    setwd(ct)
	  if(file.exists("probs.tif")) { next } 
	dir.create(file.path(getwd(), "data"),
               showWarnings = FALSE)
	data.dir <- file.path(getwd(), "data")
    dir.create(file.path(getwd(), "tmp"), 
               showWarnings = FALSE)
    tmp.dir <- file.path(getwd(), "tmp")
	cbdy <- sf::st_read(getwd(), "bdy")
	  suppressWarnings({sf::st_crs(cbdy) <- sf::st_crs(usgs.prj)})
        m <- rast("mask.tif")
   
	# read nass data
	nass <- crop(terra::rast(file.path(root,"model",state,"nass.tif")),
	             ext(m))
	  nass <- terra::project(nass, m, method="near")
        nass <- mask(nass,m)    

    cat("processing spatial model parameters", "\n")
    #****************************************
    # processing topographic parameters  
	#   twi, trasp, slope, TPI, TRI,  
	#   roughness, scosa, diss, hli
    #****************************************		
    elev <- fetch_ned(cbdy, out.res = 13, out.dir = tmp.dir)
      elev <- terra::rast(list.files(tmp.dir, "tif$", full.names = TRUE))
        min.elev = min(elev[], na.rm=TRUE)
        max.elev = max(elev[], na.rm=TRUE)
        names(elev) <- "elev"
        elev <- terra::project(elev, m, method="bilinear")
		  elev <- terra::mask(elev, vect(cbdy))
            elev[elev < min.elev] <- min.elev
            elev[elev > max.elev] <- max.elev
            writeRaster(elev, file.path(data.dir, "elev.tif"), 
		                overwrite=TRUE, wopt=list(gdal=c("COMPRESS=LZW")) )
	if(!(min(elev[],na.rm=TRUE) == max(elev[],na.rm=TRUE))) {
      try({
        topo <- list()
	    topo[["elev"]] <- raster(elev)
	    topo[["twi"]] <- fasterTopidx(topo[["elev"]], grassDir=grassDir)
	    topo[["trasp"]] <- trasp(topo[["elev"]])
          geomorph <- terrain(topo[["elev"]], opt=c("slope", "aspect", "TPI", "TRI", "roughness"))
        topo[["slope"]] <- geomorph[[1]]
        topo[["tpi"]] <- geomorph[[2]] 
        topo[["tri"]] <- geomorph[[3]] 
        topo[["roughness"]] <- geomorph[[4]] 
        topo[["scosa"]] <- raster::overlay(geomorph[[4]], geomorph[[5]], fun = sa.trans)
	    topo[["hli"]] <- hli(topo[["elev"]])
	      rmin <- terra::focal(elev, w = 5, fun = min)
          rmax <- terra::focal(elev, w = 5, fun = max)
        topo[["diss"]] <- raster( (elev - rmin) / (rmax - rmin) )
      })
    }
    if(length(topo) > 0){
      for(tn in 1:length(topo)) {
        writeRaster(topo[[tn]], 
	    	        file.path(data.dir, paste0(names(topo)[tn], ".tif")),
	    			 overwrite=TRUE, options="COMPRESS=LZW")  
       } 
    }
    remove(elev, topo, rmin, rmax)  
      unlink(file.path(getwd(), "tmp"), recursive = TRUE)
    #****************************************
    # processing climate parameters
	# (map) Mean annual precipitation
	# (mat) Mean annual temperature
	# (gsp) Growing season precipitation, April to September
	# (ffp) Length of the frost-free period (days)
	# (dd5) Degree-days >5 degrees C (based on mean monthly temperature)
	# (smrp) Summer precipitation: (jul+aug) 
	# (smrsprpb) Summer/Spring precipitation balance: (jul+aug)/(apr+may)
	# (sprp) Spring precipitation: (apr+may)
	# (winp) Winter precipitation: (nov+dec+jan+feb)
	#****************************************
	map <- crop(rast(file.path(root,"climate", "map.tif")), ext(m))
	mat <- crop(rast(file.path(root,"climate", "mat.tif")), ext(m))
	  if(!(min(map[],na.rm=TRUE) == max(map[],na.rm=TRUE)) |
	     !(min(mat[],na.rm=TRUE) == max(mat[],na.rm=TRUE))) {
    try({
      climate <- list()
      climate[["map"]] <- mask(terra::resample(map, m),m)	  
      climate[["mat"]] <- mask(terra::resample(mat, m),m) 
      climate[["gsp"]] <- crop(rast(file.path(root,"climate", "gsp.tif")), ext(m))
        climate[["gsp"]] <- mask(terra::resample(climate[["gsp"]], m),m)   
      climate[["ffp"]] <- crop(rast(file.path(root,"climate", "ffp.tif")), ext(m))
        climate[["ffp"]] <- mask(terra::resample(climate[["ffp"]], m),m)  
      climate[["dd5"]] <- crop(rast(file.path(root,"climate", "dd5.tif")), ext(m))
        climate[["dd5"]] <- mask(terra::resample(climate[["dd5"]], m),m)  
      climate[["smrp"]] <- crop(rast(file.path(root,"climate", "smrp.tif")), ext(m))
        climate[["smrp"]] <- mask(terra::resample(climate[["smrp"]], m),m)  
      climate[["smrsprpb"]] <- crop(rast(file.path(root,"climate", "smrsprpb.tif")), ext(m))
        climate[["smrsprpb"]] <- mask(terra::resample(climate[["smrsprpb"]], m),m)  
      climate[["sprp"]] <- crop(rast(file.path(root,"climate", "sprp.tif")), ext(m))
        climate[["sprp"]] <- mask(terra::resample(climate[["sprp"]], m),m)  
      climate[["winp"]] <- crop(rast(file.path(root,"climate", "winp.tif")), ext(m))
        climate[["winp"]] <- mask(terra::resample(climate[["winp"]], m),m)  
      })
    }
    if(length(climate) > 0){
      for(cn in 1:length(climate)) {
        writeRaster(climate[[cn]], 
	    	        file.path(data.dir, paste0(names(climate)[cn], ".tif")),
	    			 overwrite=TRUE, options="COMPRESS=LZW")  
      }   
    }
    remove(map, mat, climate)
      gc()
    #****************************************
    # processing soil parameters	
    #****************************************
    try({
      soil <- list()
	aws <- terra::crop(rast(file.path(soil.path,"AvailableWaterSupply.tif")),ext(extent(cbdy)))
	  aws <- terra::project(aws, m, method="bilinear", mask=TRUE)
	    soil[["Available_Water"]] <- terra::mask(aws, vect(cbdy))
	dpt <- terra::crop(rast(file.path(soil.path,"DepthRestrictive.tif")),ext(extent(cbdy)))
	  dpt <- terra::project(dpt, m, method="bilinear", mask=TRUE)
	    soil[["Restrictive_Depth"]] <- terra::mask(dpt, vect(cbdy))
	hc <- terra::crop(rast(file.path(soil.path,"Hydraulic_Conductivity.tif")),ext(extent(cbdy)))
	  hc <- terra::project(hc, m, method="bilinear", mask=TRUE)
	    soil[["Hydraulic_Conductivity"]] <- terra::mask(hc, vect(cbdy))
	nccp <- terra::crop(rast(file.path(soil.path,"NCCPI.tif")),ext(extent(cbdy)))
	  nccp <- terra::project(nccp, m, method="bilinear", mask=TRUE)
	    soil[["cpi"]] <- terra::mask(nccp, vect(cbdy))   
 	om <- terra::crop(rast(file.path(soil.path,"OrganicMatter.tif")),ext(extent(cbdy)))
	  om <- terra::project(om, m, method="bilinear", mask=TRUE)
	    soil[["Organic_Matter"]] <- terra::mask(om, vect(cbdy))
  	pc <- terra::crop(rast(file.path(soil.path,"PercentClay.tif")),ext(extent(cbdy)))
	  pc <- terra::project(pc, m, method="bilinear", mask=TRUE)
	    soil[["Pct_Clay"]] <- terra::mask(pc, vect(cbdy))
  	ps <- terra::crop(rast(file.path(soil.path,"PercentSand.tif")),ext(extent(cbdy)))
	  ps <- terra::project(ps, m, method="bilinear", mask=TRUE)
	    soil[["Pct_Sand"]] <- terra::mask(ps, vect(cbdy))
  	ph <- terra::crop(rast(file.path(soil.path,"pH30cm.tif")),ext(extent(cbdy)))
	  ph <- terra::project(ph, m, method="bilinear", mask=TRUE)
	    soil[["ph"]] <- terra::mask(ph, vect(cbdy))
  	wc <- terra::crop(rast(file.path(soil.path,"Water_Capacity30cm.tif")),ext(extent(cbdy)))
	  wc <- terra::project(wc, m, method="bilinear", mask=TRUE)
	    soil[["Water_Capacity"]] <- terra::mask(wc, vect(cbdy))
      })
    if(length(soil) > 0){
      for(sn in 1:length(soil)) {
        writeRaster(soil[[sn]], 
	    	        file.path(data.dir, paste0(names(soil)[sn], ".tif")),
	    			 overwrite=TRUE, options="COMPRESS=LZW")  
      } 
    }  
    remove(soil, aws, dpt, hc, nccp, om, pc, ps, ph, wc)
      gc()
    #****************************************	
    #****************************************
    #              Tillage model	
    #****************************************
    #****************************************
  	b = 1001 # Number of Bootstrap replicates
	
  	cat("\n", "Running tillage model for", ct, "\n") 
      dir.create(file.path(getwd(), "results"),
                 showWarnings = FALSE)
	  results.dir <- file.path(getwd(), "results")
    #****************************************
    # Check to see if any parameters were created, if not
	# write a zero probability raster
	#****************************************
	vnames = c("twi", "trasp", "tri", "tpi", "roughness",
	           "slope", "scosa", "diss", "hli",
               "map", "mat", "gsp", "ffp", "dd5", "smrp", 
	            "smrsprpb", "sprp", "winp", "elev", 	
	            "Water_Supply", "Restrictive_Depth", "Hydraulic_Conductivity",
                "NCCPI", "Organic_Matter", "Pct_Clay", "Pct_Sand", "PH",
                "Water_Capacity")		   
	parms <- na.omit(vnames[grep(paste(vnames,collapse="|"), list.files(data.dir, "tif$"))])			
	  if(length(parms) < 3) {
	    write(paste0(ct, "  reason: failed to create any parameters"), 
	          file.path(root,"model_fail.txt"), 
	          append = TRUE)
	    m[m==1] <- 0
        writeRaster(m, "probs.tif", overwrite=TRUE, 
      	  		  options="COMPRESS=LZW")        		
        next			  
	  }
    #****************************************
    # read raster data and assign to training
    #****************************************
    # Read and assign y data to sample data	
	train <- st_read("sample.shp")
      suppressWarnings({sf::st_crs(train) <- sf::st_crs(usgs.prj)})
    train$y  <- extract(nass, vect(train))[,2]
	  na.idx <- which(is.nan(train$y))
    if(length(na.idx) > 1) train <- train[-na.idx,]

	# assign parameter values, remove columns > 30% NA
	r <- rast(list.files(data.dir, "tif$", full.names=TRUE))
      train <- data.frame(train, extract(r, vect(train)))
        na.col.idx <- vector()
          for(cl in 1:ncol(train)) {
            p=length(which(is.na(train[,cl]))) / nrow(train)
            if(p > 0.3) na.col.idx <- append(na.col.idx, cl)
          }
      if(length(na.col.idx)>0) train <- train[,-na.col.idx]
	    train <- na.omit(train)
	      train <- train[,c(3,5:ncol(train))]
		    train$y <- ifelse(train$y > 1, 1, train$y)

    # remove invariant data
	rm.vars <- vector()
	  for(rv in 1:ncol(train)) {
	    if(var(train[,rv], na.rm=TRUE) == 0.0000001){ 
		  rm.vars <- append(rm.vars, names(train)[rv])
		}
	  }
      if(length(rm.vars) > 0) 
        train <- train[,-which(names(train) %in% rm.vars)]

	#****************************************
    # test multcollinearity using permutation
    #   with leave-one-out	
     try({
    ( cl.test <- multi.collinear(train[,2:ncol(train)], perm = TRUE, 
                                 leave.out = TRUE, n = 99) )
      rm.vars <- cl.test[cl.test$frequency > 0,]$variables
        if(length(rm.vars) > 0) 
          train <- train[,-which(names(train) %in% rm.vars)]
    })
    #****************************************
    # test collinearity 
     try({
      ( cl.vars <- collinear(train[,2:ncol(train)], p = 0.70, 
                             nonlinear = FALSE, p.value = 0.001) )
	  if(length(cl.vars) > 0) 					   
        train <- train[,-which(names(train) %in% cl.vars)]
    })
    #****************************************
    # model selection and fit
    try({
      rand.seed = 4327  	
      y <- factor(train[,"y"])  
      x <- train[,2:ncol(train)]  
      rf.sel <- rf.modelSel(xdata = x, ydata = y, imp.scale = "mir",  
                            r = seq(0,1,0.2)[-c(1,6)], seed = rand.seed,
      					  num.trees = b) 			
        ( sel.vars <- rf.sel$selvars )  
      ( rf.fit <- ranger(x = x[,sel.vars], y = as.factor(y), probability = TRUE, 
                         num.trees = b, importance="permutation") )
    })
    
    if(exists("rf.fit")) {
	  idx <- order(rf.fit$variable.importance, decreasing = FALSE)
	    imp <- rf.fit$variable.importance[idx]
    	
    #****************************************
    # Back-prediction
	probs <- data.frame(y = y, probs = as.numeric(ranger:::predict.ranger(rf.fit,  
                        data = x[,sel.vars], verbose = FALSE)$predictions[,2]))	

    #****************************************
    # Model validation of fit and performance, global log loss
    v <- accuracy(table(y, ifelse(rf.fit$predictions[,2] < 0.65, 0, 1)))
    ll <- logLoss(y = y, p = rf.fit$predictions[,2])
	  sink(file.path(ct, "results", "validation.txt"))
	    cat("Global log loss:", ll, "\n")
        cat("Accuracy (PCC):", v[["PCC"]], "\n")
        cat("Kappa:", v[["kappa"]], "\n")
        cat("Area under the ROC curve:", v[["auc"]], "\n") 
        cat("True Skill statistic:", v[["true.skill"]], "\n") 
        cat("Sensitivity (aka recall):", v[["sensitivity"]], "\n") 
        cat("Specificity:", v[["specificity"]], "\n") 
        cat("Gain (aka precision):", v[["gain"]], "\n") 
        cat("Positive Likelihood Ratio:", v[["plr"]], "\n") 
        cat("Negative Likelihood Ratio:", v[["nlr"]], "\n")
        cat("Type I error:", v[["typeI.error"]], "\n")
        cat("Type II error:", v[["typeII.error"]], "\n") 
        cat("Gini Index:", v[["gini"]], "\n") 
        cat("F-score:", v[["f.score"]], "\n")
      sink()
	#( mcv <- rf.crossValidation(rf.fit,  p=0.10, n = 99) )
  
    #****************************************
    # Make spatial probabilistic predictions	
    r <- rast(list.files(data.dir, "tif$", full.names = TRUE))	
      idx <- which(names(r) %in% names(rf.fit$variable.importance))
        r <- r[[idx]]
    rnames <- names(r)
      r <- stack(r)
        names(r) <- rnames
	predict.prob <- function(model, data) {
      as.numeric(ranger:::predict.ranger(model,  
                 data = data, verbose = FALSE)$predictions[,2])
    }  
	p <- raster::predict(r, rf.fit, fun = predict.prob, 
	                     progress="window")
      p <- mask(crop(p, extent(raster(m))), raster(m))

    #****************************************
    # estimate uncertainty
    se.mdl <- ranger(x=x[,sel.vars], y=as.factor(y), probability = TRUE, 
                    num.trees = 99, importance="permutation", 
				    write.forest = TRUE, keep.inbag = TRUE)
    # Standard error
    predict.se <- function(model, data) {
      as.numeric(ranger:::predict.ranger(model, data = data,
                 type = "se", se.method = "infjack")$se[,2])
    }
    sdm.se <- predict(r, se.mdl, fun=predict.se, progress="window")
      sdm.se <- mask(crop(sdm.se, extent(raster(m))), raster(m))
    # Upper and Lower Confidence interval
    ci95 <- stack( p - (sdm.se * 1.96),
                   p + (sdm.se * 1.96) )
      ci95 <- mask(ci95, raster(m))				   
        names(ci95) <- c("lower.ci", "upper.ci")

    # write probabilities, standardar error, and confidence intervels
	  writeRaster(stack(p, sdm.se, ci95), "probs.tif",
    	  		  overwrite=TRUE, options="COMPRESS=LZW")     
    #****************************************
    # Plot probabilities and partial plots for 
    # each parameter
    pdf(file.path(results.dir, "model_plots.pdf"), height=10, width=10)	
        dotchart(imp, names(imp), pch=19, 
                 main="Variable Importance") 
      pal <- colorRampPalette(c("darkcyan","yellow","red"))
      plot(p, col = pal(10),
        main="Tillage probabilities") 
      plot(raster.vol(p, p = 0.70), col = c("azure3", "azure4"),
           main="80% prediction volume", legend=FALSE)		 
      plot(sdm.se, col = pal(10),
        main="Standard Error (spatial uncertainty)") 
      plot(ci95[[2]], col = pal(10), 
           main="Upper 95% confidence interval")
      plot(ci95[[1]], col = rev(pal(10)), 
           main="Lower 95% confidence interval")
      mypal <- c("cyan", "blue", "red3")
        hc <- classIntervals(probs[,"probs"], n=20, style="quantile", 
                             method="complete")
      h.col <- findColours(hc, mypal)
        plot(hc, mypal, main="Random Forests Logistic Function")
          legend(c(95, 155), c(0.12, 0.4), fill=attr(h.col, "palette"),
             legend=names(attr(h.col, "table")), bg="white")	 
       for(rn in names(rf.fit$variable.importance)) {
          cat("Calculating partial plot for parameter: ", rn, "\n")
            
    	  pp <- partial(rf.fit, pred.var = rn, plot = TRUE, which.class = 2, 
    	                train = data.frame(y = y, x), levelplot = TRUE,
    			        chull = TRUE, quantiles = TRUE, smooth=TRUE,
    			        plot.engine = "ggplot2")
          suppressWarnings(print(pp))
        }	  
    dev.off()
      remove(p, se.mdl, sdm.se, ci95, r, x, y) 
        gc()
    } else {
	  write(paste0(ct, "  reason: failed to execute model"), 
	        file.path(root,"model_fail.txt"), 
	        append = TRUE)
    }
    if(exists("rf.fit") && file.exists("probs.tif")){
	   write(paste(state, basename(ct), sep=" - "), 
	         file.path(root,"model_sucess.txt"), 
	         append = TRUE)
    }
	unlink(file.path(getwd(), "data"), recursive = TRUE)
      save.image(file.path(ct, "tillage_model.RData"))
    suppressWarnings({ 
      unlink(list.files(tempdir(), include.dirs = TRUE, 
             full.names = TRUE, recursive = TRUE),
             recursive = TRUE)
    })
  } # end county loop
} # end state loop  
