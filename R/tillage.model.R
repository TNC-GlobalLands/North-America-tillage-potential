############################################################st_crs(2163
############################################################
# Tillage Model - probabilistic estimate of
# tillage suitability using NASS fro training data
# SSURGO soil data for stratification and climatic,
# geomorphometric and soil for model parameters.
#
# Output is a tiff stack with LZW compression contaning 4 rasters:
# (1) probabilities, (2) standardar errors, (3 & 4) upper and 
# lower confidence intervels
# Projection is USGS Albers (EPSG:5070):
#   "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 
#    +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#
# Contact:
# Jeffrey S. Evans, Ph.D.,
# Landscape Ecologist & Biometrician 
# SAGE Insights (Spatial Analysis and Global Ecology)
# Visiting Professor | University of Wyoming | Ecosystem Sciences
# Office, Agriculture - C6, Laramie, WY  82070
# sage_insights@outlook.com 
############################################################
# Check and add required libraries
############################################################
pkg <- c("rfUtilities", "terra", "sf", "spatialEco", "randomForest", 
         "ranger", "rstac", "gpkg", "pdp", "devtools")
install.packages(setdiff(pkg[-1], rownames(installed.packages())))  		 
devtools::install_version("rfUtilities", version = "2.1-5", 
                          repos = "http://cran.us.r-project.org")
    lapply(pkg, require, character.only = TRUE)

#*************************************************
# parameter and output switches
ncpu = round(parallel::detectCores()/4,0) # numberr of processors
boot.crossvalidate = c(TRUE,FALSE)[1]     # run bootstrap cross-validation
cv.boot = 99                              # how many iterations in cross-validation
sample.prop = 0.10                        # sample proportion of Bootstrap (based on % of minority class)
optimize.preds = c(TRUE,FALSE)[1]         # run log loss model optimization
prob.raster = c(TRUE,FALSE)[1]            # write probability raster (predictions)
uncertanity = c(TRUE,FALSE)[1]            # calculate and report model uncertanity
uncertanity.raster = c(TRUE,FALSE)[2]     # calculate and write uncertanity raster(s)
plot.partial = c(TRUE,FALSE)[2]           # Plot partial plots for each parameter

# Validation output flat (csv) files
validation.file = "validation_fit.csv"
cross.validation.file = "cross_validation_performance.csv"

#*************************************************
# set working directory environmnet
dir.create("C:/temp/terra",showWarnings = FALSE)
  terraOptions(tempdir="D:/tmp/terra")

root <- "D:/tillage"
  source(file.path(root, "code", "multi.collinear.R"))
  source(file.path(root, "code", "accuracy.R"))
climate.path = file.path(root, "climate")
soil.path = file.path(root, "soil")

usgs.prj <- st_crs("EPSG:5070")

#*************************************************
# Configure STAC service, returns DEM COG asset connection
olm <- read_stac("http://s3.eu-central-1.wasabisys.com/stac/openlandmap/catalog.json")
  olm$links <- links(olm, rel == "child")
     glc_link <- links(olm, grepl("DEM", title))[[1]]
    glc <- link_open(glc_link)
  glc_items <- read_items(glc, progress = FALSE)
urls <- assets_url(glc_items, asset_names = "dsm_glo30_m_30m_s", append_gdalvsi = TRUE)

#*************************************************
#*************************************************
# Start of state(st) - county(ct) model loop
#*************************************************
#*************************************************
states <- basename(list.dirs(path = file.path(root, "model"),
               full.names = TRUE, recursive = FALSE))
d <- list.dirs(path = file.path(root, "model"),
          full.names = TRUE, recursive = TRUE)
  d <- d[-which(stringr::str_detect(d, "results"))]
  
  # this removes base directory and is dependent on original root path
  d = d[unlist(lapply(strsplit(d, "/"), \(i) {
      ifelse(length(i) == 5, TRUE, FALSE)
    }))]
 
for(j in 1:length(d)) {
  ct = d[j]
  state = unlist(strsplit(ct, "/"))[4]
  county = unlist(strsplit(ct, "/"))[5]
  	cat("\n", "Processing data for", ct, "\n")
      flush.console(); Sys.sleep(0.01)			
      setwd(ct)
   if(file.exists("probs.tif")) { next } 
	dir.create(file.path(getwd(), "data"),
               showWarnings = FALSE)
	data.dir <- file.path(getwd(), "data")

    # read mask, nass, boundary and traning data from GeoPack	
	train <- st_read("data.gpkg", "sample", quiet=TRUE)
    m <- rast("data.gpkg", opt="BAND_COUNT=1")[[1]]   
    cbdy <- sf::st_read("data.gpkg", "bdy", quiet=TRUE)
    nass <- rast("data.gpkg", opt="BAND_COUNT=1")[[2]]   
	
	##  read nass data
	#nass <- crop(terra::rast(file.path(root,"model", state, "nass.tif")), ext(m))
	#  crs(nass) <- crs(cbdy)
	#    nass <- terra::project(nass, m, method="near")
    #      nass <- mask(nass,m)    

    cat("processing spatial model parameters", "\n")
	  flush.console(); Sys.sleep(0.01)		
    #****************************************
    # processing topographic parameters  
	#   twi, trasp, slope, TPI, TRI,  
	#   roughness, scosa, diss, hli
    #****************************************		
    mask.raster <- terra::rasterize(terra::vect(cbdy), m)
     crs(mask.raster) <- crs(cbdy)
   
	# Creates a geographic projection crop extent then
    # connects to DEM COG, crop and reproject to study aarea 
    e <- ext(st_transform(cbdy, st_crs(4326)))
    elev <- rast(urls, vsi = TRUE, raw = TRUE) 
      elev <- crop(elev, e)
        elev <- mask(project(elev, mask.raster, method = "bilinear"), cbdy)
          names(elev) <- "elevation" 
    writeRaster(elev, file.path(data.dir, "elev.tif"), gdal=c("COMPRESS=LZW"), 
                datatype="FLT4S", overwrite = TRUE)
    
	if(!(min(elev[],na.rm=TRUE) == max(elev[],na.rm=TRUE))) {
	  topo <- list()
      try({ 
	    topo[["elev"]] <- elev	
		elev.cor <- flowdem::fill(flowdem::breach(topo[["elev"]]), epsilon = TRUE) 
          fd <- mask(terra::terrain(elev.cor, v = "flowdir"), topo[["elev"]])
            fa <- mask((terra::flowAccumulation(fd) + 1) * 30, topo[["elev"]])
          slope <- terra::terrain(elev.cor, "slope", unit="radians")
            slope <- ifel(slope <= 0, 0.001, slope)
          topo[["twi"]] <- log(fa / tan(slope), base=2.71828)
	    topo[["trasp"]] <- trasp(topo[["elev"]])
        geomorph <- terrain(topo[["elev"]], v=c("slope", "aspect", "TPI", "TRI", "roughness"))
          topo[["slope"]] <- geomorph[[1]]
		  topo[["aspect"]] <- geomorph[[2]] 
          topo[["tpi"]] <- geomorph[[3]] 
          topo[["tri"]] <- geomorph[[4]] 
          topo[["roughness"]] <- geomorph[[5]] 
        topo[["scosa"]] <- terra::lapp(c(geomorph[[1]], geomorph[[2]]), fun = sa.trans)
	    topo[["hli"]] <- hli(topo[["elev"]])
	      rmin <- terra::focal(topo[["elev"]], w = 5, fun = min)
          rmax <- terra::focal(topo[["elev"]], w = 5, fun = max)
        topo[["diss"]] <- (topo[["elev"]] - rmin) / (rmax - rmin)
		remove(elev.cor, fd,fa, slope, rmin, rmax, geomorph)
      })
    }
    for(l in 1:length(topo)){ names(topo[[l]]) <- names(topo)[l] }
    if(length(topo) > 0){
      for(tn in 2:length(topo)) {
        writeRaster(topo[[tn]], file.path(data.dir, paste0(names(topo)[tn], ".tif")),
	    			overwrite=TRUE)  
       } 
    }

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
	  crs(map) <- crs(cbdy)
	mat <- crop(rast(file.path(root,"climate", "mat.tif")), ext(m))
	    crs(mat) <- crs(cbdy)

	if(!(min(map[],na.rm=TRUE) == max(map[],na.rm=TRUE)) |
	   !(min(mat[],na.rm=TRUE) == max(mat[],na.rm=TRUE))) {
    try({
      climate <- list()
	  climate[["map"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), map)$downscale , m)
	  climate[["mat"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), mat)$downscale , m)
	  gsp <- crop(rast(file.path(root,"climate", "gsp.tif")), ext(m)) 
	    crs(gsp) <- crs(cbdy)
		  climate[["gsp"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), gsp)$downscale , m) 
	  ffp <- crop(rast(file.path(root,"climate", "ffp.tif")), ext(m)) 
	    crs(ffp) <- crs(cbdy)
		  climate[["ffp"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), ffp)$downscale , m) 	   
	  dd5 <- crop(rast(file.path(root,"climate", "dd5.tif")), ext(m)) 
	    crs(dd5) <- crs(cbdy)
		  climate[["dd5"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), dd5)$downscale , m)	  
	  smrp <- crop(rast(file.path(root,"climate", "smrp.tif")), ext(m)) 
	    crs(smrp) <- crs(cbdy)
		  climate[["smrp"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), smrp)$downscale , m)	  	  
	  smrsprpb <- crop(rast(file.path(root,"climate", "smrsprpb.tif")), ext(m)) 
	    crs(smrsprpb) <- crs(cbdy)
		  climate[["smrsprpb"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), smrsprpb)$downscale , m)	  	  
	  sprp <- crop(rast(file.path(root,"climate", "sprp.tif")), ext(m)) 
	    crs(sprp) <- crs(cbdy)
		  climate[["sprp"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), sprp)$downscale , m)	  	  
	  winp <- crop(rast(file.path(root,"climate", "winp.tif")), ext(m)) 
	    crs(winp) <- crs(cbdy)
		  climate[["winp"]] <- mask( aspline.downscale(c(topo[["elev"]], topo[["slope"]], topo[["aspect"]]), winp)$downscale , m)	  	  
	  })
    }
	for(l in 1:length(climate)){ names(climate[[l]]) <- names(climate)[l] }
    if(length(climate) > 0){
      for(cn in 1:length(climate)) {
        writeRaster(climate[[cn]], file.path(data.dir, paste0(names(climate)[cn], ".tif")), overwrite=TRUE)  
      }   
    }
      remove(map, mat, gsp, ffp, dd5, smrp, smrsprpb, sprp, winp, climate)
    gc()
	  
    #****************************************
    # processing soil parameters	
    #****************************************
    soil <- list()
	try({
    aws <- terra::crop(rast(file.path(soil.path,"Available_Water.tif")), ext(m))
	  crs(aws) <- crs(cbdy)
	    soil[["Available_Water"]] <- mask(project(aws, m, method="bilinear"), m)      
	dpt <- terra::crop(rast(file.path(soil.path,"Restrictive_Depth.tif")), ext(m))
	  crs(dpt) <- crs(cbdy)
	    ext(dpt) <- ext(m)
		  soil[["Restrictive_Depth"]] <- mask(project(dpt, m, method="bilinear"), m)
	hc <- terra::crop(rast(file.path(soil.path,"Hydraulic_Conductivity.tif")), m)
	  crs(hc) <- crs(cbdy)
	    ext(hc) <- ext(m)
	      soil[["Hydraulic_Conductivity"]] <- terra::mask(hc, m)
	nccp <- terra::crop(rast(file.path(soil.path,"cpi.tif")), ext(m))
	  crs(nccp) <- crs(cbdy)
	    ext(nccp) <- ext(m)
		  soil[["cpi"]] <- mask(project(nccp, m, method="bilinear"), ext(m))
	om <- terra::crop(rast(file.path(soil.path,"Organic_Matter.tif")), ext(m))
	  crs(om) <- crs(cbdy)
	    ext(om) <- ext(m)
		  soil[["Organic_Matter"]] <- mask(project(om, m, method="bilinear"), m)
	pc <- terra::crop(rast(file.path(soil.path,"Pct_Clay.tif")), ext(m))
	  crs(pc) <- crs(cbdy)
	    ext(pc) <- ext(m)
		  soil[["Pct_Clay"]] <- mask(project(pc, m, method="bilinear"), m)
	ps <- terra::crop(rast(file.path(soil.path,"Pct_Sand.tif")), ext(m))
	  crs(ps) <- crs(cbdy)
	    ext(ps) <- ext(m)
		  soil[["Pct_Sand"]] <- mask(project(ps, m, method="bilinear"), m)
	ph <- terra::crop(rast(file.path(soil.path,"ph.tif")), ext(m))
	  crs(ph) <- crs(cbdy)
	    ext(ph) <- ext(m)
		  soil[["ph"]] <- mask(project(ph, m, method="bilinear"), m)
	wc <- terra::crop(rast(file.path(soil.path,"Water_Capacity.tif")), ext(m))
	  crs(wc) <- crs(cbdy)
	    ext(wc) <- ext(m)
	      soil[["Water_Capacity"]] <- mask(project(wc, m, method="bilinear"), m)
	})
	for(l in 1:length(soil)){ names(soil[[l]]) <- names(soil)[l] }
	if(length(soil) > 0){
      for(sn in 1:length(soil)) {
        writeRaster(soil[[sn]], file.path(data.dir, paste0(names(soil)[sn], ".tif")), overwrite=TRUE)  
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
      flush.console(); Sys.sleep(0.01)			
        dir.create(file.path(getwd(), "results"),
                   showWarnings = FALSE)
	  results.dir <- file.path(ct)
	  
    #****************************************
    # Check to see if any parameters were created, if not
	# write a zero probability raster
	#***************************************
	vnames = c("elev", "twi", "trasp", "slope", "aspect", "tpi", "tri", "slope", "roughness", 
               "scosa", "hli", "diss", "map", "mat", "gsp", "ffp", "dd5", "smrp", "smrsprpb", 
	           "sprp", "winp", "Available_Water", "Restrictive_Depth", "Hydraulic_Conductivity", 
	           "cpi", "Pct_Clay", "Pct_Sand", "ph", "Water_Capacity")       
	parms <- vnames[sort(match(vnames, rm.ext(list.files(data.dir, "tif$"))))]
	  if(length(parms) < 3) {
        fail <- data.frame(state = state, county = county, reason = "failed to create any parameters")
          write.table(fail, file.path(root, "model_fail.csv"), sep = ",", row.names = FALSE, 
                      append = TRUE, col.names = !file.exists(file.path(root, "model_fail.csv")))
	      m[m == 1] <- 0
        writeRaster(m, "probs.tif", overwrite=TRUE)        		
        next			  
	  }
	  
    #****************************************
    # read raster data and assign to training
    #****************************************
    # Read and assign y data to sample data	

    train$y  <- extract(nass, vect(train))[,2]
	  na.idx <- which(is.nan(train$y))
    if(length(na.idx) > 1) train <- train[-na.idx,]

	# assign parameter values, remove columns > 30% NA	
	r <- rast(list.files(data.dir, "tif$", full.names = TRUE))
      train <- data.frame(train, extract(r, vect(train)))
        na.col.idx <- vector()
          for(cl in 1:ncol(train)) {
            p = length(which(is.na(train[,cl]))) / nrow(train)
            if(p > 0.3) na.col.idx <- append(na.col.idx, cl)
          }
      if(length(na.col.idx)>0) train <- train[,-na.col.idx]
	    train <- na.omit(train)
	      train <- train[,c(3,5:ncol(train))]
		    train$y <- ifelse(train$y > 1, 1, train$y)

      rm.idx <- which(names(train) %in% c("elev", "Layer_1"))
        if(length(rm.idx) > 0) train <- train[,-rm.idx]

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
    # test collinearity 
    ( cl.vars <- collinear(train[,2:ncol(train)], p = 0.70, 
                           nonlinear = FALSE, p.value = 0.001) )
	if(length(cl.vars) > 0) 					   
      train <- train[,-which(names(train) %in% cl.vars)]
 	
	#****************************************
    # test multcollinearity using permutation
    #   with leave-one-out	
 
    ( cl.test <- multi.collinear(train[,2:ncol(train)], perm = TRUE, 
                                 leave.out = TRUE, n = 99) )
      rm.vars <- cl.test[cl.test$frequency > 0,]$variables
        if(length(rm.vars) > 0) 
          train <- train[,-which(names(train) %in% rm.vars)]
 
    #****************************************
    # model selection and fit
    rand.seed = 4327  	
    y <- factor(train[,"y"])  
    x <- train[,2:ncol(train)]  
    rf.sel <- rf.modelSel(xdata = x, ydata = y, imp.scale = "mir",  
                            r = seq(0,1,0.2)[-c(1,6)], seed = rand.seed,
      					  num.trees = b) 			
       ( sel.vars <- rf.sel$selvars )  
    ( rf.fit <- ranger(x = x[,sel.vars], y = as.factor(y), probability = TRUE, 
                       num.trees = b, importance="permutation") )
  
    # Optimize fit on NULL class prediction variance
    if(optimize.preds) {
	  cat("optimizing model predictions", "\n")
	    flush.console(); Sys.sleep(0.01)		
	  p <- predict(rf.fit, data = x[,sel.vars])$predictions[,2]
	  ll <- logLoss(y, p, global=FALSE)$log.loss
	    pidx <- which(y == "0" & p > 0.45 & ll > 0.8)
          if(length(pidx) > 0) {		
            x <- x[-pidx,]	
            y <- y[-pidx]			 
            ( rf.fit <- ranger(x = x[,sel.vars], y = y, probability = TRUE, 
                               num.trees = b, importance="permutation") )	   
          }
    }
  
    if(exists("rf.fit")) {
	  idx <- order(rf.fit$variable.importance, decreasing = FALSE)
	    imp <- rf.fit$variable.importance[idx]

    #****************************************
    # Model validation of fit 
	probs <- data.frame(y = y, probs = as.numeric(ranger:::predict.ranger(rf.fit,  
                        data = x[,sel.vars], verbose = FALSE)$predictions[,2]))	
    v <- accuracy(table(y, ifelse(rf.fit$predictions[,2] < 0.65, 0, 1)))
    ll <- logLoss(y = y, p = rf.fit$predictions[,2])

    mval <- data.frame(state = state, county = county,
      "Global_Log_Loss" = ll,
	  "Accuracy_PCC" = v[["PCC"]],
      "Kappa" = v[["kappa"]],
      "AUC" = v[["auc"]], 
      "True_Skill" = v[["true.skill"]], 
      "Sensitivity" = v[["sensitivity"]], 
      "Specificity" = v[["specificity"]],  
      "Gain" = v[["gain"]],
      "Positive_Likelihood_Ratio" = v[["plr"]], 
      "Negative_Likelihood_Ratio" = v[["nlr"]],
      "Type_I_Error" = v[["typeI.error"]], 
      "Type_II_Error" = v[["typeII.error"]],  
      "Gini_Index" = v[["gini"]],  
      "F_score:" = v[["f.score"]])
	write.table(mval, file.path(root, validation.file), sep = ",", row.names = FALSE, 
           append = TRUE, col.names = !file.exists(file.path(root, validation.file)))	  
	  
    #****************************************
    # Model Bootstrap cross-validation of performance
	#( mcv <- rf.crossValidation(rf.fit,  p=0.10, n = 99) )
	if(boot.crossvalidate) {
	  cv.boot = 99 
	  xvalidation <- list()
	  for(i in 1:cv.boot) {
	    cat("Bootstrap cross-validation", i, "of", cv.boot, "\n")
		  flush.console(); Sys.sleep(0.01)		  
	    ss <- round(min(table(y)) * sample.prop, 0)
	      idx0 = sample(which(y == 0), ss)
	      idx1 = sample(which(y == 1), ss)
	        xsub = x[-c(idx0,idx1),]
	        ysub = y[-c(idx0,idx1)] 
	    rf.test <- ranger(x = xsub[,sel.vars], y = ysub, probability = TRUE, 
                          num.trees = b, importance="permutation")    
	    probs <- data.frame(y = ysub, probs = as.numeric(ranger:::predict.ranger(rf.test,  
                            data = xsub[,sel.vars], verbose = FALSE)$predictions[,2]))	
        v <- accuracy(table(ysub, ifelse(rf.test$predictions[,2] < 0.65, 0, 1)))
        gll <- logLoss(probs$y, probs$probs, global=TRUE) 	  
	      xvalidation[["Global log loss"]] <- append(xvalidation[["Global log loss"]], gll)
          xvalidation[["PCC"]] <- append(xvalidation[["PCC"]], v[["PCC"]])  
		  xvalidation[["kappa"]] <- append(xvalidation[["kappa"]], v[["kappa"]])
		  xvalidation[["AUC"]] <- append(xvalidation[["AUC"]], v[["auc"]]) 
	      xvalidation[["true.skill"]] <- append(xvalidation[["true.skill"]], v[["true.skill"]]) 
		  xvalidation[["sensitivity"]] <- append(xvalidation[["sensitivity"]], v[["sensitivity"]]) 
          xvalidation[["specificity"]] <- append(xvalidation[["specificity"]], v[["specificity"]]) 
		  xvalidation[["gain"]] <- append(xvalidation[["gain"]], v[["gain"]]) 
          xvalidation[["PositiveLikelihoodRatio"]] <- append(xvalidation[["PositiveLikelihoodRatio"]],v[["plr"]]) 
          xvalidation[["NegativeLikelihoodRatio"]] <- append(xvalidation[["NegativeLikelihoodRatio"]],v[["nlr"]])
          xvalidation[["TypeI.error"]] <- append(xvalidation[["TypeI.error"]], v[["typeI.error"]])
          xvalidation[["TypeII.error"]] <- append(xvalidation[["TypeII.error"]], v[["typeII.error"]])
          xvalidation[["Gini"]] <- append(xvalidation[["Gini"]], v[["gini"]]) 
          xvalidation[["F-score"]] <- append(xvalidation[["F-score"]], v[["f.score"]])
	  }
      cv.med <- lapply(xvalidation, median)
	  cv.coffvar <- lapply(xvalidation, \(i) { (sd(i) / mean(i)) * 100 } )
        cmval <- data.frame(state = state, county = county,
          "Global_Log_Loss" = gll,
	      "Accuracy_PCC" = cv.med[["PCC"]],
          "Kappa" = cv.med[["kappa"]],
          "AUC" = cv.med[["auc"]], 
          "True_Skill" = cv.med[["true.skill"]], 
          "Sensiticv.medity" = cv.med[["sensiticv.medity"]], 
          "Specificity" = cv.med[["specificity"]],  
          "Gain" = cv.med[["gain"]],
          "Positicv.mede_Likelihood_Ratio" = cv.med[["plr"]], 
          "Negaticv.mede_Likelihood_Ratio" = cv.med[["nlr"]],
          "Type_I_Error" = cv.med[["typeI.error"]], 
          "Type_II_Error" = cv.med[["typeII.error"]],  
          "Gini_Index" = cv.med[["gini"]],  
          "F_score:" = cv.med[["f.score"]])
	    write.table(cmval, file.path(root, cross.validation.file), sep = ",", row.names = FALSE, 
               append = TRUE, col.names = !file.exists(file.path(root, cross.validation.file)))
    }

    #****************************************
    # Make spatial probabilistic predictions	
    if(prob.raster) {
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
	  p <- terra::predict(r, rf.fit, fun = predict.prob)
        p <- mask(crop(p, m)), m)
	  writeRaster(p, "probs.tif", overwrite=TRUE)	  
    }

    #****************************************
    # estimate uncertainty
	 
	 # Standard error predection function
      predict.se <- function(model, data) {
        as.numeric(ranger:::predict.ranger(model, data = data,
                   type = "se", se.method = "infjack")$se[,2])
      }
	  
	if(uncertanity) {
	  # plot of uncertanity
	  mdl.probs = ranger:::predict.ranger(rf.fit, data = x[,sel.vars], verbose = FALSE)$predictions[,2]
      se.mdl <- ranger(x=x[,sel.vars], y=as.factor(y), probability = TRUE, 
                      num.trees = b, importance="permutation", 
	  			      write.forest = TRUE, keep.inbag = TRUE)
	    sdm.se <- predict(se.mdl, data=x[,sel.vars], type = "se", se.method = "infjack")$se[,2]
		  low.ci <- mdl.probs - 1.96  * sdm.se
          up.ci <-mdl.probs + 1.96  * sdm.se
        pdf("ConfidenceIntervals.pdf", height=10, width=10)	      
	      plot(sort.int(mdl.probs, index.return = TRUE)$x, type="l", xlab=NA, ylab="probs", xaxt = "n",
		       main=paste0("Confidence intervals for ", state, " - ", basename(ct), " tillage model" ))
		         lines(sort.int(low.ci, index.return = TRUE)$x, lty=2, col="grey")
		         lines(sort.int(up.ci, index.return = TRUE)$x, lty=2, col="grey")
	    dev.off()
	}
	
	if(uncertanity.raster & exists("p")) {
       se.mdl <- ranger(x=x[,sel.vars], y=as.factor(y), probability = TRUE, 
                      num.trees = 99, importance="permutation", 
	  			      write.forest = TRUE, keep.inbag = TRUE)	
      sdm.se <- predict(r, se.mdl, fun=predict.se, progress="window")
        sdm.se <- mask(crop(sdm.se, extent(raster(m))), raster(m))
      # Upper and Lower Confidence interval
      ci95 <- stack( p - (sdm.se * 1.96),
                     p + (sdm.se * 1.96) )
        ci95 <- mask(ci95, raster(m))
          names(ci95) <- c("lower.ci", "upper.ci")
	  
      # write probabilities, standard error, and confidence intervals
	    writeRaster(stack(p, sdm.se, ci95), "probs.tif", overwrite=TRUE)
    }
	
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
      if(plot.partial) {    
	    for(rn in names(rf.fit$variable.importance)) {
          cat("Calculating partial plot for parameter: ", rn, "\n")      
            pp <- pdp::partial(rf.fit, pred.var = rn, plot = TRUE, which.class = 2, 
                          train = data.frame(y = y, x), levelplot = TRUE,
           		        chull = TRUE, quantiles = TRUE, smooth=TRUE,
           		        plot.engine = "ggplot2")
              suppressWarnings(print(pp))
            }
      }		
    dev.off()
      remove(se.mdl, sdm.se, ci95, r, x, y) 
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
  remove(train, p, m, cbdy) 
  } # end county loop
