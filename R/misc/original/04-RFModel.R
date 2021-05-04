#######################################################################################
#######################################################################################
# 04 - RANDOM FORESTS TILLAGE MODEL WITH MODEL SELECTION AND VALIDATION. PRODUCES A 
#   RASTER OF PREDICTED PROBABILITIES. LOOPS THROUGH ALL COUNTIES. PIXEL LEVEL.
#######################################################################################
#######################################################################################
source("D:/CROP/CODE/FUNCTIONS.R")
state="COLORADO"
state.abvr="CO"

path=paste("D:/CROP", state, sep="/")
fpath=paste(paste("D:/CROP", state, sep="/"), "RESULTS", sep="/")
dirs <- list.files(path, pattern=state.abvr) 

# SHAPEFILE NAME
inshape="sample"
rtype="img"
praster="cprob"

flist <- c("drclassdcd", "drclasswet", "flodfreqdc", "flodfreqma", 
           "hydclprs", "hydgrpdcd", "pondfreqpr", "slopegradd", 
           "wtdepannmi", "wtdepaprju")

###### START LOOP FOR ALL MODELS ######           
for (d in dirs) {                
  setwd(paste(path, d, sep="/"))	
    spath = paste(getwd(), "VECTOR", sep="/") 
    rpath = paste(getwd(), "RASTER", sep="/")
print(paste("PROCESSING COUNTY", getwd(), sep=" : ") )
	
# READ SHAPEFILE
csamp <- readOGR(dsn=spath, layer=inshape)
  
#############################################  			 
#############################################
# ASSIGN RASTER VALUES TO POINTS            #
#############################################
#############################################
rlist=list.files(rpath, pattern="img$", full.names=TRUE)
  rlist <- rlist[-which(rlist == paste(rpath, "elev.img", sep="/"))]
  rlist <- rlist[-which(rlist == paste(rpath, paste(tolower(d), "img", sep="."), sep="/"))]
  
xvars <- stack(rlist)
 v <- extract(xvars, csamp)
   v <- as.data.frame(v)
csamp@data = data.frame(csamp@data, v[match(rownames(csamp@data), rownames(v)),])

# writeOGR(csamp, dsn=spath, layer="Sample", driver="ESRI Shapefile")
  
# FACTORIZE SSURGO VARIABLES IN POINT SAMPLE   
for(i in flist) {
   csamp@data[,i] <- factor(csamp@data[,i])
  }
	
#####################################################################
#####################################################################
# SCREEN VARIABES FOR MISSING DATA, UNUSABLE FACTORS AND INVARIANCE #
#####################################################################
#####################################################################
# REMOVE VARIABLES WITH > 10% PERCENT MISSING VALUES
scol=3
vars <- names(csamp@data[,scol:ncol(csamp@data)])
 r <- as.vector(array(0, dim=c((0),(1)))) 
 pct <- 0.05
 for(i in 1:length(vars) ) {   
    p <- colSums(is.na(csamp@data[,scol:ncol(csamp@data)])) / 
          colSums(!is.na(csamp@data[,scol:ncol(csamp@data)]))   
	  if ( (p[i] > pct) == TRUE) { r <- as.vector(append(r, vars[i], after=length(r))) }
	}
print(r)
 if ( (length(r) > 0) == TRUE) { csamp@data <- csamp@data[-match(r, names(csamp@data))]  }
 	
# REMOVE VARIABLES WITH INVARIANCE OR UNUSABLE FACTORS (x<2 OR x>30)
vars <- names(csamp@data[,scol:ncol(csamp@data)])
  r <- as.vector(array(0, dim=c((0),(1)))) 	
  for(i in vars ) {        
	  if (is.factor(csamp@data[,i]) == TRUE ) {
	      if( (nlevels(csamp@data[,i]) > 30) == TRUE) { r <- as.vector(append(r, i, after=length(r))) }
	      if( (nlevels(csamp@data[,i]) < 2) == TRUE) { r <- as.vector(append(r, i, after=length(r))) }   
	   }	 	  
	  if ( (is.numeric(csamp@data[,i]) ) == TRUE ) {
	    if( ( range(csamp@data[,i], na.rm=TRUE)[1] == range(csamp@data[,i], na.rm=TRUE)[2] ) == TRUE)	
	         { r <- as.vector(append(r, i, after=length(r))) }      
	   }
    }
print(r)
 if ( (length(r) > 0) == TRUE) { csamp@data <- csamp@data[-match(r, names(csamp@data))]  }
 
# REMOVE NA ROW OBSERVATIONS 
csamp@data <- na.omit(csamp@data) 
 
# RE-FACTORIZE SSURGO VARIABLES IN POINT SAMPLE 
vars <- names(csamp@data[,scol:ncol(csamp@data)])
  for(i in vars) {
     if (is.factor(csamp@data[,i]) == TRUE ) { 
       csamp@data[,i] <- factor(csamp@data[,i])
   	  } 
    }

######################################################################## 
########################################################################
#          RANDOM FOREST TILLAGE MODEL AND RASTER PREDICTION           #
########################################################################
########################################################################
# NUMBER OF BOOTSTRAP REPLICATES
b=501 
  
# CREATE X,Y DATA
ydata <- as.factor(csamp@data[,"y"])
xdata <- csamp@data[,scol:ncol(csamp@data)]

( rf.model <- rf.modelSel(x=xdata, y=ydata, imp.scale="se", ntree=b, 
                          strata=ydata, final=FALSE, na.action=na.omit,
						  plot.imp=FALSE) )

# IF ERROR IS WITHIN THRESHOLD SELECT MORE PARSIMONOUS MODEL						  
thresh=0.03
  error <- apply(rf.model$TEST[,2:3], MARGIN=1, FUN=sum)
    nparm <- rf.model$TEST[,4] 
      x <- vector()
  for (i in 2:length(error)) {
     if( abs(error[1] - error[i]) / error[i] <= thresh) {
	   x <- append(x, "TRUE", after=length(x))
		  } else {
	   x <- append(x, "FALSE", after=length(x))
	}
  }
epost <- as.vector(1)
 epost <- append(epost, which(x == "TRUE")+1)
   nparm <- nparm[epost]
  if( length(epost) > 1 ) { 
	 min.post <- which(nparm  == min(nparm))	 	 
    index=as.numeric(rownames(rf.model$TEST))[min.post]
    sel.vars=unlist(rf.model$PARAMETERS[index])	 	 
    } else {
      sel.vars <- rf.model$SELVARS
    }						  
  						  
# RUN FINAL MODEL 
  rf.data <- data.frame(y=ydata, xdata[,sel.vars])
  rf.final <- randomForest(x=rf.data[,2:ncol(rf.data)], y=rf.data[,"y"], ntree=b, 
                           importance=TRUE, norm.votes=TRUE, na.action=na.omit)

#( rf.balanced <- rfClassBalance(ydata=rf.data[,1], xdata=rf.data[,2:ncol(rf.data)], 
#                                  ntree=b) )
  						   
#################		   
# PREDICT MODEL #
#################
xvars <- stack(paste(rpath, paste(rownames(rf.final$importance), rtype, sep="."), sep="/"))
  r <- predict(xvars, rf.final, type="prob", progress="window", index=2,
               filename=paste(fpath, paste(paste(d, "cprob", sep="_"), 
               rtype, sep="."), sep="/"), overwrite=TRUE)				   
###################################################
save.image( paste(getwd(), "Model.RData", sep="/") ) 
} # END OF ALL MODELS LOOP

