#***********************************************
#***********************************************
# Summarize validation by state-county
#***********************************************
#***********************************************
library(sp)
library(sf)
library(rgdal)
library(spatialEco)

root = "D:/tillage"
model.dir = "model"

results <- data.frame(matrix(vector(), 0, 16,
           dimnames=list(c(), c("state", "county", "log.loss", "pcc", "kappa",
           "auc", "tss", "sensitivity", "specificity", "precision", "pos.likelihood",
           "neg.likelihood" , "typeIerror", "typeIIerror", "gini", "fscore"))),
            stringsAsFactors=FALSE)

#***********************************************
# Start of state(st) - county(ct) model loop
#***********************************************
i=0
states = list.dirs(path = file.path(root,model.dir),
                   full.names = TRUE, 
                   recursive = FALSE)

for(st in states) {
  state = basename(st)
  counties = list.dirs(path = st, full.names = TRUE, 
                       recursive = FALSE) 
  for(ct in counties) {
    county = basename(ct)  
    if(file.exists(file.path(ct,"results","validation.txt"))) {
      i= i+1
      d <- read.table(file.path(ct,"results","validation.txt"),
                      header=FALSE, sep=":")
      results[i,][1:2] <- c(state,county)
        results[i,][3:16] <- as.numeric(d[,2])
    } else {
      next
    }
  }
}
write.csv(results, file.path(root, "county_validation.csv"))

results <- read.csv(file.path(root, "county_validation.csv"))
  results[is.na(results)] <- -999
    
# Join to county spatial data ("CONAME","STNAME") and write shapefile
ctys <- as(st_read(file.path(root, "model", "CONUScounties.shp")), "Spatial")
  ctys@data <- ctys@data[,c(3,65)]
ctys@data$mergeby <- paste(ctys@data[,"STATE_NAME"], ctys@data[,"CONAME"], sep=".")
  results$mergeby <- paste(results[,"state"], results[,"county"], sep=".")
    results <- results[-which(duplicated(results$mergeby)),]
   ctys <- merge(ctys, results, by="mergeby")
     ctys <- ctys[-which(is.na(ctys$log.loss)),]
st_write(as(ctys,"sf"), file.path(root, "county_validation.shp"),
         overwrite = TRUE)
		 
which(duplicated(results$mergeby))		 
