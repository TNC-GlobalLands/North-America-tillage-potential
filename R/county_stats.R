#***********************************************
#***********************************************
# Summarize validation by state-county
#***********************************************
#***********************************************
library(sp)
library(sf)
library(terra)
library(rgdal)
library(spatialEco)

root = "C:/evans/tillage"
model.dir = "missing_states"

#***********************************************
# Start of state(st) - county(ct) model loop
#***********************************************
results <- data.frame(matrix(vector(), 0, 34,
             dimnames=list(c(), c("state", "county", "n", "prob_n", "min", 
             "p25th", "mean", "hmean", "gmean", "median", "p75th", "max",  
             "stdv", "var", "cv", "mad", "skew", "kurt", "nmodes", "mode", 
             "tillage_n", paste0("tfreq", as.character(0:12))) )),
             stringsAsFactors=FALSE)
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
    if(file.exists(file.path(ct,"probs.tif"))) {
	  i= i+1
      cat("Calculating statistics for: ", state, "-", county, "\n")
	  setwd(ct) 
      p <- terra::rast("probs.tif")
          nass <- crop(terra::rast(file.path(root,"model",state,"nass.tif")),ext(p))
	    nass <- terra::project(nass, p, method="near")
      nass <- mask(nass,p[[1]])  
    # calculate stats
	  p <- as.numeric(p[[1]][])
        p <- p[!is.na(p)]
          n = length(p)
          prob.count = length( p[p >= 0.40] )
        prob.stats = moments(p)
      remove(p)
	  tillage <- as.numeric(nass[])
        tillage <- tillage[!is.na(tillage)]
          tillage.count = length( tillage[tillage > 0] )
            tillage <- factor(tillage)
              l <- as.character(0:12)  
            new.levels <-  l[!l %in% levels(tillage)]
          if(length(new.levels) > 0)
            levels(tillage) <- c(levels(tillage), new.levels)
        tillage.freq <- table(tillage)
      remove(tillage)
      results[i,][1:2] <- c(state, county)
        results[i,][3:34] <- round(c(n, prob.count, prob.stats, tillage.count, 
                                   tillage.freq),4)
          row.names(results)[i] <- i
    } else {
      next
    }
  }
}
write.csv(results, file.path(root, "county_statistics.csv"))

#***************************************
# Summarize data
pct.dif <- function(x, y) { (x - y) / ((x + y) / 2) }
total.area <- function(x, units = c("acres", "hectares")) {
  if(units[1] == "hectares") {
    x.area <- (x * 30^2) / 0.0001  
  } else {
    x.area <- (x * 30^2) / 4046.8564224  
  }
  return(x.area)				
} 

results <- read.csv(file.path(root, "county_statistics.csv"))
  results[is.na(results)] <- -999

current <- results$tillage_n / results$n
potential <- results$prob_n / results$n
dif <- potential - current
  dif <- ifelse(dif < 0, 0, dif)
expansion.ac <- total.area( (dif * results$n) )

results <- data.frame(results, 
                      current = current, 
                      potential = potential,
                      exp_pct = dif, 
				      exp_ac = expansion.ac)

# Join to county spatial data ("CONAME","STNAME") and write shapefile
ctys <- as(st_read(file.path(root, "model_all", "CONUScounties.shp")), "Spatial")
  ctys@data <- ctys@data[,c(65,66)]
ctys@data$mergeby <- paste(ctys@data[,"STNAME"], ctys@data[,"CONAME"], sep=".")
  dat$mergeby <- paste(dat[,"state"], dat[,"county"], sep=".")
   ctys <- merge(ctys, dat, by="mergeby")
     ctys <- sp.na.omit(ctys)
st_write(as(ctys,"sf"), file.path(root, "county_statistics.shp"))


# exp_pct <- abs(pct.dif(results$tillage_n, results$prob_n))
#   exp_pct <- ifelse(results$prob_n < results$tillage_n, 0, exp_pct)*100 
# 
# exp_ac <- total.area(results$prob_n) - total.area(results$tillage_n)  
#   exp_ac <- ifelse(results$prob_n < results$tillage_n, 0, exp_ac)*100 
