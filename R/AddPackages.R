# Check and add required packages


############### Required libraries ############### 
libs <- c("rgdal", "FedData", "geoknife", "purrr", 
          "rfUtilities","raster", "sf", "randomForest", 
		  "spatialEco", "ranger")
##################################################

options(stringsAsFactors=FALSE)
options(scipen=9999)
options(help_type="html")

#### set site library
.Library.site <- file.path(chartr("\\", "/", R.home()), "library")
.libPaths(file.path(chartr("\\", "/", R.home()), "library"))

#### set a CRAN mirror
local({r <- getOption("repos")
       r["CRAN"] <- "https://cran.cnr.berkeley.edu"
       options(repos=r)})

p <- as.data.frame(installed.packages())[,c("Package", "Version")]

# Install devtools from CRAN and FedData from GitHub
libraries(c("devtools","remotes"), check.source = FALSE, 
             repository = getOption("repos"), lib = .Library)
if(!"FedData" %in% p$Package) {
  devtools::install_github("ropensci/FedData")
  } else if(packageVersion("FedData") < "3.0.0.9000") { 
  devtools::install_github("ropensci/FedData")  
}

# Install packages
libraries(libs, add=TRUE, check.source=FALSE)


