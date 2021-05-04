library(RSAGA)
library(raster)

# SET SAGA ENVIRONMENT
( saga.env <- rsaga.env(workspace = getwd(), cores = 4, parallel = TRUE,  
    path = "C:/Program Files/saga-7.5.0_x64") )		

options(warn = -1) # turn off saga version warnings

#***********************************************
# DISPLAY AVALIABLE SAGA LIBRARIES
rsaga.get.libraries(path = saga.env$modules)

# DISPLAY MODULES IN SPECIFIC LIBRARY 			
rsaga.get.lib.modules("ta_morphometry", env=saga.env)
rsaga.get.lib.modules("ta_hydrology", env=saga.env)

# DISPLAY USAGE OF MODULE
rsaga.get.usage("ta_morphometry", 17, env=saga.env)
rsaga.get.usage("ta_hydrology", 15, env=saga.env)

# SHOW HELP FOR gdal AND IMPORT RASTER
rsaga.get.lib.modules("io_gdal", env=saga.env)
  rsaga.get.usage("io_gdal", 0, env=saga.env) # GDAL IMPORT 
  rsaga.get.usage("io_gdal", 1, env=saga.env) # GDAL EXPORT 
#***********************************************
ct = "C:/evans/tillage/model/Wyoming/Sublette"

setwd(ct)
dir.create(file.path(getwd(), "tmp"), 
           showWarnings = FALSE)

inrast = "elev.tif"
outrast = "cti.tif"
out.dir = file.path(getwd(), "data")
tmp.dir = file.path(getwd(), "tmp")

# Import DEM
rsaga.geoprocessor(lib="io_gdal", module=0, 
                   param=list(GRIDS=file.path(tmp.dir, unlist(strsplit(inrast, "\\."))[1]), 
                   FILES=file.path(out.dir,inrast), RESAMPLING=1), env=saga.env)

#*************************************************************
# Wetness Index (R function interface)
rsaga.wetness.index(file.path(tmp.dir,"elev.sgrd"), 
                    file.path(tmp.dir,"cti.sgrid"))

# EXPORT TO tif FORMAT RASTER					
rsaga.geoprocessor("io_gdal", 1, list(GRIDS=file.path(tmp.dir, 
                   paste(unlist(strsplit(outrast, "\\."))[1],
                   "sgrd", sep=".")), FILE = file.path(tmp.dir, outrast), 
				   FORMAT=5, TYPE=0), env=saga.env)

# Read into raster and export with LZW compression
writeRaster(raster(file.path(tmp.dir, "cti.tif")), 
            file.path(out.dir, "wetness.tif"),
            overwrite=TRUE, options="COMPRESS=LZW" )
#*************************************************************

# HSP
rsaga.geoprocessor("ta_morphometry", 23, list(DEM=file.path(tmp.dir, 
                   paste(unlist(strsplit(inrast, "\\."))[1], "sgrd", sep=".")),  
                   FEATURES=file.path(tmp.dir, paste("mpf", "sgrd", sep=".")) 
				   ), env=saga.env)
rsaga.geoprocessor("io_gdal", 1, list(GRIDS=file.path(tmp.dir, 
                   paste("mpf", "sgrd", sep=".")), 
                   FILE = file.path(tmp.dir, "mpf.tif"), 
				   FORMAT=5, TYPE=0), env=saga.env)
writeRaster(raster(file.path(tmp.dir, "mpf.tif")), 
            file.path(out.dir, "mpf.tif"),
            overwrite=TRUE, options="COMPRESS=LZW" )


#*************************************************************
file.remove(list.files(tmp.dir, full.names = TRUE))



# CLEAN UP				   
file.remove(paste(getwd(), paste(unlist(strsplit(inrast, "\\."))[1], 
            c("sgrd", "sdat", "mgrd", "prj", "xml"), sep="."), sep="/"))
file.remove(paste(getwd(), paste(unlist(strsplit(outrast, "\\."))[1], 
            c("sgrd", "sdat", "mgrd", "prj", "xml"), sep="."), sep="/"))
