# https://gis.stackexchange.com/questions/39021/how-to-perform-random-forest-land-cover-classification

print("Setting work directory...")
setwd("E:/heitor.guerra/db_backup/rasters/ipcc")
datasource <- getwd()

result <- paste(datasource, "/result/", sep="")
dir.create(result, showWarnings = FALSE)

rf.dump <- "rf.mdl_chm_3_total_below_60.RData"
rf.file <- "random_forest_chm_3_total_below_60_2.tif"

print("Loading packages...")
require(sp)
require(rgdal)
require(raster)
require(randomForest)

# LOAD DATA FROM
datasource <- getwd()
rf.dump <- paste(result, rf.dump, sep="")
rf.file <- paste(result, rf.file, sep="")

# CREATE LIST OF RASTERS
print("Listing rasters...")
rlist=list.files(datasource, pattern="\\.tif$", full.names=TRUE) 

# CREATE RASTER STACK
print("Creating a stack...")
xvars <- stack(rlist)      

load(rf.dump)

# PREDICT MODEL
print("Predicting model...")
predict(xvars, rf.mdl, filename=rf.file, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

print("Done.")
