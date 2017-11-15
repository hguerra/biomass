# https://stackoverflow.com/questions/20733555/how-to-create-a-raster-brick-with-rasters-of-different-extents
require(raster)

# LOAD DATA FROM
setwd("E:/heitor.guerra/db_backup/rasters/random_forest")
base.raster <- raster("E:/heitor.guerra/db_backup/rasters/test/trmm/tests/base.tif")

datasource <- "E:/heitor.guerra/db_backup/rasters/test/trmm/original"

# CREATE LIST OF RASTERS
rlist=list.files(datasource, pattern="tif$", full.names=TRUE) 

for (rpath in rlist) {
  r <- raster(rpath)

  # name <- basename(rpath)
  # print(paste("Creating raster ", name, "...", sep = ""))
  # 
  # new.raster <- raster(ncol=base.raster@ncols, nrow=base.raster@nrows)
  # crs(new.raster) <- crs(base.raster)
  # extent(new.raster) <- extent(base.raster)
  # 
  # r <- resample(r, new.raster, method="ngb")
  # writeRaster(r, name, format = "GTiff", overwrite=TRUE)
  
  r <- projectRaster(r, base.raster)
}


