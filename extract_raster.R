print("Loading package 'RPostgreSQL'...")
require("raster")
require("sp")
require("rgdal")

print("Creating functions...")

exportOccurence <- function(class.path, output.dir) {
  print(paste("Loading Data Frame '", class.path, "' from disk"))
  load(class.path)
  
  print("Exporting occurrence data...")
  occurence <- subset(transects, select=c("class", "x", "y"))
  rm(transects)

  write.csv(occurence, file=paste(output.dir, "\\class.csv", sep=""), row.names=FALSE)
  print("Done.")
}

exportRaster <- function(grid.path, raster.path, select, output.dir) {
  print(paste("Loading Data Frame '", grid.path, "' from disk"))
  load(grid.path)

  points <- subset(cells, select=select, na.rm=TRUE)
  rm(cells)
  
  coordinates(points) <- ~ x + y
  proj4string(points) <- CRS("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  base.raster <- raster(raster.path)
  for(name in names(points)) {
    print(paste("Creating raster ", name, "...", sep = ""))
    new.raster <- raster(ncol=base.raster@ncols, nrow=base.raster@nrows)
    crs(new.raster) <- crs(base.raster)
    extent(new.raster) <- extent(base.raster)
    names(new.raster) <- name
    
    filename <- paste(output.dir, "\\", name, ".asc", sep="")
    rasterize(points, new.raster, field=name, fun=mean, background=NA, mask=FALSE, update=FALSE, updateValue='all', filename=filename, na.rm=TRUE)
  }
}

#
# MAIN
#

output.dir <- "E:\\heitor.guerra\\R_test\\output"
amazon.class.path <- "E:\\heitor.guerra\\R_test\\transects.RData"
amazon.grid.path <- "E:\\heitor.guerra\\R_test\\cells.RData"
base.raster.path <- "E:\\heitor.guerra\\tests\\extrapolar\\EVI_max.tif"

# predictor variables
select <- c("x", "y",
            "evi_max", "evi_mean", "evi_median", "evi_min", "evi_q1", "evi_q3", "evi_sd",
            "ndvi_max", "ndvi_mean", "ndvi_median", "ndvi_min", "ndvi_q1", "ndvi_q3",
            "palsar_hh", "palsar_hhrhv1", "palsar_hv",
            "trmm_max", "trmm_median", "trmm_min", "trmm_q1", "trmm_q3")

exportOccurence(amazon.class.path, output.dir)
exportRaster(amazon.grid.path, base.raster.path, select, output.dir)
