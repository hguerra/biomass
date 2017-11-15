require(raster)
require(rgdal)

setwd("Z:\\heitor.guerra\\EVI")
file.tiff <- 'vegtype_2000_5880.tif'


r.rain <- raster(file.tiff)
r.pol = rasterToPolygons(r.rain)

names(r.pol@data) = "value"
writeOGR(obj = r.pol, dsn = "data", layer = "vegtype_2000", driver = "ESRI Shapefile")
