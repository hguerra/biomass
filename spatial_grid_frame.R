print("Loading package 'RPostgreSQL'...")
require("sp")
require("rgdal")
require("RPostgreSQL")
require("dismo")

print("Creating functions...")
createConnection <- function(host = "150.163.58.218", user = "eba", password = "ebaeba18", dbname = "eba") {
  dbConnect(PostgreSQL(), host = host, user = user, password = password, dbname = dbname)
}

query <- function(sql, connection) {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  rs <- dbSendQuery(connection, sql)
  fetch(rs, n = -1)
}

getClass <- function(host = "localhost") {
  sql <- "
  SELECT
  polys.ogc_fid,
  st_x(st_centroid(polys.geom)) AS x,
  st_y(st_centroid(polys.geom)) AS y,
  fbiomass_classes(polys.agblongo_tch_alive) as class,
  polys.agblongo_tch_alive,
  polys.evi_max,
  polys.evi_mean,
  polys.evi_median,
  polys.evi_min,
  polys.evi_q1,
  polys.evi_q3,
  polys.evi_sd,
  polys.ndvi_max,
  polys.ndvi_mean,
  polys.ndvi_median,
  polys.ndvi_min,
  polys.ndvi_q1,
  polys.ndvi_q3,
  polys.palsar_hh,
  polys.palsar_hhrhv1,
  polys.palsar_hv,
  polys.trmm_max,
  polys.trmm_median,
  polys.trmm_min,
  polys.trmm_q1,
  polys.trmm_q3
  FROM
  amazon_test polys INNER JOIN transects bb ON ST_Intersects(bb.polyflown, polys.geom)
  WHERE
  polys.agblongo_tch_alive IS NOT NULL AND
  polys.evi_max IS NOT NULL AND
  polys.evi_mean IS NOT NULL AND
  polys.evi_median IS NOT NULL AND
  polys.evi_min IS NOT NULL AND
  polys.evi_q1 IS NOT NULL AND
  polys.evi_q3 IS NOT NULL AND
  polys.evi_sd IS NOT NULL AND
  polys.ndvi_max IS NOT NULL AND
  polys.ndvi_mean IS NOT NULL AND
  polys.ndvi_median IS NOT NULL AND
  polys.ndvi_min IS NOT NULL AND
  polys.ndvi_q1 IS NOT NULL AND
  polys.ndvi_q3 IS NOT NULL AND
  polys.palsar_hh IS NOT NULL AND
  polys.palsar_hhrhv1 IS NOT NULL AND
  polys.palsar_hv IS NOT NULL AND
  polys.trmm_max IS NOT NULL AND
  polys.trmm_median IS NOT NULL AND
  polys.trmm_min IS NOT NULL AND
  polys.trmm_q1 IS NOT NULL AND
  polys.trmm_q3 IS NOT NULL;"
  
  query(sql, createConnection(host))
}

getGrid <- function(host = "localhost") {
  sql <- "
  SELECT
  polys.ogc_fid,
  st_x(st_centroid(polys.geom)) AS x,
  st_y(st_centroid(polys.geom)) AS y,
  polys.agblongo_tch_alive,
  polys.evi_max,
  polys.evi_mean,
  polys.evi_median,
  polys.evi_min,
  polys.evi_q1,
  polys.evi_q3,
  polys.evi_sd,
  polys.ndvi_max,
  polys.ndvi_mean,
  polys.ndvi_median,
  polys.ndvi_min,
  polys.ndvi_q1,
  polys.ndvi_q3,
  polys.palsar_hh,
  polys.palsar_hhrhv1,
  polys.palsar_hv,
  polys.trmm_max,
  polys.trmm_median,
  polys.trmm_min,
  polys.trmm_q1,
  polys.trmm_q3
  FROM
  amazon_test polys
  WHERE
  polys.evi_max IS NOT NULL AND
  polys.evi_mean IS NOT NULL AND
  polys.evi_median IS NOT NULL AND
  polys.evi_min IS NOT NULL AND
  polys.evi_q1 IS NOT NULL AND
  polys.evi_q3 IS NOT NULL AND
  polys.evi_sd IS NOT NULL AND
  polys.ndvi_max IS NOT NULL AND
  polys.ndvi_mean IS NOT NULL AND
  polys.ndvi_median IS NOT NULL AND
  polys.ndvi_min IS NOT NULL AND
  polys.ndvi_q1 IS NOT NULL AND
  polys.ndvi_q3 IS NOT NULL AND
  polys.palsar_hh IS NOT NULL AND
  polys.palsar_hhrhv1 IS NOT NULL AND
  polys.palsar_hv IS NOT NULL AND
  polys.trmm_max IS NOT NULL AND
  polys.trmm_median IS NOT NULL AND
  polys.trmm_min IS NOT NULL AND
  polys.trmm_q1 IS NOT NULL AND
  polys.trmm_q3 IS NOT NULL;"

  query(sql, createConnection(host))
}

#
# MAIN
#

roraima.class.path <- "E:\\heitor.guerra\\R_test\\roraima_class.RData"
roraima.grid.path <- "E:\\heitor.guerra\\R_test\\roraima_grid.RData"
r.path <- "E:\\heitor.guerra\\tests\\extrapolar\\EVI_max.tif"

#print("Querying data from PostgreSQL...")
#roraima.class <- getClass()
roraima.grid <- getGrid()

#print(paste("Saving Data Frame '", roraima.class.path, "' in disk"))
#save(roraima.class, file = roraima.class.path)

print(paste("Saving Data Frame '", roraima.grid.path, "' in disk"))
save(roraima.grid, file = roraima.grid.path)

print(paste("Loading Data Frame '", roraima.class.path, "' from disk"))
load(roraima.class.path)

# print(paste("Loading Data Frame '", roraima.grid.path, "' from disk"))
# load(roraima.grid.path)

print("Importing occurrence data...")

occurence <- subset(roraima.class, select=c("class", "x", "y"))
occ <- occurence[,-1]

# witholding a 20% sample for testing 
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1, ]
occtrain <- occ[fold != 1, ]

# get predictor variables
select <- c("x", "y",
            "evi_max", "evi_mean", "evi_median", "evi_min", "evi_q1", "evi_q3", "evi_sd", 
            "ndvi_max", "ndvi_mean", "ndvi_median", "ndvi_min", "ndvi_q1", "ndvi_q3",
            "palsar_hh", "palsar_hhrhv1", "palsar_hv", 
            "trmm_max", "trmm_median", "trmm_min", "trmm_q1", "trmm_q3")

# predictors <- subset(roraima.class, select=select)

points <- subset(roraima.class, select=select, na.rm=TRUE)
coordinates(points) <- ~ x + y
proj4string(points) <- CRS("+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

base.raster <- raster(r.path)
predictors <- stack()
for(name in names(points)) {
  print(paste("Creating raster ", name, "...", sep = ""))
  new.raster <- raster(ncol=base.raster@ncols, nrow=base.raster@nrows)
  crs(new.raster) <- crs(base.raster)
  extent(new.raster) <- extent(base.raster)
  names(new.raster) <- name

  new.raster <- rasterize(points, new.raster, field=name, fun=mean, background=NA, mask=FALSE, update=FALSE, updateValue='all', na.rm=TRUE)
  predictors <- stack(predictors, new.raster)
}



# evi_max <- rasterize(points, new.raster, field="evi_max", fun=mean, background=NA, mask=FALSE, update=FALSE, updateValue='all', na.rm=TRUE)
# evi_max <- rasterize(points, new.raster, points$evi_max, fun=mean)

# pixels <- SpatialPixelsDataFrame(points, points@data, tolerance = 0.700001)
# gridded(pixels) <- TRUE
# predictors <- as(pixels, "SpatialGridDataFrame")
# predictors <- stack(pixels)
# 
# # fit model, biome is a categorical variable
# me <- maxent(predictors, occtrain, factors='class')
