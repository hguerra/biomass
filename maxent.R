# https://www.rdocumentation.org/packages/rpostgis/versions/1.3.0
require("rpostgis")
require("dismo")
require("raster")

createConnection <- function(host="150.163.58.218", port="5432", user="eba", password="ebaeba18", dbname="eba") {
  dbConnect("PostgreSQL",dbname=dbname,host=host,port=port,user=user,password=password)
}

loadFromPostGIS <- function(table, connection, clauses=NULL, geom="geom") {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  pgGetGeom(connection, name=c("public", table), geom=geom, clauses=clauses)
}

getData <- function(query, connection, geom="geom") {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  pgGetGeom(connection, query = query, geom=geom)
}

getAmazonCells <- function(host = "150.163.58.218") {
  sql <- "
  SELECT
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
  polys.trmm_mean,
  polys.trmm_median,
  polys.trmm_min,
  polys.trmm_q1,
  polys.trmm_q3,
  polys.trmm_sd,
  polys.agblongo_als_total,
  polys.agblongo_als_alive,
  polys.agblongo_tch_total,
  polys.agblongo_tch_alive,
  fbiomass_classes(polys.agblongo_tch_alive) as class,
  polys.geom
  FROM amazon_trmm polys INNER JOIN transects bb ON ST_Intersects(bb.polyflown, polys.geom);"
  
  getData(sql, createConnection(host))
}

#
# MAIN
#

amazonCells <- getAmazonCells()

#save(amazonCells, file = "E:\\heitor.guerra\\R_test\\amazonCells_amazon_trmm.RData")
load("E:\\heitor.guerra\\R_test\\amazonCells.RData")

bradypus <- subset(as.data.frame(amazonCells), select=c("class", "x", "y"))
predictors <- subset(amazonCells, select=c("evi_max", "evi_mean", "evi_median", "evi_min", "evi_q1", "evi_q3", "evi_sd", "ndvi_max", "ndvi_mean", "ndvi_median", "ndvi_min", "ndvi_q1", "ndvi_q3", "palsar_hh", "palsar_hhrhv1", "palsar_hv"))

# we do not need the first column
bradypus <- bradypus[,-1]
presvals <- extract(predictors, bradypus)


# setting random seed to always create the same
# random set of points for this example
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(predictors)), rep(0, nrow(predictors)))

sdmdata <- subset(amazonCells, select=c("class", "evi_max", "evi_mean", "evi_median", "evi_min", "evi_q1", "evi_q3", "evi_sd", "ndvi_max", "ndvi_mean", "ndvi_median", "ndvi_min", "ndvi_q1", "ndvi_q3", "palsar_hh", "palsar_hhrhv1", "palsar_hv"))
sdmdata[, "class"] = as.factor(sdmdata[, "class"])

jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
if (file.exists(jar)) {
  xm <- maxent(predictors, pres_train, factors='class')
  plot(xm)
} else {
  cat('cannot run this example because maxent is not available')
  plot(1)
 }





