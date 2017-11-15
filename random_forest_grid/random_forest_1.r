print("Loading packages...")
require("randomForest")
require("raster")
require("sp")
require("rgdal")
require("RPostgreSQL")

print("Creating functions...")
createConnection <- function(host="150.163.58.218", user="eba", password="ebaeba18", dbname="eba") {
  dbConnect(PostgreSQL(), host = host, user = user, password = password, dbname = dbname)
}

query <- function(sql, connection) {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  rs <- dbSendQuery(connection, sql)
  fetch(rs, n = -1)
}

getGrid <- function(con, id) {
  sqlIntro <- "
  SELECT
    ogc_fid,
    st_x(st_centroid(geom)) AS x,
    st_y(st_centroid(geom)) AS y,
    evi_max,
    evi_mean,
    evi_median,
    evi_min,
    evi_q1,
    evi_q3,
    ndvi_max,
    ndvi_mean,
    ndvi_median,
    ndvi_min,
    ndvi_q1,
    ndvi_q3,
    palsar_hh,
    palsar_hhrhv1,
    palsar_hv
  FROM amazon_trmm
  WHERE
    mosaic_id ="
  
  sqlEnd <- "AND evi_max IS NOT NULL AND evi_mean IS NOT NULL AND evi_median IS NOT NULL AND evi_min IS NOT NULL AND
  evi_q1 IS NOT NULL AND evi_q3 IS NOT NULL AND ndvi_max IS NOT NULL AND ndvi_mean IS NOT NULL
  AND ndvi_median IS NOT NULL AND ndvi_min IS NOT NULL AND ndvi_q1 IS NOT NULL AND ndvi_q3 IS NOT NULL AND
  palsar_hh IS NOT NULL AND palsar_hhrhv1 IS NOT NULL AND palsar_hv IS NOT NULL;"
  
  sql <- paste(sqlIntro, id, sqlEnd)
  query(sql, con)
}

getMap <- function(cells, select=c("evi_max", "evi_mean", "evi_median", "evi_min", "evi_q1", "evi_q3", "ndvi_max", "ndvi_mean", "ndvi_median", "ndvi_min", "ndvi_q1", "ndvi_q3", "palsar_hh", "palsar_hhrhv1", "palsar_hv")) {
  rf.newdata <- subset(cells, select=select)

  print("Predicting using Random Forest")
  map <- predict(object=rf.M2, newdata=rf.newdata,type='response')
  
  print("Removing temp objects...")
  rm(rf.newdata)
  
  print("Retrieving map")
  map
}

getBiomassValues <- function(con, id, select=c("ogc_fid", "biomass")) {
  cells <- getGrid(con, id)
  cells$biomass <- getMap(cells)
  biomass <- subset(cells, select=select)
  
  print("Removing temp objects...")
  rm(cells)

  print("Retrieving biomass values")
  biomass
}

updateDB <- function(con, values) {
  transaction = 0

  for(i in 1:nrow(values)) {
    row <- values[i,]
    if (transaction == 0) {
      dbGetQuery(con, "BEGIN;")
    }
    
    sql <- paste(sqlIntro, row$biomass, sqlWhere, row$ogc_fid, sqlEnd, sep="")
    dbGetQuery(con, sql)
    
    transaction = transaction + 1
    if (transaction == 1000) {
      dbGetQuery(con, "COMMIT;")
      transaction = 0
    }
  }
  
  if (transaction > 0) {
    print("Closing remaining transactions...")
    dbGetQuery(con, "COMMIT;")
  }
  
  rm(transaction)
}

#
# MAIN
#

modelPath <- "E:\\heitor.guerra\\R_test\\extrapolacao\\modeltchA_Rf_without_trmm.RData"

sqlIntro <- "UPDATE amazon_trmm SET random_forest = "
sqlWhere <- " WHERE ogc_fid="
sqlEnd <- ";"

print(paste("Loading model '", modelPath, "' from disk"))
load(modelPath)

connection <- createConnection("localhost")
for (i in 17:17) {
  print(paste("Running 'Random Forest' for mosaic", i, "..."))
  biomass <- getBiomassValues(connection, i)
  updateDB(connection, biomass)
}
