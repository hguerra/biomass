print("Loading package 'RPostgreSQL'...")
require("RPostgreSQL")

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

getTransects <- function(host = "localhost") {
  sql <- "
  SELECT
  polys.ogc_fid,
  st_x(st_centroid(polys.geom)) AS x,
  st_y(st_centroid(polys.geom)) AS y,
  fbiomass_classes(polys.agblongo_tch_alive) as class_tch_alive,
  polys.agblongo_als_total,
  polys.agblongo_als_alive,
  polys.agblongo_tch_total,
  polys.agblongo_tch_alive,
  polys.chm,
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
  amazon_trmm polys INNER JOIN transects bb ON ST_Intersects(bb.polyflown, polys.geom);"

  query(sql, createConnection(host))
}

#
# MAIN
#
transects.dump <- "E:\\heitor.guerra\\R_test\\transects_chm.RData"

print("Querying data from PostgreSQL...")
transects <- getTransects()

print(paste("Saving Data Frame '", transects.dump, "' in disk"))
save(transects, file = transects.dump)
#load(transects.dump)

chm <- subset(transects, select=c("chm"))
