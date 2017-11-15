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

getGrid <- function(host = "localhost") {
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
  amazon_trmm polys
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
cells.dump <- "E:\\heitor.guerra\\R_test\\cells.RData"

print("Querying data from PostgreSQL...")
cells <- getGrid()

print(paste("Saving Data Frame '", cells.dump, "' in disk"))
save(cells, file = cells.dump)
print("File saved!")

load(cells.dump)
print(head(cells))
