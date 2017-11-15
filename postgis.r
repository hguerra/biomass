# https://www.rdocumentation.org/packages/rpostgis/versions/1.3.0
require("rpostgis")

createConnection <- function(host="150.163.58.218", port="5432", user="eba", password="ebaeba18", dbname="eba") {
  dbConnect("PostgreSQL",dbname=dbname,host=host,port=port,user=user,password=password)
}

loadFromPostGIS <- function(table, connection, clauses=NULL, geom="geom") {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  pgGetGeom(connection, name=c("public", table), geom=geom, clauses=clauses)
}

getData <- function(query, connection) {
  if (missing(connection)) {
    connection = createConnection()
  }
  
  pgGetGeom(connection, query = query)
}

#
# MAIN
#

agb_average_spf <- loadFromPostGIS("agb_average")
print(length(agb_average_spf))

agb_average_spf_limited <- loadFromPostGIS("agb_average", clauses="LIMIT 10")
print(length(agb_average_spf_limited))
