# https://www.r-bloggers.com/getting-started-with-postgresql-in-r/

# install.packages("RPostgreSQL")
require("RPostgreSQL")

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- {
  "postgres"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "eba",
                 host = "192.168.50.10", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

# check for the cartable
dbExistsTable(con, "amazon")
# TRUE

df <- data.frame(biomass=16.2)

# writes df to the PostgreSQL database "postgres", table "cartable" 
dbWriteTable(con, "amazon", 
             value = df, append = TRUE, row.names = FALSE)

# sends the command
dbGetQuery(con, "BEGIN;")
dbGetQuery(con, "UPDATE amazon SET biomass = 40 WHERE amazon_id = 1;")
dbGetQuery(con, "COMMIT;")

#
# amazon
#
updateDB <- function(mosaic_id) {
  transaction = 0
  
  for(i in 1:1000) {
    biomass <- runif(1, min=0, max=35)
    ogc_fid <- floor(runif(1, min=0, max=99999))
    
    if (transaction == 0) {
      #dbGetQuery(con, "BEGIN;")
    }
    
    sql <- paste(sqlIntro, biomass, sqlWhere, ogc_fid, sqlEnd, sep="")
    #dbGetQuery(con, sql)
    
    if (transaction == 500) {
      #dbGetQuery(con, "COMMIT;")
      print(sql)
      transaction = 0
    }
    
    transaction = transaction + 1
  }
  
  if (transaction > 0) {
    print("Closing remaining transactions...")
    #dbGetQuery(con, "COMMIT;")
  }
  
  rm(transaction)
}

#
# MAIN
#

sqlIntro <- "UPDATE amazon_trmm SET random_forest = "
sqlWhere <- " WHERE ogc_fid="
sqlEnd <- ";"

for(i in 1:30){
  print(paste("Running 'Random Forest' for mosaic", i, "..."))
  updateDB(i)
}

for (i in 1: 10) {
  print(i)
}

for (i in 11: 20) {
  print(i)
}

for (i in 21: 30) {
  print(i)
}
