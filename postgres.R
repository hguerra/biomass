# https://cran.r-project.org/web/packages/RPostgreSQL/RPostgreSQL.pdf
# https://cran.r-project.org/web/packages/sf/
require("RPostgreSQL")

# Cria uma conexao com BD
con <- dbConnect(PostgreSQL(), host= "150.163.58.218", user= "eba", password="ebaeba18", dbname="eba")

query <- "
SELECT
  filename,
  index_,
  x,
  y,
  all_,
  min,
  max,
  avg,
  qav,
  std,
  ske,
  kur,
  p01,
  p05,
  p10,
  p25,
  p50,
  p75,
  p90,
  p95,
  p99,
  b10,
  b20,
  b30,
  b40,
  b50,
  b60,
  b70,
  b80,
  b90,
  c00,
  c01,
  c02,
  c03,
  c04,
  c05,
  c06,
  c07,
  d00,
  d01,
  d02,
  d03,
  d04,
  d05,
  d06,
  cov_gap,
  dns_gap,
  chm,
  agblongo_als_total,
  agblongo_als_alive,
  agblongo_tch_total,
  agblongo_tch_alive,
  ST_AsText(geom) as geom
FROM
  metrics
LIMIT 10;
"

# Realiza a consulta em SQL
rs <- dbSendQuery(con, query)

# Trasforma a consulta em um DataFrame
# CUIDADO: o programa pode ficar lento caso sua consulta traga muitos valores.
df <- fetch(rs, n = -1)
df
