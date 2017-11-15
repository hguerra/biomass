require("raster")

# variables
map.path <- "G:\\Mapa_Biomassa_EBA\\EXTRAPOLACAO_2\\extrapolação_random_forest\\MAPAS"
map.no.trmm.path <- "random_forest_without_trmm.tif"
map.trmm.path <- "biomass_rf_map_M1_3trmm.tif"

# change working directory
setwd(map.path)

# loading maps as raster
map.no.trmm <- raster(map.no.trmm.path)
map.trmm <- raster(map.trmm.path)

# stats
map.no.trmm.summary <- summary(map.no.trmm)
map.no.trmm.raster.mean <- mean(map.no.trmm)
plot(map.no.trmm.raster.mean)

map.trmm.summary <- summary(map.trmm)
map.trmm.raster.mean <- mean(map.trmm)
plot(map.trmm.raster.mean)

# statistics across cells
map.no.trmm.mean <- cellStats(map.no.trmm, mean) # 18.14577
map.no.trmm.min <- cellStats(map.no.trmm, min) # 0.001629475
map.no.trmm.max <- cellStats(map.no.trmm, max) # 61.56808

map.trmm.mean <- cellStats(map.trmm, mean) # 17.99963
map.trmm.min <- cellStats(map.trmm, min) # 0.0009042446
map.trmm.max <- cellStats(map.trmm, max) # 64.25075

# histogram
hist(map.no.trmm, main="Distribution of biomass values without trmm", col= "red")
hist(map.trmm, main="Distribution of biomass values with trmm", col= "red")

