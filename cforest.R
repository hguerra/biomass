# https://gis.stackexchange.com/questions/39021/how-to-perform-random-forest-land-cover-classification
print("Loading packages...")
require(sp)
require(rgdal)
require(raster)
require(randomForest)
require(party)

print("Setting work directory...")
setwd("E:/heitor.guerra/db_backup/rasters/random_forest")
rf.dump <- "crf.mdl_1.RData"

# LOAD DATA FROM
datasource <- getwd()

# CREATE LIST OF RASTERS
print("Listing rasters...")
rlist=list.files(datasource, pattern="\\.tif$", full.names=TRUE) 

# CREATE RASTER STACK
print("Creating a stack...")
xvars <- stack(rlist)      

# READ POINT SHAPEFILE TRAINING DATA
print("Loading training data...")
sdata <- readOGR(dsn=datasource, layer="rf_train")

# ASSIGN RASTER VALUES TO TRAINING DATA
print("Assign raster values to training data...")
v <- as.data.frame(extract(xvars, sdata))
sdata@data <- data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])
sdata@data <- na.omit(sdata@data)

# RUN RF MODEL
print("Running Random Forest model...")
#rf.mdl <- randomForest(x=sdata@data[,2:ncol(sdata@data)], y=as.factor(sdata@data[,"TRAIN"]), ntree=500, importance=TRUE)

# rf.mdl <- cforest(as.factor(TRAIN) ~ 
#                  X17_evi_max + X17_evi_mean + X17_evi_median + X17_evi_min + X17_evi_q1 + X17_evi_q3 + X17_evi_sd + 
#                  X17_ndvi_max + X17_ndvi_mean + X17_ndvi_median + X17_ndvi_min + X17_ndvi_q1 + X17_ndvi_q3 + X17_ndvi_sd + 
#                  X17_trmm_max +X17_trmm_mean + X17_trmm_median + X17_trmm_min + X17_trmm_q1 + X17_trmm_q3 + X17_trmm_sd +
#                  X17_palsar_hh + X17_palsar_hv + X17_srtm,
#                data = sdata@data, 
#                controls=cforest_unbiased(ntree=500))

# rf.mdl <- cforest(TRAIN~., data = sdata@data, controls=cforest_unbiased(ntree=501))

print("Saving model to disk...")
#save(rf.mdl, file = rf.dump)
load(rf.dump)

# CHECK ERROR CONVERGENCE
print("Checking error...")
plot(rf.mdl)

# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
print("Plotting mean decrease in accuracy variable importance...")
varImpPlot(rf.mdl, type=1)

# PREDICT MODEL
print("Predicting model...")
predictions <- predict(xvars, rf.mdl, filename="cforest_17.tif", type="response", index=1, na.rm=TRUE, progress="trace", overwrite=TRUE)

print("Calculating root mean square error (RMSE)...")
RMSE <- sqrt(sum((predictions - sdata$TRAIN)^2)/length(predictions))
print(RMSE)

RMSEPerMean <- RMSE/mean(sdata$TRAIN)
percentageRMSE <- RMSEPerMean * 100
print(paste("RMSE is only about ", percentageRMSE, "% (", RMSEPerMean,") as large as the mean of our outcome", sep=""))

print("Done.")
