# https://gis.stackexchange.com/questions/39021/how-to-perform-random-forest-land-cover-classification

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 6) {
  stop("At least four arguments must be supplied. Arguments: raster path, train data name, model name and output file name", call.=FALSE)
}

print("Setting work directory...")
setwd(args[1])
datasource <- getwd()

result <- paste(datasource, "/result/", sep="")
dir.create(result, showWarnings = FALSE)

rf.train <- args[2]
rf.dump <- args[3]
rf.file <- args[4]
rf.select <- args[5]
rf.drop <- args[6]

print("Loading packages...")
require(sp)
require(rgdal)
require(raster)
require(randomForest)

# LOAD DATA FROM
datasource <- getwd()
rf.dump <- paste(result, rf.dump, sep="")
rf.file <- paste(result, rf.file, sep="")
rf.train.dump <- paste(rf.train, ".RData", sep="")

# CREATE LIST OF RASTERS
print("Listing rasters...")
rlist=list.files(datasource, pattern="\\.tif$", full.names=TRUE) 

# CREATE RASTER STACK
print("Creating a stack...")
xvars <- stack(rlist)      

# READ POINT SHAPEFILE TRAINING DATA
#print("Loading training data from RDATA...")
#load(rf.train.dump)

print(paste("Loading training data from shapefile '", rf.train, "'...", sep=""))
sdata <- readOGR(dsn=datasource, layer=rf.train)

# ASSIGN RASTER VALUES TO TRAINING DATA
print("Assigning raster values to training data...")
v <- as.data.frame(extract(xvars, sdata))
sdata@data <- data.frame(sdata@data, v[match(rownames(sdata@data), rownames(v)),])

# DUMP DATA
print("Saving training data...")
save(sdata, file = rf.train.dump)

# REMOVING UNNECESSARY DATA
print(paste("Removing column ", rf.drop, sep=""))
drops <- c("CHM", rf.drop)
sdata <- sdata[, !(names(sdata) %in% drops)]
sdata@data <- na.omit(sdata@data)

# RUN RF MODEL
print(paste("Running Random Forest model with ", rf.select,sep=""))
names(sdata)[names(sdata) == rf.select] <- "train"
rf.mdl <- randomForest(train~.,data = sdata@data, ntree=500, importance=TRUE)

#print("Saving model to disk...")
save(rf.mdl, file = rf.dump)
#load(rf.dump)

# CHECK ERROR CONVERGENCE
print("Checking error...")
plot(rf.mdl)
print(rf.mdl)

# PLOT mean decrease in accuracy VARIABLE IMPORTANCE
print("Plotting mean decrease in accuracy variable importance...")
varImpPlot(rf.mdl, type=1)

# PREDICT MODEL
print("Predicting model...")
predict(xvars, rf.mdl, filename=rf.file, type="response", index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

print("Done.")
