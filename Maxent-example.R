#'##############################################################################################################
#'# R scripts from "An Introduction to Best Practices in Species Distribution Modeling" ########################
#'# from Adam B. Smith || Missouri Botanical Garden || 2012-10-15  #############################################
#'# modified and extended by R. Castilho || CCMAR || 2014-09-15  ###############################################
#'# modified and extended by M. Gandra || UALG || 2015-07-10  ##################################################
#'##############################################################################################################

#' MAXENT (Maximum Entropy Modelling)          

#' Goals:
#' 1. Demonstrate how to handle spatial data (points, shapefiles, and rasters) in R
#' 2. Demonstrate how to prepare species' and environmental data
#' 3. Demonstrate "raster-points" method for training Maxent model in R
#' 4. Make a map of predictions


#######################################################################################################
## Read dependences ###################################################################################

# install Sun's Java JRE from www.oracle.com/technetwork/java/javase/downloads/
# download Maxent (www.cs.princeton.edu/~schapire/maxent/)
# use your file system to cut and paste the Maxent jar file into the folder returned by this command:
# system.file('java', package='dismo')

# Sys.setenv(NOAWT=TRUE)
# only for certain macs before loading rJava

# options(java.parameters = "-Xmx1g" )
# optional, to set the memory allocated to java, hence maxent (has to be done before loading dismo)


#######################################################################################################
## Check maxent.jar file ##############################################################################
## only proceed if the maxent.jar file is available, on the right folder

jar <- file.path(system.file(package="dismo"), "/java/maxent.jar")
if (file.exists(jar)==FALSE) {stop("maxent.jar file not available")}

  
#######################################################################################################
## Automatically install required libraries  ##########################################################
# (check http://tlocoh.r-forge.r-project.org/mac_rgeos_rgdal.html
# if rgeos and rgdal installation fail on a Mac)

if(!require(rJava)){install.packages("rJava"); library(rJava)}
if(!require(dismo)){install.packages("dismo"); library(dismo)}
if(!require(rgeos)){install.packages("rgeos"); library(rgeos)}
if(!require(rgdal)){install.packages("rgdal"); library(rgdal)}
if(!require(raster)){install.packages("raster"); library(raster)}
if(!require(sp)){install.packages("sp"); library(sp)}
if(!require(rgl)){install.packages("rgl"); library(rgl)}
if(!require(virtualspecies)){install.packages("virtualspecies"); library(virtualspecies)}

# set the directory containing the data files - user's desktop SDM folder predefined
setwd(file.path(path.expand('~'),'Desktop/SDM'))


#######################################################################################################
## Load species occurrences ###########################################################################

# run script Occurrence_data.R
# file.edit('./code/Occurrence_data.R')

# convert sample points to spatial points 
spoints <- SpatialPoints(cbind(x.thinned,y.thinned))
projection(spoints) <- "+proj=longlat +ellps=WGS84 +datum=WGS84"


#######################################################################################################
## Load predictor rasters #############################################################################

# Note that using formats other than ASCII (as used here) can save on memory... 
# 1. Download rasters from http://www.oracle.ugent.be/DATA/90_90_ST/BioOracle_9090ST.rar
# 2. Place rasters in folder SDM/data/Oracle
# 3. Download rasters from http://esapubs.org/archive/ecol/E094/086/MARSPEC_LowResFiles/MARSPEC_5m.7z
# 2. Place uncompressed folder in SDM/data

list.rasters<-(list.files("./data/Oracle", full.names=T, pattern=".asc"))
list.rasters <- c(list.rasters, "./data/MARSPEC_5m/Core_Variables_5m/bathy_5m/w001001.adf")
list.rasters <- c(list.rasters, "./data/MARSPEC_5m/Core_Variables_5m/biogeo05_5m/w001001.adf")
list.rasters

# make raster "stack" with raster for each predictor 
rasters <- stack(list.rasters)

# set missing predictor names
names(rasters[[24]]) <- 'bathymetry'
names(rasters[[25]]) <- 'shoredist'
names(rasters)

# set the coordinate reference system for the raster stack
# this is not absolutely necessary if the rasters are unprojected (e.g., WGS84),
# but we'll do it to avoid warning messages below
projection(rasters) <- CRS("+proj=longlat +datum=WGS84")

# crop rasters from script Occurrence_data.R
xmin=min(x.thinned)-5
xmax=max(x.thinned)+5
ymin=min(y.thinned)-5
ymax=max(y.thinned)+5
limits <- c(xmin, xmax, ymin, ymax)
rasters.crop <- crop(rasters,limits)


#######################################################################################################
## Plot each raster with the sampling points ##########################################################

k <- length(list.rasters)
par(mar = c(0.1, 0.1, 0.1, 0.1), mfrow = c(5,5)) 
for (i in 1:k) {
  plot(rasters.crop[[i]],axes=FALSE,legend=FALSE, asp = 1 ,maxnl=23)
  points(spoints, col='red', pch=20, cex=0.4)
  mtext(names(rasters.crop[[i]]), side=4, font=0, line=0, cex=0.6)
}

par(mfrow=c(1,1))


#######################################################################################################
## Co-linearity between  predictors ###################################################################


rasters.crop.reduced <- removeCollinearity(rasters.crop, multicollinearity.cutoff = 0.85, plot= TRUE,
                                           select.variables = FALSE, sample.points = FALSE)


groups <- which(sapply(rasters.crop.reduced, length)>1)
groups
rasters.crop.reduced[groups]

# manually select predictors to retain from each group
rasters.crop.reduced[2]<-'chlomean'
rasters.crop.reduced[6]<-'sstmax'
rasters.crop.reduced[7]<-'sstmin'
rasters.crop.reduced[8]<-'nitrate'

rasters.crop.reduced <- unlist(rasters.crop.reduced)

# subset selected layers from the raster stack
rasters.selected <- subset(rasters.crop, rasters.crop.reduced)


#######################################################################################################
## Extract values from rasters ########################################################################

# presence values
presvals <- extract(rasters.selected, spoints)

# setting random seed to always create the same random set of points for this example
set.seed(1963)

# background values
backgr <- randomPoints(rasters.selected, 1000)
absvals <- extract(rasters.selected, backgr)

# create dataframe with presence/background values 
# the first column (variable ’pb’) indicates whether this is a presence or a background point
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata.present <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmdata.present <- sdmdata.present[complete.cases(sdmdata.present),]


#######################################################################################################
## Generalized linear model (GLM) #####################################################################

# a significant intercept in a model only means that there is also a constant in the model, 
# in addition to all other explanatory variables.
model.present <- glm(pb ~ .,data=sdmdata.present) 
summary(model.present)

# automatically selects significant variables
significant.vars <- summary(model.present)$coeff[-1,4] < 0.05
significant.vars <- names(significant.vars)[significant.vars == TRUE] 
significant.vars

# manually add or delete variables to customize predictors set
significant.vars <- c(significant.vars, 'sstmin')
significant.vars<-significant.vars[-c(3,4,7)]
significant.vars

reduced.sdmdata.present <- subset(sdmdata.present, select=c('pb', significant.vars))

# run the regression with only significant variables
reduced.model.present<-glm(pb ~ .,data=reduced.sdmdata.present)
summary(reduced.model.present)


#######################################################################################################
## Model selection - AIC (Akaike information criterion) ###############################################
# the preferred model is the one with the minimum AIC value

aic <- AIC(model.present, reduced.model.present)
aic
selected.model <- row.names(aic)[which.min(aic$AIC)]
selected.model


#######################################################################################################
## Maxent model #######################################################################################

# Call Maxent using the "raster-points" format (x=a raster stack and p=a two-column matrix of coordinates).
# Only x and p are really required, but I'm showing many of the commands in case you want to tweak some later.  

# All of the "args" are set to their default values except for "randomtestpoints" which says to randomly 
# select 30% of the species' records and use these as test sites (the default for this is 0). 

# All other arguments are set to the defaults except "threads" which can be set up to the number of cores 
# you have on your computer to speed things up... to see more "args" see bottom of "Help" file from the Maxent program.


if(selected.model=='reduced.model.present'){rasters.final<-subset(rasters.selected, significant.vars)}
if(selected.model=='model.present') {rasters.final<-rasters.selected} 


model.maxent <- maxent(
	x=rasters.final,
	p=spoints,
  a=backgr,
	args=c(
		'randomtestpoints=30',
		'betamultiplier=1',
		'linear=true',
		'quadratic=true',
		'product=true',
		'threshold=true',
		'hinge=true',
		'threads=2',
		'responsecurves=true',
		'jackknife=true',
		'askoverwrite=false'
	)
)

# look at model output (HTML page)
model.maxent

# variable contribution
plot(model.maxent)

# model evaluation
evaluate(model.maxent, x=rasters.final, p=spoints, a=backgr)


#######################################################################################################
## Maxent prediction ##################################################################################

# Note that you could save the prediction raster in any number of formats (see ?writeFormats),
# but GeoTiffs are small and can be read by ArcMap... 
# ASCIIs can also be read by other programs but are large... 
# The default raster format (GRD) sometimes can't be read by ArcMap...
# If you don't specifiy a file name then the results are written to R's memory

global <- extent(-180, 180, -90, 90)

rasters.pred <- subset(rasters, rasters.crop.reduced)
rasters.pred <- subset(rasters.pred, significant.vars)

map.maxent <- predict(
  object=rasters.pred,
  model=model.maxent,
	na.rm=TRUE,
	#format='GTiff',
  #filename= "./results/Prediction_raster.GTiff",
	#overwrite=TRUE,
	progress='text',
  ext=global
)


pdf("./results/Prediction_global.pdf", width = 16, height = 9)
plot(map.maxent, mar=c(0,0,0,0), zlim = c(0,1), axes=F, maxpixels=ncell(map.maxent))
#scalebar(d=2000, xy=c(140, -60), type="bar", below="km", lonlat=TRUE, cex=0.7)
dev.off()

pdf("./results/Prediction_original.pdf")
plot(map.maxent, mar=c(0,0,0,0), zlim = c(0,1), axes=F, maxpixels=ncell(map.maxent), ext=limits)
points(x, y, pch=21, col=adjustcolor("black", alpha=0.6), bg=adjustcolor("blue", alpha=0.8), cex=0.6, lwd=0.2) 
dev.off()

e1<-extent(130, 180, -55, -20)
pdf("./results/Prediction_local1.pdf")
plot(map.maxent, mar=c(0,0,0,0), zlim = c(0,1), axes=F, maxpixels=ncell(map.maxent), ext=e1)
dev.off()

e2<-extent(-85, -48, -58, -25)
pdf("./results/Prediction_local2.pdf")
plot(map.maxent, mar=c(0,0,0,0), zlim = c(0,1), axes=F, maxpixels=ncell(map.maxent), ext=e2)
dev.off()


################################################################################################################
################################################################################################################
################################################################################################################