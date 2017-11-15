#############################################################################
# Imports
source("taxonomy.R")

#############################################################################
# Load Dataset
forest = read.table("E:/heitor.guerra/PycharmProjects/pyLiDARForest-util/db/simple_plotdata_ddl/data/TLO_2015_RESUMO.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, stringsAsFactors = FALSE)
# forest = read.table("E:/heitor.guerra/PycharmProjects/pyLiDARForest-util/db/simple_plotdata_ddl/data/JARAUA_2017_RESUMO.csv", header = TRUE, sep = ";", quote = "\"", dec = ",", fill = TRUE, stringsAsFactors = FALSE)

#############################################################################
# WOOD DENSITY
forest <- getTaxonomyFromDB(forest)
forest <- forest[!is.na(forest$DAP),] # Remove rows without diameter measurements.
forest <- forest[is.na(forest$Morta),] # Remove dead trees.

#############################################################################
# TREE HEIGHT

# Compare different local H-D models
HDmodel <- modelHD(D = forest$DAP, H = forest$Altura, drawGraph = TRUE, useWeight = TRUE)

# Compute the local H-D model with the lowest RSE
HDmodel <- modelHD(D = forest$DAP, H = forest$Altura, method = "log2", useWeight = TRUE)

# Compute models specific to given stands
HDmodelPerPlot <- by(forest, forest$Parcela, function(x) modelHD(D = x$DAP, H = x$Altura, method = "weibull", useWeight = T), simplify = FALSE)                  
RSEmodels <- sapply(HDmodelPerPlot, function(x) x$RSE)
Coeffmodels <- lapply(HDmodelPerPlot, function(x) x$coefficients)

# Retrieve height data from a local HD model
dataHlocal <- retrieveH(D = forest$DAP, model = HDmodel)

# Retrieve height data from a Feldpaush et al. (2012) averaged model
dataHfeld <- retrieveH(D = forest$DAP, region = "ECAmazonia")

forest$H = dataHlocal$H
forest$Hfeld = dataHfeld$H
forest$HfeldRSE = dataHfeld$RSE

#############################################################################
# AGB CALCULATION

# Compute AGB(Mg) per tree
AGBtree <- computeAGB(D = forest$DAP, WD = forest$density, H = forest$H)

# Compute AGB(Mg) per tree with Feldpausch et al. (2012)
AGBtreeFeld <- computeAGB(D = forest$DAP, WD = forest$density, H = forest$Hfeld)

# Compute AGB(Mg) per plot
AGBPlotList <- by(forest, forest$Parcela, function(x) computeAGB(D = x$DAP, WD = x$density, H = x$H), simplify = F)
AGBplot <- sapply(AGBPlotList, sum) 

# Compute AGB(Mg) per tree with Feldpausch et al. (2012) regional H-D model
AGBPlotListFeld <- by(forest, forest$Parcela, function(x) computeAGB(D = x$DAP, WD = x$density, H = x$Hfeld), simplify = F)
AGBplotFeld <- sapply(AGBPlotListFeld, sum) 

forest$agb <- AGBtreeFeld
write.table(forest, file = "TLO_2015_RESUMO.csv", append = FALSE, quote = TRUE, sep = ";",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")
#############################################################################
# PROPAGATING ERRORS

# Per plot using the Feldpaush regional HD averaged model
resultMC <- by(forest, forest$Parcela, function(x) AGBmonteCarlo(D = x$DAP, WD = x$density, errWD = x$densitySD, H = x$Hfeld, errH = x$HfeldRSE, Dpropag = "chave2004"), simplify = F)
meanAGBperplotFeld <- unlist(sapply(resultMC, "[", 1))
credperplotFeld <- sapply(resultMC, "[", 4)
