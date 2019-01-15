#IMPORTAMOS LAS COVARIABLES AMBIENTALES
library(raster)
covs <- stack("covs5km.tif")
names(covs) <- readRDS('worldgridsCOVS_names.rds')
#EXTRAEMOS A LOS DATOS DE LAS COVARIABLES A LOS PUNTOS 
e <- extract(covs, training[c('x','y')])
training <- cbind(training, data.frame(e))
#QUITAMOS VARIABLES CON MUCHOS HUECOS
 training$ln2dms3a <- NULL          
 training$lnmdms3a <- NULL
#QUITAMOS VALOES VACIOS 
 NA2mean <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
 training[] <- lapply(training, NA2mean)
#QUITAMOS VALORES NO REALES
training$ARENA[training$ARENA>65] <- 65
#QUITAMOS LOS CEROS
training$ARENA[training$ARENA==0] <- NA
training <- na.omit(training)

#PREPARAMOS COVARIABLES
x <- as(covs,'SpatialPixelsDataFrame')
x@data[] <- lapply(x@data, NA2mean)
covs <- raster::stack(covs)









