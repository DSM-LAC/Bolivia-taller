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

#AUTOCORRELACION ESPACIAL
dat <- training
coordinates(dat) <- ~ x + y
class(dat)
dat@proj4string <- CRS(projargs = "+init=epsg:4326")
dat@proj4string
library(raster)


datdf <- dat@data
datdf <- datdf[, c("ARENA", names(covs))]
# Fit a multiple linear regression model between the log transformed
# values of ARENA and the top 20 covariates
model.MLR <- lm(log(ARENA) ~ ., data = datdf)
# stepwise variable selection
model.MLR.step <- step(model.MLR, direction="both")
# summary and anova of the new model using stepwise covariates
# selection
summary(model.MLR.step)
anova(model.MLR.step)

# Graphical diagnosis of the regression analysis
par(mfrow=c(2,2))
plot(model.MLR.step)

# Project point data
dat <- spTransform(dat, CRS("+init=epsg:6204"))
# Project covariates to VN-2000 UTM 48N
covs <- projectRaster(covs, crs = CRS("+init=epsg:6204"),
method='ngb')





