#IMPORTAMOS LAS COVARIABLES AMBIENTALES
library(raster)
covs <- stack("covs5km.tif")
names(covs) <- readRDS('worldgridsCOVS_names.rds')
#EXTRAEMOS A LOS DATOS DE LAS COVARIABLES A LOS PUNTOS 
e <- extract(covs, training[c('x','y')])
training <- cbind(training, data.frame(e))
#QUITAMOS VALORES POR ENCIMA DE 65% DE ARENA
training$ARENA[training$ARENA>65] <- 65
#QUITAMOS LOS CEROS
training$ARENA[training$ARENA==0] <- NA
training <- na.omit(training)
#QUITAMOS LAS VARIABLES CATEGORICAS
cat1 <- grep('igb', names(training))[1:6]
		cat2 <- grep('esa', names(training))[23]		
		cat <- c(cat1, cat2)
t <- na.omit(training[-cat])
#ENCONTRAMOS LAS VARIABLES MEJOR CORRELACIONADAS CON LA ARENA
COR <- cor(as.matrix(t[,3]), as.matrix(t[-c(1, 2, 3)]))
library(reshape)
x <- subset(melt(COR), value != 1 | value != NA)
x <- x[with(x, order(-abs(x$value))),]
		names(x)[1] <- 'country'
		names(x)[2] <- 'predictor'
		names(x)[3] <- 'correlation'
bestCor <- data.frame(country = character(), predictor = character(), 
correlation = numeric())
bestCor$country <- as.character(bestCor$country)
bestCor$predictor <- as.character(bestCor$predictor)
bestCor <- rbind (bestCor, x[1:10,])			
idx <- as.character(x$predictor[1:10])	
print(bestCor)
#NOS QUEDAMOS SOLAMENTE CON LAS VARIABLES MEJOR CORRELACIONADAS
train <- training[idx]
train$ARENA <- training$ARENA
train <- na.omit(train)
COVS <- covs[[idx]]
#ENTRENAMOS UN MODELO LINEAL
model.MLR <- lm(log(ARENA) ~ ., data = train)
predLM <- predict(COVS, model.MLR)
#ENTRENAMOS UN MODELO NO LINEAL
arbolReg <- randomForest(ARENA~., train)
predRF <- predict(COVS, arbolReg)
#VISUALIZAMOS PREDICCIONES
library(rasterVis)
plot(exp(mapLM))
plot(predRF)
#
#####
#####HASTA AQUI



set.seed(102)
ctrl <- trainControl(savePred=T, method="repeatedcv", number=5, repeats=5)

cl <- makeCluster(detectCores(), type='SOCK')
registerDoParallel(cl)
models <- caretList(training[-11], training[,11], trControl=ctrl ,
methodList=c("rf", "svmLinear"))
ens <- caretEnsemble(models)
stopCluster(cl = cl)





















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

#
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
mapLM <- predict(model.MLR, covs)

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





