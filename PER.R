setwd("~/Downloads/NoSilverBulletOnDSM-2.0/Peru")

library(rgdal)
library(raster)
#CARGA LOS DATOS
dat <- read.csv("alldata_Peru.csv")
#LIMITE DE PAIS
#QUITA NA
dat <- na.omit(dat)
#GENERA ID COLUMN
dat$IDPROF <- paste0("IDPROF_", dat$longitude, "_", dat$latitud)

#SELECCIONA SOLAMENTE LOS DATOS SUPERFICIALES
dat0 <- dat[dat$top==0,]
training <- dat0[c(2,3,6)]
str(dat0)

#IMPORTAMOS LAS COVARIABLES AMBIENTALES
library(raster)
covs <- stack("PER_worldgridsCOVS.tif")
names(covs) <- readRDS('worldgridsCOVS_names.rds')
#EXTRAEMOS A LOS DATOS DE LAS COVARIABLES A LOS PUNTOS 
e <- extract(covs, training[c('longitude','latitud')])
training <- cbind(training, data.frame(e))

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
#ENTRENAMOS UN MODELO NO LINEAL (Random Forests)
arbolReg <- randomForest(ARENA~., train)
predRF <- predict(COVS, arbolReg)
#VISUALIZAMOS PREDICCIONES
library(rasterVis)
plot(exp(mapLM))
plot(predRF)
#GUARDA LOS MAPAS EN TIF
writeRaster(predRF, file='prediccionArbolRegARENA.tif')
writeRaster(exp(mapLM) , file='prediccionLinearModelARENA.tif')
#####
#####HASTA AQUI



