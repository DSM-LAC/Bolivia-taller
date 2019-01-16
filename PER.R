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
#ELIMINO EL PERFIL Lo44 
dat <- dat[!dat$CODIGO=='Lo44',]
#
dat <- dat[!is.na(dat$bottom),]

# Y dos perfiles tienen los valores erroneos porque tenian 80-60
dat[dat$bottom <= dat$top,]

# los quitamos
dat <- dat[!dat$bottom <= dat$top,]

library(aqp)

depths(dat) <- IDPROF ~ top + bottom

# diferenciamos atributos de perfil de atributos de horizontes
site(dat) <- ~ longitude + latitud 

# Especificamos las coordenadas espaciales
coordinates(dat) <- ~ longitude + latitud 
dat@sp@proj4string <- CRS("+init=epsg:4326")

plot(dat@sp)


library(GSIF)

try(SOC <- mpspline(dat, 'SOC', d = t(c(0,30))))

training <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  SOC = SOC$var.std[,1])

#SELECCIONA SOLAMENTE LOS DATOS SUPERFICIALES
#dat0 <- dat[dat$top==0,]
#training <- dat0[c(2,3,6)]
#str(dat0)

####
####HASTA AQUI

#IMPORTAMOS LAS COVARIABLES AMBIENTALES
library(raster)
covs <- stack("PER_worldgridsCOVS_5km.tif")
#covs <- aggregate(covs, 5, mean)
names(covs) <- readRDS('worldgridsCOVS_names.rds')
#EXTRAEMOS A LOS DATOS DE LAS COVARIABLES A LOS PUNTOS 
e <- extract(covs, training[c('x','y')])
training <- cbind(training, data.frame(e))

training <- na.omit(training)
#QUITAMOS LAS VARIABLES CATEGORICAS
cat1 <- grep('igb', names(training))
		cat2 <- grep('esa', names(training))		
		cat <- c(cat1, cat2)
t <- na.omit(training[-cat])

#ENCONTRAMOS LAS VARIABLES MEJOR CORRELACIONADAS CON SOC
#SOC es la varianle 3, las coordenadas son 1 y 2
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
#INCLUIMOS SOC A train
train$SOC <- training$SOC
train <- na.omit(train)
COVS <- covs[[idx]]
#QUITAMOS CEROS EN CARBONO
train[train$SOC==0,] <- NA
train <- na.omit(train)
#QUITAMOS VALORES EXTREMOS DE CARBONO
train[train$SOC==60,] <- 60
#ENTRENAMOS UN MODELO LINEAL
model.MLR <- lm(log1p(SOC) ~ ., data = train)
predLM <- predict(COVS, model.MLR)
#ENTRENAMOS UN MODELO NO LINEAL (Random Forests)
library(randomForest)
arbolReg <- randomForest(log1p(SOC)~., train)
predRF <- predict(COVS, arbolReg)
#VISUALIZAMOS PREDICCIONES
library(rasterVis)
levelplot(predLM)
x11()
levelplot(predRF)
#GUARDA LOS MAPAS EN TIF
writeRaster(predRF, file='prediccionArbolRegSOClog1p.tif')
writeRaster(predLM, file='prediccionLinearModelSOClog1p.tif')
#####
#####HASTA AQUI



