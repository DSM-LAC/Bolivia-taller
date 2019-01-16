
####PREPARE DATASETS
library(aqp)
library(rgdal)
#CARGA LOS DATOS
dat <- read.csv("~/Downloads/NoSilverBulletOnDSM-2.0/Peru/Base de datos Ambo_01.csv")
#SELECCIONAMOS LAS COLUMNAS DE INTERES
names(dat)
dat <- dat[c(1:3, 11:12, 15)]
head(dat)
#ELIMINA CELDAS SIN DATOS
dat <- na.omit(dat)
#DEFINE PROFUNDIDADES
depths(dat) <- Calicata ~ LS + LI

# diferenciamos atributos de perfil de atributos de horizontes
site(dat) <- ~ X + Y

library(rgdal)

# Especificamos las coordenadas espaciales
coordinates(dat) <- ~ X + Y
dat@sp@proj4string <- CRS('+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')


plot(dat@sp)

########### Splines #####################3

library(GSIF)

try(CE <- mpspline(dat, 'CE', d = t(c(0,30))))

training <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  CE = CE$var.std[,1])


####PREPARE COVARIATES
library(raster)

DEM <- raster('DEM_AMBO_10m_AP.tif')

lito <- shapefile('Litologico/Litologia.shp')
fisio <- shapefile('Fisiografia/Fisiografia.shp')
zvida <- shapefile('Zonas de Vida/Z_Vida.shp')
ero  <- shapefile('Erosion/Erosion.shp')
geo  <- shapefile('Geologia/Geologia.shp')
uat  <- shapefile('UAT/UAT.shp')

#the "Symbol" attribute from the vector layer will be used for the
#rasterization process. It has to be a factor
lito@data$Simb_Lito <- as.factor(lito@data$Simb_Lito)
fisio@data$SIMB <- as.factor(fisio@data$SIMB)
zvida@data$SIMB_ZV <- as.factor(zvida@data$SIMB_ZV)
ero@data$SIM_EROSIO <- as.factor(ero@data$SIM_EROSIO)
geo@data$Simb_Geol <- as.factor(geo@data$Simb_Geol)
uat@data$SIMB_UAT <- as.factor(uat@data$SIMB_UAT)

# The rasterization process needs a layer with the target grd
# system: spatial extent and cell size.
lito.r <- rasterize(x = lito, y = DEM, field = "Simb_Lito")
fisio.r <- rasterize(x = fisio, y = DEM, field = "SIMB")
zvida.r <- rasterize(x = zvida, y = DEM, field = "SIMB_ZV")
ero.r <- rasterize(x = ero, y = DEM, field = "SIM_EROSIO")
geo.r <- rasterize(x = geo, y = DEM, field = "Simb_Geol")
uat.r <- rasterize(x = uat, y = DEM, field = "SIMB_UAT")

COVS <- stack(DEM, lito.r, fisio.r, zvida.r, ero.r, geo.r, uat.r)
names(COVS) <- c('DEM', 'lito', 'fisio', 'zvida', 'ero', 'geo', 'uat')

####MODELING
#EXTRAE VALORES DE COVARIABLES A LOS PUNTOS
e <- extract(COVS, training[c('x','y')])
train <- cbind(training, data.frame(e))
##VAYAN A MODELING
#####HASTA AQUI
library(caret)
##DEFINE LOS CONTROLES DE VALIDACION CRUZADA DEL MODELO
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 5,
			   savePredictions = TRUE,
                           repeats = 5)
                           
#VALIDACION CRUZADA DE AJUSTE NO LINEAL
set.seed(825)

(ajusteRandomForest <- train(CE ~ ., data = train[-c(1,2)], 
                 method = "rf", 
                 trControl = fitControl,
                 verbose = FALSE))
                 
           ....




