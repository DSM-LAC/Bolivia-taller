

setwd("~/Downloads/NoSilverBulletOnDSM-2.0/Paraguay")

library(rgdal)
library(raster)
#CARGA LOS DATOS
dat <- read.csv("Datos Carbono  Completo PY.csv")
#LIMITE DE PAIS
lim  <- readRDS('gadm36_PRY_2_sp.rds')
#QUITA LOS DATOS FUERA DE PRY
d <- dat
coordinates(d) <- ~longitude + latitude
proj4string(d) <- CRS(projection(lim))
d <- d[lim,]
dat <- as.data.frame(d)
#QUITA NA
dat <- na.omit(dat)
#GENERA ID COLUMN
dat$IDPROF <- paste0("IDPROF_", dat$longitude, "_", dat$latitude)

library(aqp)
#SOIL PROFILE COLLECTION
depths(dat) <- IDPROF ~ top + botton
#VISUALIZA
plot(dat[1:50], color='X..SOC')
#DEFINE SITIO Y FALLA
site(dat) <- ~ longitude + latitude
# Especificamos las coordenadas espaciales
coordinates(dat) <- ~ longitude + latitude
dat@sp@proj4string <- CRS("+init=epsg:4326")

plot(dat@sp)
library(GSIF)

try(SOC <- mpspline(dat, 'SOC', d = t(c(0,30))))

training <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  SOC = SOC$var.std[,1])


########ESCRIBE TU CODIGO AQUI


