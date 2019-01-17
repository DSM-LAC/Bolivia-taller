#DIRECTORIO DE TRABAJO
setwd("~/Downloads/NoSilverBulletOnDSM-2.0/Bolivia/BOLIVIA/")
#LIBRERIAS DE TRABAJO
library(raster)
library(rgdal)
#IMPORTAMOS LOS DATOS DISPONIBLES
dat1 <- read.csv("hoja1.csv")
dat3 <- read.csv("hoja3.csv")
#COMBINAMOS LAS DOS BASES DE DATOS
dat <- rbind(dat1, dat3)
#CAMBIAR LA PROFUNDIDAD A CARACTER
dat$PROF_EFECT <- as.character(dat$PROF_EFECT)

# Creamos un campo unico para cada perfil
dat$IDPROF <- paste0("IDPROF_", dat$LAT_UTM, "_", dat$LON_UTM)


## El objetivo de este script es completar los valores
# de LIM SUP y LIM INF en la tabla. Al ser una mezcla de diferentes
# casos tendremos que trabajar cada caso por separado
# Los valores controlados de LIM_SUP los guardaremos en la columna
# TOP y los de LIM INF en la columna BOTTOM
dat$top <- NA
dat$bottom <- NA

####### 1- DATOS INCOMPLETOS ###################

# Algunos perfiles no tiene informacion en prof_EFECT ni en
# PROF_INF, estos no pueden utilizarse

dat <- dat[!(is.na(dat$PROF_EFECT) & is.na(dat$LIM_INF)),]
dat <- dat[!(dat$PROF_EFECT == "" & is.na(dat$LIM_INF)),]


####### 2- DATOS con 0 #########################

# Otros perfiles tienen 0 en ambos campos. Tambien deben ser eliminados

dat <- dat[!(dat$PROF_EFECT == "0" & dat$LIM_INF == 0),]


####### 3- CASO 1 ##########################

# En algunos perfiles, el valor de PROF_EFECT representa
# el lim inf y superior del horizontes.
# Por ej en el horizontes nro 4955
dat[4955,]

# En estos casos, podemos utilizar el primer valor de prof_Efect como 
# valor superior y el segundo como inferior.
# con grepl podemos buscar un caracter que este en una cadena de texto
# como la de PROF_EFECT
grepl("-",dat$PROF_EFECT)

# Y podemos seleccionar los horizontes que tiene la informacion de esta forma.
dat.caso1 <- dat[grepl("\\d+ ?-|– ?\\d+",dat$PROF_EFECT),]
dat <- dat[!grepl("\\d+ ?-|– ?\\d+",dat$PROF_EFECT),]

# primero convertimos PROF_EFECT a string desde factor
# y luego creamos top y bottom a partir de prof_efect
for(i in 1:nrow(dat.caso1)){
  dat.caso1$top[i] <- as.numeric(strsplit(dat.caso1$PROF_EFECT[i], "-")[[1]][1])
  dat.caso1$bottom[i] <- as.numeric(strsplit(dat.caso1$PROF_EFECT[i], "-")[[1]][2])
}


####### CASO 2 ##########################

## Para el resto de los casos, podemos considerar que las muestras son superficiales
# y entonces el LIM_INF es bottom y el superior es 0.
# Sin embargo bajo esta hipotesis no deberian haber horizontes que compartan las
# mismas coordenadas.

# vemos los duplicados
dat$IDPROF[duplicated(dat$IDPROF)]

# Eliminamos los perfiles con coordenadas duplicadas
dat.caso2 <- dat[!dat$IDPROF %in% dat$IDPROF[duplicated(dat$IDPROF)],]

## Para los perfiles que tienen un valor diferente de 0/NA en Lim_inf vamos 
# a considerar el lim_inf como bottom, y 0 como top
# Y si Lim_inf es 0, usaremos el Prof_efect como bottom

for(i in 1:nrow(dat.caso2)){
  dat.caso2$top[i] <- 0
  if(dat.caso2$LIM_INF[i] == 0 | is.na(dat.caso2$LIM_INF[i])){
    dat.caso2$bottom[i] <- as.numeric(dat.caso2$PROF_EFECT[i])
  } else {
    dat.caso2$bottom[i] <- as.numeric(dat.caso2$LIM_INF[i])
  }
}


########################################################################################
########################################################################################
########################################################################################
########################################################################################

## Finalmente nuestra base de datos seran las suma de los datos validados

dat <- rbind(dat.caso1, dat.caso2)

dat$IDPROF <- factor(dat$IDPROF)
dat$IDPROF <- as.numeric(dat$IDPROF)
dat$IDPROF <- as.character(dat$IDPROF)

# hasta 1500 esta bien
#dat <- dat[1:4000,]

# Pero aun quedan algunos problemas: 
# Un perfil no tiene valor en bottom dado que su prof_efect era 90-1,2
dat[is.na(dat$bottom),]

# lo quitamos
dat <- dat[!is.na(dat$bottom),]

# Y dos perfiles tienen los valores erroneos porque tenian 80-60
dat[dat$bottom <= dat$top,]

# los quitamos
dat <- dat[!dat$bottom <= dat$top,]

## Los convertimos a un Soil Profile Collection
# usando el paquete aqp

library(aqp)

depths(dat) <- IDPROF ~ top + bottom

# diferenciamos atributos de perfil de atributos de horizontes
site(dat) <- ~ LAT_UTM + LON_UTM 

# Especificamos las coordenadas espaciales
coordinates(dat) <- ~ LON_UTM + LAT_UTM
dat@sp@proj4string <- CRS("+init=epsg:4326")

plot(dat@sp)

####
########### Splines #####################3
#arena
library(GSIF)

try(ARENA <- mpspline(dat, 'ARENA', d = t(c(0,30))))

ARENA <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  ARENA = ARENA$var.std[,1])

try(LIMO <- mpspline(dat, 'LIMO', d = t(c(0,30))))

LIMO <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  LIMO = LIMO$var.std[,1])

try(ARCILLA <- mpspline(dat, 'ARCILLA', d = t(c(0,30))))

ARCILLA <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  ARCILLA = ARCILLA$var.std[,1])


try(X.MO <- mpspline(dat, 'X.MO', d = t(c(0,30))))

X.MO <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  X.MO = X.MO$var.std[,1])

try(PHAGUA <- mpspline(dat, 'PHAGUA', d = t(c(0,30))))

PHAGUA <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  PHAGUA = PHAGUA$var.std[,1])


try(CE <- mpspline(dat, 'CE', d = t(c(0,30))))


CE <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  CE = CE$var.std[,1])

try(CICE <- mpspline(dat, 'CICE', d = t(c(0,30))))


CICE <- data.frame(
                  x = dat@sp@coords[,1],
                  y = dat@sp@coords[,2],
                  CICE = CICE$var.std[,1])






write.csv(training, file='arena30cm.csv')
###
training <- na.omit(training)
 coordinates (training) <- ~ x+y
 plot(training)
 e <- drawExtent()
 e <- as(e, 'SpatialPolygons')
 training <- training[e,]
 plot(training)
 names(training)
fitSOC <- autoKrige(ARENA~1., training)
mapSOC <- stack(fitSOC$krige_output)
plot(mapSOC)
writeRaster(mapSOC[[1]], file='krigingARENA.tif')
#
