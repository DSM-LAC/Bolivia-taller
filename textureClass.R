#CARGA LAS LIBRERIAS
library(sp)
library(rgdal)
library(raster)
library(soiltexture)
library(rasterVis)
library(RColorBrewer)
library(dplyr)
#CARGA LOS MAPAS DE ARCILLA, LIMO Y AREANA
CLAY <- raster("mapaArcilla5kmBOL.tif")
SAND <- raster("mapaArena5kmBOL.tif")
SILT <- raster("mapaLIMO5kmBOL.tif")  
#GENERA UN ARCHIVO CON LOS TRES MAPAS
SSC <- stack(CLAY, SILT, SAND)
#CONVIERTELO EN UNA BASE DE DATOS TABULAR ESPACIAL
SSCP <- as(SSC, 'SpatialPixelsDataFrame')
names(SSCP@data) <- c('CLAY', 'SILT', 'SAND')
SSCP@data <- round(SSCP@data, 2)
#NORMALIZA Y CORRIGE VALORES POR ENCIMA DE 100 (PORCENTAJE ARENAS, LIMOS Y ARCILLAS)
SSCP@data$raw_totals <- rowSums(SSCP@data[, 1:3])
SSCP_norm <- TT.normalise.sum(tri.data = SSCP@data, residuals = T)
colnames(SSCP_norm)[1:3] <- paste0(colnames(SSCP_norm)[1:3], "_n")
SSCP@data <- cbind(SSCP@data, round(SSCP_norm, 2)) 
rm(SSCP_norm)
str(SSCP@data)

#CALCULA LA CLASE TEXTURAL 
SSCP@data <- cbind(SSCP@data, 
                  "TEXCLASS" = TT.points.in.classes(tri.data  = SSCP@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],
                                                    css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),
                                                    class.sys = "USDA.TT", 
                                                    PiC.type  = "t",
                                                    collapse  = ', '))
                  
#REVISA LOS NIVELES DE TEXTURA                                                   
 levels(SSCP@data$TEXCLASS)
#VISUALIZA EL MAPA DE TEXTURAS
 spplot(SSCP['TEXCLASS'])
#CLAVES DE MAPA 
##Cl = arcilloso
##Lo = limoso 
##Sa = arenoso
 
#CONVIERTE A UN FORMATO RASTER
 text <- as.factor( raster(SSCP['TEXCLASS']))

## Add a soilTexture column to the Raster Attribute Table
rat <- levels(text)[[1]]
rat[["soilTexture"]] <-as.character(data.frame(levels(raster(SSCP['TEXCLASS'])))[,2])
levels(text) <- rat
## VISUALIZA EL RASTER
levelplot(text, par.settings=RdBuTheme, xlab="", ylab="")
#GUARDA TU RASTER DE TEXTURAS EN UN TIF
#writeRaster(text, 'text_rat.tif', datatype = 'INT2S', overwrite = TRUE)
#FIN











                  
