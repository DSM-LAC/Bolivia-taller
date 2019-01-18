
#CARGAR LAS BASES DE DATOS 
 
  dem <-stack ("/home/mguevara/Downloads/NoSilverBulletOnDSM-2.0/Bolivia/BOLIVIA/covs5km.tif")
  names(dem) <- readRDS ("/home/mguevara/Downloads/NoSilverBulletOnDSM-2.0/Bolivia/BOLIVIA/worldgridsCOVS_names.rds")
  
  elevacion <- dem$DEMSRE3a

#CRITERIOS DE ELEVACION 

  elevacion[elevacion>3000] <- NA
  elevacion[elevacion<500] <- NA

#CRITERIO DE TEMPERATURA
  tmax <- raster('tmax_anual.tif')
  tmax[tmax>32] <- NA
  tmax[tmax<27] <- NA
  
  #tmin <- raster('tmin_media_anual.tif')
  #tmin <- resample(tmin, tmax)
  #temp <- stack(tmax, tmin)
  #temp <- calc(temp, mean)
 
 #CRITERIO DE PH
  ph <- raster("pH.tif")
  ph[ph>7] <- NA
  ph[ph<5] <- NA
 
 #CRITERIO DE PRECIPITACION
  pp <- raster("pp_anual1.tif")
  pp[pp<500] <- NA
  pp[pp>2000] <- NA
  
 #CRITERIO DE TEXTURA DE SUELOS
arcilla <- raster("mapaArcilla5kmBOL.tif")
arena <- raster("mapaArena5kmBOL.tif")
limo <- raster("mapaLIMO5kmBOL.tif") 

arena1 <- arena
arena1[arena1>70] <- NA
arena1[arena1<50] <- NA

limo1 <- limo
limo1[limo1>=50] <- NA

arcilla1 <- arcilla
arcilla1[arcilla1>20] <- NA
arcilla1[arcilla1<10] <- NA

#AJUSTA A UN MISMO SISTEMA DE INFORMACION GEOGRAFICA

ph <- projectRaster(ph, mapas)
pp <- projectRaster(pp, mapas)
tmax <- projectRaster(tmax, mapas)

#GENERA UN STACK DE VALORES
mapas <- stack(elevacion, arcilla1, limo1, arena1, ph, pp, tmax)
mapas <- stack(elevacion,  ph, pp, tmax)


#BUSCA 
df <- as.data.frame(mapas, xy=TRUE)
df <- na.omit(df)

coordinates(df) <- ~x+y
plot(df)

