#IMPORTAMOS LAS COVARIABLES AMBIENTALES
library(raster)
covs <- stack("covs5km.tif")
names(covs) <- readRDS('worldgridsCOVS_names.rds')
###CAMBIA AQUI LAS PROPIEDADES
training <- ARCILLA
#EXTRAEMOS A LOS DATOS DE LAS COVARIABLES A LOS PUNTOS 
e <- extract(covs, training[c('x','y')])
training <- cbind(training, data.frame(e))
#QUITAMOS VALORES POR ENCIMA DE 65% DE ARCILLA
training$ARCILLA[training$ARCILLA>65] <- 65
#QUITAMOS LOS CEROS
training$ARCILLA[training$ARCILLA==0] <- NA
training <- na.omit(training)
#QUITAMOS LAS VARIABLES CATEGORICAS
cat1 <- grep('igb', names(training))[1:6]
		cat2 <- grep('esa', names(training))[23]		
		cat <- c(cat1, cat2)
t <- na.omit(training[-cat])
#ENCONTRAMOS LAS VARIABLES MEJOR CORRELACIONADAS CON LA ARCILLA
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
train$ARCILLA <- training$ARCILLA
train <- na.omit(train)
COVS <- covs[[idx]]
COVS[is.na(COVS[])] <- 0
COVS <- mask(COVS, covs[[1]])
library(caretEnsemble)
library(doParallel)
library(doMC)
set.seed(102)

ctrl <- trainControl(method="repeatedcv", number=5, repeats=5, savePredictions = TRUE)
cl <- makeCluster(detectCores(), type='SOCK')
registerDoParallel(cl)
models <- caretList(train[-11], train[,11], trControl=ctrl ,
methodList=c("rf", "lm"))
ens <- caretEnsemble(models)
stopCluster(cl = cl)

pred <- predict(COVS, ens)

library(rasterVis)

levelplot(pred)

writeRaster(pred, file='mapaArcillas5kmBOL.tif')





