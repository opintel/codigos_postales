library(rgdal)
library(sp)
library(dplyr)
library(rgeos)
library(raster)
library(data.table)

# Este código clasifica manzanas por CP de dos formas.
# El primero genrea código postales asignados a los polígonos de colonias del INE.
# En el segundo se le asigna a cada manzana el CP reportado en el DENUE con mayor frecuencia en la colonia.

## Importar codigo y coordenadas de manzanas:
manz   <- fread("centroides_manzanas_2016.csv")
tlmanz <- manz[substr(manz$cvegeo, 1, 2) == "29",]
tlmanz$centroide_x <- readr::parse_number(tlmanz$centroide_x)
tlmanz$centroide_y <- readr::parse_number(tlmanz$centroide_y)

## Importar poligonos con codigos postales por colonia (del INE) 
col    <- readOGR("COLONIA.shp" )
## Geometrias nulas: Id 748, colonia NAZARETH & Id 758, colonia ATLAHAPA.
col    <- spTransform(col, CRS("+proj=longlat +ellps=WGS84"))

## Importar poligonos con codigos postales establecidos por SEPOMEX y transformar coordenadas a lon-lat:
sepo   <- readOGR("cp_pais.shp" )
proj4string(sepo) 
proj4string(sepo) <- CRS(
    paste0("+proj=lcc +lat_1=17.5 +lat_2=29.5 ",
           "+lat_0=12 +lon_0=-102 +x_0=2500000 ",
           "+y_0=0 +ellps=GRS80 +units=m +no_defs")
                        )
sepo   <- spTransform(sepo, CRS("+proj=longlat +ellps=WGS84"))

## Clasificar manzanas respecto a las colonias del INE (sin poligonos sobrepuestos, m??s rapido):
tlmanz.sp <- SpatialPoints(tlmanz[, c(3, 4), with = FALSE],
                          proj4string = CRS("+proj=longlat +ellps=WGS84"))
class1 <- over(tlmanz.sp, col)
class1 <- class1$CP
ind1   <- which(!is.na(class1))
cp1    <- data.frame(Codigo = tlmanz[ind1, 1, with = FALSE],
                    CP     = class1[ind1])

## Clasificar manzanas respecto a las colonias del INE (considerando pol??gonos sobrepuestos):
##class1<-vector()
##for(i in 1:nrow(tlmanz[,c(3,4)])){
##  ov<-over(SpatialPoints(tlmanz[i,c(3,4)],proj4string=CRS("+proj=longlat +ellps=WGS84")),col,returnList = T)
##  if(length(ov[[1]]$ID)>1){
##    areas<-vector()
##    for(j in 1:length(ov[[1]]$ID)){
##      areas[j]<-area(col[(col@data$ID==ov[[1]]$ID[j]),])
##    }
##    class1[i]<-as.character(ov[[1]]$CP[which.min(areas)])
##  }else if (length(ov[[1]]$ID)==1){
##    class1[i]<-as.character(ov[[1]]$CP)
##  }else{
##    class1[i]<-NA
##  }
##}
##ind1<-which(!is.na(class1))
##cp1<-data.frame(Codigo=tlmanz[ind1,1],CP=class1[ind1])

## Clasificar manzanas respecto a los pol??gonos de SPOMEX: 
class2 <- over(tlmanz.sp, sepo)
ind2   <- which(!is.na(class2$d_cp))
cp2    <- data.frame(Codigo = tlmanz[ind2, 1, with = FALSE],
                    CP     = class2$d_cp[ind2])

## Comparar resultados:
ind      <- intersect(ind1, ind2)
accuracy <- sum(as.character(class2$d_cp[ind]) == as.character(class1[ind]))/length(ind)

############################################

## Este codigo clasifica las colonias por mayoria de votos, utilizando los datos del DENUE.
## El codio asume:
## denue_inegi_29_.shp 

dn_shp <- readOGR("denue_inegi_29_.shp" )
proj4string(dn_shp)
dn_shp <- spTransform(dn_shp, CRS("+proj=longlat +ellps=WGS84"))
dn_shp <- dn_shp[!is.na(dn_shp@data$cod_postal),]

colcls <- vector()
for(i in 1:length(col)){ 
  ls <- over(col[i,],dn_shp, returnList = T)
  if(length(ls[[1]][[1]]) < 25){ 
    colcls[i] <- as.character(col[i,]@data$CP)
  }else{
      colcls[i] <- as.character(names(sort(table(ls[[1]]$cod_postal),
                                          decreasing = T)[1])) 
  }
}
col@data$CP_class<-colcls

class1_dn <- over(tlmanz.sp,col)
ind1_dn   <- which(!is.na(class1_dn$CP_class))
cp1_dn    <- data.frame(Codigo = tlmanz[ind1_dn, 1],
                       CP = class1_dn$CP_class[ind1_dn])


ind <- intersect(ind1_dn,ind2)
accuracy3 <- sum(as.character(class2$d_cp[ind]) ==
                as.character(class1_dn$CP_class[ind]))/length(ind)
