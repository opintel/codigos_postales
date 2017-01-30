library(rgdal)
library(sp)
library(dplyr)
library(rgeos)
library(raster)
library(kknn)

# Importar codigo y coordenadas de manzanas:
manz   <- fread("centroides_manzanas_2016.csv")
tlmanz <- manz[substr(manz$cvegeo, 1, 2) == "29",]
tlmanz$centroide_x <- readr::parse_number(tlmanz$centroide_x)
tlmanz$centroide_y <- readr::parse_number(tlmanz$centroide_y)

# Importar poligonos con codigos postales por colonia (del INE) 
col<-readOGR("COLONIA.shp" )
# Geometrias nulas: Id 748, colonia NAZARETH & Id 758, colonia ATLAHAPA.
col<- spTransform(col, CRS("+proj=longlat +ellps=WGS84"))

ageb<-vector()
for(i in 1:nrow(tlmanz)){
  ageb[i]<-substr(tlmanz[i,"cvegeo"],1,13)
}
ageb<-as.character(ageb)

#Importar poligonos de manzanas viejas:
sh <- readOGR("29m.shp", stringsAsFactors = F)
# Solo manzanas viejas:
tlmanz_viejo<-which(tlmanz[,1] %in% sh@data[,"cvegeo"])
# Solo manzanas nuevas:
tlmanz_nuevo<-which(!(tlmanz[,1] %in% sh@data[,"cvegeo"]))

# Importar poligonos con codigos postales establecidos por SEPOMEX y transformar coordenadas a lon-lat:
sepo<-readOGR("cp_pais.shp" )
proj4string(sepo) <- CRS("+proj=lcc +lat_1=17.5 +lat_2=29.5 +lat_0=12 +lon_0=-102 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
sepo<-spTransform(sepo, CRS("+proj=longlat +ellps=WGS84"))

# Clasificar manzanas respecto a las colonias del INE (sin poligonos sobrepuestos, m??s rapido): 
tlmanz.sp<-SpatialPoints(tlmanz[,c(3,4)],proj4string=CRS("+proj=longlat +ellps=WGS84"))
class1<-over(tlmanz.sp,col)$CP
ind1<-which(!is.na(class1))
cp1<-data.frame(Codigo=tlmanz[ind1,1],CP=class1[ind1])

# Clasificar manzanas respecto a los pol??gonos de SPOMEX: 
class2<-over(tlmanz.sp,sepo)
ind2<-which(!is.na(class2$d_cp))
cp2<-data.frame(Codigo=tlmanz[ind2,1],CP=class2$d_cp[ind2])

# Comparar resultados:
ind<-intersect(ind1,ind2)
ind<-intersect(ind,tlmanz_viejo)
accuracyINE_viejo<-sum(as.numeric(as.vector(class2$d_cp[ind]))==as.numeric(as.vector(class1[ind])))/length(ind)
ind<-intersect(ind1,ind2)
ind<-intersect(ind,tlmanz_nuevo)
accuracyINE_nuevo<-sum(as.numeric(as.vector(class2$d_cp[ind]))==as.numeric(as.vector(class1[ind])))/length(ind)
######################################

# Importat DENUE:
dn<-read.csv("denue_inegi_29_.csv",header=T,colClasses = "character")
b<-(dn$cod_postal!="" & dn$cve_ent!="" & dn$cve_mun!="" & dn$cve_loc!="" & dn$ageb!="" & dn$manzana!="")
dn<-dn[b,]

# Unir codigo de AGEB:
clv<-vector()
for(j in 1:nrow(dn)){
  ent<-dn[j,"cve_ent"]
  mun<-dn[j,"cve_mun"]
  loc<-dn[j,"cve_loc"]
  ag<-dn[j,"ageb"]
  m<-dn[j,"manzana"]
  if(nchar(ent)==1){
    ent<-paste(c("0",ent),collapse = "")
  }
  if(nchar(mun)==2){
    mun<-paste(c("0",mun),collapse = "")
  }else if(nchar(mun)==1){
    mun<-paste(c("00",mun),collapse = "")
  }
  if(nchar(loc)==3){
    loc<-paste(c("0",loc),collapse = "")
  }else if(nchar(loc)==2){
    loc<-paste(c("00",loc),collapse = "")
  }else if(nchar(loc)==1){
    loc<-paste(c("000",loc),collapse = "")
  }
  if(nchar(ag)==3){
    ag<-paste(c("0",ag),collapse = "")
  }else if(nchar(ag)==2){
    ag<-paste(c("00",ag),collapse = "")
  }else if(nchar(ag)==1){
    ag<-paste(c("000",ag),collapse = "")
  }
  if(nchar(m)==2){
    m<-paste(c("0",m),collapse = "")
  }else if(nchar(m)==1){
    m<-paste(c("00",m),collapse = "")
  }
  clv[j]<-paste(c(ent,mun,loc,ag),collapse = "")
}
dn[,"clv"]<-clv

# Añadir projecciones UTM zona 14 a manzanas y DENUE para usar como variables en knn:
sppoints1<-SpatialPoints(tlmanz[,c(3,4)],proj4string=CRS("+proj=longlat +ellps=WGS84"))
tlmanz[,c("UTM1","UTM2")]<-as.data.frame(spTransform(sppoints1,CRS("+proj=utm +zone=14 +datum=WGS84")))
sppoints2<-SpatialPoints(data.frame(as.numeric(dn[,"longitud"]),as.numeric(dn[,"latitud"])),proj4string=CRS("+proj=longlat +ellps=WGS84"))
dn[,c("UTM1","UTM2")]<-as.data.frame(spTransform(sppoints2,CRS("+proj=utm +zone=14 +datum=WGS84")))

# Importar RUV y poligonos de AGEB:
ruv<-read.csv("ruv_0.csv",header=T)
shageb<-readOGR("29a.shp", stringsAsFactors = F)

# Transformar coordenadas a UTM zona 14 y a??adir clave de AGEB (a partir de los poligonos):
ruv<-data.frame(cod_postal=ruv[,"codigoPost"],x=ruv[,"x"],y=ruv[,"y"])
ruv_points<-SpatialPoints(data.frame(x=ruv[,"x"],y=ruv[,"y"]),proj4string=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
ruv_points<-spTransform(ruv_points, CRS(proj4string(shageb)))
ruv[,c("UTM1","UTM2")]<-as.data.frame(spTransform(ruv_points,CRS("+proj=utm +zone=14 +datum=WGS84")))
ruv[,"clv"]<-over(ruv_points,shageb)$cvegeo

# Unir datos relevantes de DENUE y RUV:
dn<-rbind(dn[,c("clv","UTM1","UTM2","cod_postal")],ruv[which(!is.na(ruv[,"clv"])),c("clv","UTM1","UTM2","cod_postal")])

# Clasificar manzanas viejas por codigo postal:
manz_viejo<-tlmanz[tlmanz_viejo,]
ver_col<-accuracyINE_viejo
class6<-vector()
for(j in unique(ageb[tlmanz_viejo])){
  mag<-filter(manz_viejo,substr(cvegeo,1,13)==j)
  dag<-filter(dn,clv==j)
  ine<-as.character(over(SpatialPoints(mag[,c(3,4)],proj4string=CRS("+proj=longlat +ellps=WGS84")),col)$CP)
  ind<-match(mag[,"cvegeo"],manz_viejo[,1])
  if(nrow(dag)>25){
    if(length(unique(as.factor(as.character(dag[,"cod_postal"]))))>1){
      denue<-kknn(as.factor(as.character(dag[,"cod_postal"]))~.,train=dag[,c("UTM1","UTM2")],test=mag[,c("UTM1","UTM2")],kernel="triangular",distance=2,k=25)
      for(i in 1:nrow(mag)){
        if(!(ine[i] %in% colnames(denue$prob[i,]))){
          class6[which(manz_viejo[,1]==mag[i,"cvegeo"])[1]]<-as.character(denue$fitted.values[i])
        }else if(denue$prob[i,ine]==0 | as.character(denue$fitted.values[i])==ine[i]){
          class6[which(manz_viejo[,1]==mag[i,"cvegeo"])[1]]<-as.character(denue$fitted.values[i])
        }else if(denue$prob[i,ine]*ver_col < (1-ver_col)/(1-denue$prob[i,ine])*max(denue$prob[i,])^2){
          class6[which(manz_viejo[,1]==mag[i,"cvegeo"])[1]]<-as.character(denue$fitted.values[i])
        }else{
          class6[which(manz_viejo[,1]==mag[i,"cvegeo"])[1]]<-ine[i]
        }
      }
    }else{
      class6[ind]<-as.character(unique(as.factor(as.character(dag[,"cod_postal"]))))
    }
  }else{
    class6[ind]<-ine
  }
}

ind6<-which(!is.na(class6))
ind2<-which(!is.na(class2$d_cp[tlmanz_viejo]))
ind<-intersect(ind6,ind2)
accuracy_viejo<-sum(as.numeric(as.vector(class2$d_cp[tlmanz_viejo][ind]))==as.numeric(as.vector(class6[ind])))/length(ind)
###############################

# Clasificar manzanas nuevas por codigo postal:
manz_nuevo<-tlmanz[tlmanz_nuevo,]
ver_col<-accuracyINE_nuevo
class6.2<-vector()
for(j in unique(ageb[tlmanz_nuevo])){
  mag<-filter(manz_nuevo,substr(cvegeo,1,13)==j)
  dag<-filter(dn,clv==j)
  ine<-as.character(over(SpatialPoints(mag[,c(3,4)],proj4string=CRS("+proj=longlat +ellps=WGS84")),col)$CP)
  ind<-match(mag[,"cvegeo"],manz_nuevo[,1])
  if(nrow(dag)>25){
    if(length(unique(as.factor(as.character(dag[,"cod_postal"]))))>1){
      denue<-kknn(as.factor(as.character(dag[,"cod_postal"]))~.,train=dag[,c("UTM1","UTM2")],test=mag[,c("UTM1","UTM2")],kernel="triangular",distance=2,k=25)
      for(i in 1:nrow(mag)){
        if(!(ine[i] %in% colnames(denue$prob[i,]))){
          class6.2[which(manz_nuevo[,1]==mag[i,"cvegeo"])[1]]<-as.character(denue$fitted.values[i])
        }else if(denue$prob[i,ine]==0 | as.character(denue$fitted.values[i])==ine[i]){
          class6.2[which(manz_nuevo[,1]==mag[i,"cvegeo"])[1]]<-as.character(denue$fitted.values[i])
        }else if(denue$prob[i,ine]*ver_col < (1-ver_col)/(1-denue$prob[i,ine])*max(denue$prob[i,])^2){
          class6.2[which(manz_nuevo[,1]==mag[i,"cvegeo"])[1]]<-as.character(denue$fitted.values[i])
        }else{
          class6.2[which(manz_nuevo[,1]==mag[i,"cvegeo"])[1]]<-ine[i]
        }
      }
    }else{
      class6.2[ind]<-as.character(unique(as.factor(as.character(dag[,"cod_postal"]))))
    }
  }else{
    class6.2[ind]<-ine
  }
}

ind6.2<-which(!is.na(class6.2))
ind2<-which(!is.na(class2$d_cp[tlmanz_nuevo]))
ind<-intersect(ind6.2,ind2)
accuracy_nuevo<-sum(as.numeric(as.vector(class2$d_cp[tlmanz_nuevo][ind]))==as.numeric(as.vector(class6.2[ind])))/length(ind)
#######################

# Data frames finales:

tlaxcala_viejo<-data.frame(Manzana=as.character(manz_viejo[,"cvegeo"]),SEPOMEX=as.character(class2$d_cp[tlmanz_viejo]),INE=as.character(class1[tlmanz_viejo]),Estimacion=class6)
tlaxcala_nuevo<-data.frame(Manzana=as.character(manz_nuevo[,"cvegeo"]),SEPOMEX=as.character(class2$d_cp[tlmanz_nuevo]),INE=as.character(class1[tlmanz_nuevo]),Estimacion=class6.2)


