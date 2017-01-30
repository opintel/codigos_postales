library(rgdal)
library(sp)
library(dplyr)
library(rgeos)
library(raster)
library(stringdist)
library(stringi)
library(data.table)

# Importar codigo y coordenadas de manzanas:
manz   <- fread("centroides_manzanas_2016.csv")
tlmanz <- manz[substr(manz$cvegeo, 1, 2) == "29",]
tlmanz$centroide_x <- readr::parse_number(tlmanz$centroide_x)
tlmanz$centroide_y <- readr::parse_number(tlmanz$centroide_y)

# Importar poligonos con codigos postales establecidos por SEPOMEX y transformar coordenadas a lon-lat:
sepo<-readOGR("cp_pais.shp" )
proj4string(sepo) 
proj4string(sepo) <- CRS(paste0("+proj=lcc +lat_1=17.5 +lat_2=29.5 ","+lat_0=12 +lon_0=-102 +x_0=2500000 ","+y_0=0 +ellps=GRS80 +units=m +no_defs"))
sepo<-spTransform(sepo, CRS("+proj=longlat +ellps=WGS84"))

# Clasificar manzanas respecto a los pol??gonos de SPOMEX: 
tlmanz.sp <- SpatialPoints(tlmanz[, c(3, 4), with = FALSE],proj4string = CRS("+proj=longlat +ellps=WGS84"))
class2<-over(tlmanz.sp,sepo)
ind2<-which(!is.na(class2$d_cp))


Sys.setlocale("LC_CTYPE", "UTF-8")

# Importar codigos postales asociados a colonias, a traves de sepomex:
cof<-read.csv("CPdescarga.csv",header = T, sep ="|",stringsAsFactors = F)

# Importar poligonos de colonias, del INE:
col<-readOGR("COLONIA.shp" )
col<- spTransform(col, CRS("+proj=longlat +ellps=WGS84"))

# Estandarizar caracteres, sin acentos y en minusculas:
cof$d_asenta<-tolower(stri_trans_general(cof$d_asenta,"latin-ascii"))
col@data$NOMBRE<-tolower(stri_trans_general(as.character(col@data$NOMBRE),"latin-ascii"))

#col@data$NOMBRE<-tolower(chartr("�","n",col@data$NOMBRE))
#cof$d_asenta<-tolower(chartr("�","n",cof$d_asenta))
Sys.setlocale("LC_CTYPE", "C")

cof$c_estado<-as.numeric(cof$c_estado)
cof$c_mnpio<-as.numeric(cof$c_mnpio)

col@data$ENTIDAD<-as.numeric(col@data$ENTIDAD)
col@data$MUNICIPIO<-as.numeric(col@data$MUNICIPIO)

# Clasificar poligonos de colonias:
maxdist<-3
flag<-list()
flag2<-vector()
sclass<-vector()
snom<-vector()
for(j in unique(col@data$ENTIDAD)){
  for(i in unique(col@data$MUNICIPIO)){
    id<-which(i==col@data$MUNICIPIO & j==col@data$ENTIDAD)
    nombres<-as.character(col[id,]@data$NOMBRE)
    temp<-filter(cof,c_estado==j)
    temp<-filter(temp,c_mnpio==i)
    seponom<-temp$d_asenta
    sepocod<-temp$d_codigo
    distm<-stringdistmatrix(nombres,temp$d_asenta,method="lcs")
    if(length(nombres)>length(temp$d_asenta)){
      flag<-c(flag,list(c(i,j))) 
    }
    while(T){
      if(length(id)==1 & length(seponom)>0){
        if(min(distm)>maxdist){
          sclass[id]<- as.character(col[id,]@data$CP)
          snom[id]<-NA
          flag2<-c(flag2,id)
        }else{
          sclass[id]<-sepocod[which.min(distm)]
          snom[id]<-seponom[which.min(distm)]
        }
        break
      }else if(length(seponom)==0){
        sclass[id]<-as.character(col[id,]@data$CP)
        snom[id]<-NA
        flag2<-c(flag2,id)
        break
      }else{
        k<-which.min(apply(distm,1,min))
        if(min(distm[k,])>maxdist){
          sclass[id[k]]<-as.character(col[id[k],]@data$CP)
          snom[id[k]]<-NA
          flag2<-c(flag2,id[k])
          id<-id[-k]
          nombres<-nombres[-k]
          distm<-stringdistmatrix(nombres,seponom,method="lcs")
        }else{
          sclass[id[k]]<-sepocod[which.min(distm[k,])]
          snom[id[k]]<-seponom[which.min(distm[k,])]
          id<-id[-k]
          seponom<-seponom[-which.min(distm[k,])]
          sepocod<-sepocod[-which.min(distm[k,])]
          nombres<-nombres[-k]
          distm<-stringdistmatrix(nombres,seponom,method="lcs")
        }
      } 
    }
  }
}
col@data$SEPOMEX<-sclass
col@data$SEPOMEX_nombres<-snom

# Clasificar manzanas:
class1<-over(tlmanz.sp,col)
ind1<-which(!is.na(class1$SEPOMEX))

# Comparar resultados:
ind<-intersect(ind1,ind2)
accuracy<-sum(as.numeric(as.vector(class2$d_cp[ind]))==as.numeric(as.vector(class1$SEPOMEX[ind])))/length(ind)

# Data frame final:
final<-data.frame(Manzana=tlmanz$cvegeo,SEPOMEX=class2$d_cp,INE=class1$CP,Metodo=class1$SEPOMEX)


