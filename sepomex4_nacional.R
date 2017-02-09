library(rgdal)
library(sp)
library(dplyr)
library(rgeos)
library(raster)
library(stringdist)
library(stringi)
library(data.table)
library(tmap)
library(stringi)

Sys.setlocale("LC_CTYPE", "UTF-8")

# Importar codigos de manzanas con coordenadas de centroides, tablas y shapefiles de SEPOMEX, INE, RUV y DENUE:
cof<-read.csv("CPdescarga.csv",header = T, sep ="|",stringsAsFactors = F)
sepo<-readOGR("cp_pais.shp" )
proj4string(sepo) <- CRS(paste0("+proj=lcc +lat_1=17.5 +lat_2=29.5 ","+lat_0=12 +lon_0=-102 +x_0=2500000 ","+y_0=0 +ellps=GRS80 +units=m +no_defs"))
sepo<-spTransform(sepo, CRS("+proj=longlat +ellps=WGS84"))
manz<- fread("centroides_manzanas_2016.csv")
ruv<-read.csv("ruv_0.csv",header=T,colClasses = "character")
ruv<-ruv[which(!is.na(as.numeric(ruv$codigoPost))),]

accuracy<-vector()
final<-data.frame()
estados<-c("01","02","03","04","05","06","07","08","09",as.character(10:32)[-19])
for(id_edo in estados){
  dn<-read.csv(paste0("denue_",id_edo,"_csv/conjunto_de_datos/denue_inegi_",id_edo,"_.csv"),header=T,colClasses = "character")
  dn<-dn[,c("cod_postal","longitud","latitud")]
  dn<-dn[which((!is.na(as.numeric(dn$longitud)))&(!is.na(as.numeric(dn$latitud)))&(!is.na(as.numeric(dn$cod_postal)))&(as.numeric(dn$cod_postal)!=0)),]
  municipios<-readOGR(paste0("Shapefile/",id_edo,"/MUNICIPIO.shp" ))
  col<-readOGR(paste0("Shapefile/",id_edo,"/COLONIA.shp" ))
  col<- spTransform(col, CRS("+proj=longlat +ellps=WGS84"))
  
  # Filtrar manzanas:
  tlmanz <- manz[substr(manz$cvegeo, 1, 2) == id_edo,]
  tlmanz$centroide_x <- readr::parse_number(tlmanz$centroide_x)
  tlmanz$centroide_y <- readr::parse_number(tlmanz$centroide_y)
  
  # Clasificar manzanas respecto a los polígonos de SPOMEX: 
  tlmanz.sp <- SpatialPointsDataFrame(data.frame(tlmanz[, c(3, 4), with = FALSE]),data=data.frame(cvegeo=(tlmanz[,1])),proj4string = CRS("+proj=longlat +ellps=WGS84"))
  class2<-over(tlmanz.sp,sepo)
  ind2<-which(!is.na(class2$d_cp))
  
  # Estandarizar caracteres, sin acentos y en minusculas:
  cof$d_asenta<-tolower(stri_trans_general(cof$d_asenta,"latin-ascii"))
  cof$D_mnpio<-tolower(stri_trans_general(cof$D_mnpio,"latin-ascii"))
  col@data$NOMBRE<-tolower(stri_trans_general(as.character(col@data$NOMBRE),"latin-ascii"))
  municipios$NOMBRE<-tolower(stri_trans_general(as.character(municipios$NOMBRE),"latin-ascii"))
  
  nuevo<-c("","","n"," 1. "," 2. "," 3. "," 4. "," 5. "," 6. "," 7. "," 8. "," 9. "," 10. ",
           " 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 "," 10 ",
           " 11 "," 12 "," 13 "," 14 "," 15 "," 16 "," 17 "," 18 "," 19 "," 20 ",
           " 21 "," 22 "," 23 "," 24 "," 25 "," 26 "," 27 "," 28 "," 29 "," 30 "," 31 ")
  
  viejo<-c("-","\\(.+\\)","ñ","((\\s|^)primer(a|o)(\\s|$))|((\\s|^)1(\\S){1,3}((\\s|$)))","((\\s|^)segund(a|o)(\\s|$))|((\\s|^)2(\\S){1,3}((\\s|$)))","((\\s|^)tercer(a|o)(\\s|$))|((\\s|^)3(\\S){1,3}((\\s|$)))","((\\s|^)cuart(a|o)(\\s|$))|((\\s|^)4(\\S){1,3}((\\s|$)))","((\\s|^)quint(a|o)(\\s|$))|((\\s|^)5(\\S){1,3}((\\s|$)))","((\\s|^)sext(a|o)(\\s|$))|((\\s|^)6(\\S){1,3}((\\s|$)))","((\\s|^)septim(a|o)(\\s|$))|((\\s|^)7(\\S){1,3}((\\s|$)))","((\\s|^)octav(a|o)(\\s|$))|((\\s|^)8(\\S){1,3}((\\s|$)))","((\\s|^)noven(a|o)(\\s|$))|((\\s|^)9(\\S){1,3}((\\s|$)))","((\\s|^)decim(a|o)(\\s|$))|((\\s|^)10(\\S){1,3}((\\s|$)))",
           "(((\\s|^)uno(\\s|$)))|((\\s|^)i(\\s|$))","((\\s|^)dos(\\s|$))|((\\s|^)ii(\\s|$))","((\\s|^)tres(\\s|$))|((\\s|^)iii(\\s|$))","((\\s|^)cuatro(\\s|$))|((\\s|^)iv(\\s|$))","((\\s|^)cinco(\\s|$))|((\\s|^)v(\\s|$))","((\\s|^)seis(\\s|$))|((\\s|^)vi(\\s|$))","((\\s|^)siete(\\s|$))|((\\s|^)vii(\\s|$))","((\\s|^)ocho(\\s|$))|((\\s|^)viii(\\s|$))","((\\s|^)nueve(\\s|$))|((\\s|^)ix(\\s|$))", "((\\s|^)diez(\\s|$))|((\\s|^)x(\\s|$))",
           "(\\s|^)once(\\s|$)","(\\s|^)doce(\\s|$)","(\\s|^)trece(\\s|$)","(\\s|^)catorce(\\s|$)","(\\s|^)quince(\\s|$)","(\\s|^)diecises(\\s|$)","(\\s|^)diecisiete(\\s|$)","(\\s|^)dieciocho(\\s|$)","(\\s|^)diecinueve(\\s|$)","(\\s|^)veinte(\\s|$)",
           "(\\s|^)veintiuno(\\s|$)","(\\s|^)veintidos(\\s|$)","(\\s|^)veintitres(\\s|$)", "(\\s|^)veinticuatro(\\s|$)","(\\s|^)veinticinco(\\s|$)","(\\s|^)veintiseis(\\s|$)","(\\s|^)veintisiete(\\s|$)","(\\s|^)veintiocho(\\s|$)","(\\s|^)veintinueve(\\s|$)","(\\s|^)treinta(\\s|$)","(\\s|^)treintaiuno(\\s|$)")
  
  cof$d_asenta<-stri_replace_all_regex(cof$d_asenta,viejo,nuevo,vectorize_all = F)
  cof$D_mnpio<-stri_replace_all_regex(cof$D_mnpio,viejo,nuevo,vectorize_all = F)
  col@data$NOMBRE<-stri_replace_all_regex(col@data$NOMBRE,viejo,nuevo,vectorize_all = F)
  municipios$NOMBRE<-stri_replace_all_regex(municipios$NOMBRE,viejo,nuevo,vectorize_all = F)
  
  cof$d_asenta<-stri_replace_all_regex(cof$d_asenta,c("�","  ","^\\s|\\s$"),c("n"," ",""),vectorize_all = F)
  cof$D_mnpio<-stri_replace_all_regex(cof$D_mnpio,c("�","  ","^\\s|\\s$"),c("n"," ",""),vectorize_all = F)
  col@data$NOMBRE<-stri_replace_all_regex(col@data$NOMBRE,c("�","  ","^\\s|\\s$"),c("n"," ",""),vectorize_all = F)
  municipios$NOMBRE<-stri_replace_all_regex(municipios$NOMBRE,c("�","  ","^\\s|\\s$"),c("n"," ",""),vectorize_all = F)
  
  cof$c_estado<-as.numeric(cof$c_estado)
  cof$c_mnpio<-as.numeric(cof$c_mnpio)
  
  col@data$ENTIDAD<-as.numeric(col@data$ENTIDAD)
  col@data$MUNICIPIO<-as.numeric(col@data$MUNICIPIO)
  
  # Clasificar colonias respecto a INE, DENUE y RUV:
  
  dn<-dn[which(dn$cod_postal!=""),]
  dn_points<-SpatialPointsDataFrame(data.frame(longitud=as.numeric(dn[,"longitud"]),latitud=as.numeric(dn[,"latitud"])),data=data.frame(CP=as.character(dn[,"cod_postal"])),proj4string=CRS("+proj=longlat +ellps=WGS84"))
  
  ruv<-ruv[which(ruv$codigoPost!=""),]
  ruv_points<-SpatialPointsDataFrame(data.frame(longitud=as.numeric(ruv[,"x"]),latitud=as.numeric(ruv[,"y"])),data=data.frame(CP=ruv[,"codigoPost"]),proj4string=CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"))
  ruv_points<-spTransform(ruv_points, CRS("+proj=longlat +ellps=WGS84"))
  
  com<-SpatialPointsDataFrame(data.frame(coords=rbind(ruv_points@coords,dn_points@coords)),data=data.frame(CP=c(as.character(ruv_points@data$CP),as.character(dn_points@data$CP))),proj4string=CRS("+proj=longlat +ellps=WGS84"))
  
  colcls <- vector()
  for(i in 1:length(col)){ 
    ls <- over(col[i,],com, returnList = T)
    if(length(ls[[1]][[1]]) < 25){ 
      colcls[i] <- as.character(col[i,]@data$CP)
    }else{
      colcls[i] <- as.character(names(sort(table(ls[[1]]$CP),decreasing = T)[1])) 
    }
  }
  
  # Clasificar colonias respecto a la tabla de SEPOMEX. 
  # En caso de no encontrar una colonia compatible en la tabla, el metodo asigna el valor encontrado en el paso anterior:
  maxdist<-3
  flag<-list()
  flag2<-vector()
  sclass<-vector()
  snom<-vector()
  for(j in unique(col@data$ENTIDAD)){
    for(i in unique(col@data$MUNICIPIO)){
      mun<-as.character(municipios@data$NOMBRE[which(municipios@data$MUNICIPIO==i)])
      id<-which(i==col@data$MUNICIPIO & j==col@data$ENTIDAD)
      nombres<-as.character(col[id,]@data$NOMBRE)
      temp<-filter(cof,c_estado==j)
      m_cof<-unique(temp$D_mnpio)[which.min(stringdist(mun,unique(temp$D_mnpio),method="lcs"))] 
      temp<-filter(temp,D_mnpio==m_cof)
      seponom<-temp$d_asenta
      sepocod<-temp$d_codigo
      distm<-stringdistmatrix(nombres,temp$d_asenta,method="lcs")
      if(length(nombres)>length(temp$d_asenta)){
        flag<-c(flag,list(c(i,j))) 
      }
      while(T){
        if(length(id)==1 & length(seponom)>0){
          if(min(distm)>maxdist){
            sclass[id]<- colcls[id]
            snom[id]<-NA
            flag2<-c(flag2,id)
          }else{
            sclass[id]<-sepocod[which.min(distm)]
            snom[id]<-seponom[which.min(distm)]
          }
          break
        }else if(length(seponom)==0){
          sclass[id]<-colcls[id]
          snom[id]<-NA
          flag2<-c(flag2,id)
          break
        }else{
          k<-which.min(apply(distm,1,min))
          if(min(distm[k,])>maxdist){
            sclass[id[k]]<-colcls[id[k]]
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
  accuracy<-c(accuracy,sum(as.numeric(as.vector(class2$d_cp[ind]))==as.numeric(as.vector(class1$SEPOMEX[ind])))/length(ind))
  
  # Data frame final:
  final<-rbind(final,data.frame(Manzana=tlmanz$cvegeo,SEPOMEX=class2$d_cp,INE=class1$CP,Metodo=class1$SEPOMEX))
}

ind1<-which(!is.na(final$SEPOMEX))
ind2<-which(!is.na(final$Metodo))
ind<-intersect(ind1,ind2)
acc_final<-sum(as.numeric(as.vector(final$SEPOMEX[ind]))==as.numeric(as.vector(final$Metodo[ind])))/length(ind)
Sys.setlocale("LC_CTYPE", "C")