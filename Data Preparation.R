####################################
#### "Grocery for Votes" ###########
#### Replication File ##############
#### Data Preparation ##############
####################################

rm(list=ls(all=TRUE))
library(foreign)


#####Importing 2012 electoral results ("http://www.ife.org.mx/documentos/proceso_2011-2012/alterna/docs/computos2012.zip")
data<-read.table("/rawdata/datos_computos_casillas_presidente.txt", sep="|", header=T)

#cleaning the database
data$observaciones<-data$descripcion.1
drops <- c("descripcion.1","usuario", "X")
data<-data[ , !(names(data) %in% drops)]

#Dropping "Casillas Especiales" and "Casillas Extraordinarias." Voters can use cast their ballot in these polling stations for reasons other than their residence.
drops<-c("E","S")
data<-data[ !(data$TIPO_CASILLA) %in% drops, ]



#aggregating results by presidential candidate
#Josefina Vazquez Mota (PAN)
data$jvm2012=data$PAN
#Enrique Pena Nieto (PRI)
data$epn2012=data$PRI+data$PVEM+data$PRI_PVEM
#Andres Manuel Lopez Obrador (PRD)
data$amlo2012=data$PRD+data$PT+data$MC+data$PRD_PT_MC+data$PRD_PT+data$PRD_MC+data$PT_MC

data$total2012=data$jvm2012+data$epn2012+data$amlo2012+data$PANAL+data$NUM_VOTOS_NULOS+data$NUM_VOTOS_CAN_NREG

#creating a unique ID for each precinct
data$ID=paste(data$ID_ESTADO," _ ",data$SECCION)

data$ln2012<-data$LISTA_NOMINAL_CASILLA

data2012<-aggregate(data[,c(34:37,39)], by=list(seccionID=data$ID), FUN=sum) 

write.dta(data2012,"Precinct2012.dta")

#####Importing 2009 elections
data<-read.table("/rawdata/Computos Distritales 2009.txt", sep="\t", header=T, fill=TRUE)

#Dropping observations with no results
#Dropping  (1) polling stations that did not open the election day or (2) polling stations that did not submitted electoral results 
data<-data[data$ESTATUS_ACTA_W!="CASILLA NO INSTALADA",]
data<-data[data$ESTATUS_ACTA_W!="PAQUETE NO ENTREGADO",]


#Dropping "Casillas Especiales" and "Casillas Extraordinarias." Voters can use cast their ballot in these polling stations for reasons other than their residence.
drops<-c("ESPECIAL", "EXTRAORDINARIA")
data<-data[ !(data$TIPO_CASILLA) %in% drops, ]


##Aggregating votes by coalition
data$PRIMERO_MEXICO[is.na(data$PRIMERO_MEXICO)==TRUE]<-0
data$SALVEMOS_MEXICOO[is.na(data$SALVEMOS_MEXICO)==TRUE]<-0


data$PRI2009<-data$PRI+data$PVEM+data$PRIMERO_MEXICO
data$PAN2009<-data$PAN
data$PRD2009<-data$PRD+data$SALVEMOS_MEXICO+data$CONVERGENCIA+data$PT

data$total2009=data$PRI2009+data$PAN2009+data$PRD2009+data$NUEVA_ALIANZA+data$PSD+data$NUMERO_VOTOS_NULOS+data$NUM_VOTOS_CANDIDATOS_NO_REG

data$ID=paste(data$ID_ESTADO," _ ",data$SECCION)

data$ln2009<-data$LISTA_NOMINAL_CASILLA

data<-data[is.na(data$ln2009)==FALSE,]

data2009<-aggregate(data[,c(28:31,33)], by=list(seccionID=data$ID), FUN=sum) 

write.dta(data2009,"Precinct2009.dta")

####Importing 2006 information
data<-read.table("/rawdata/resultadosCasilla2006.txt", sep="|", header=T, fill=TRUE)

#Dropping  polling stations that did not open the election day
data$observaciones<-data$descripcion.1
data<-data[data$observaciones!="Casilla no instalada",]

#Dropping "Casillas Especiales" and "Casillas Extraordinarias." Voters can use those polling stations for reasons other than their residence.
drops<-c("E","S")
data<-data[ !(data$tipo_casilla) %in% drops, ]

data$PAN2006<-data$participante1 
data$PRI2006 <-data$participante2 
data$PRD2006 <-data$participante3 
data$PANAL2006 <-data$participante4 
data$PASC2006 <-data$participante5 
data$noreg2006 <-data$participante6 
data$nulos2006 <-data$participante7 

data$total2006=data$PAN2006+data$PRI2006+data$PRD2006+data$PANAL2006+data$PASC2006+data$nulos2006+data$noreg2006

data$ID=paste(data$id_estado," _ ",data$seccion)

data$ln2006<-data$lista_nominal

data<-data[is.na(data$ln2006)==FALSE,]

data2006<-aggregate(data[,c(86:93,95)], by=list(seccionID=data$ID), FUN=sum) 

write.dta(data2006,"Precinct2006.dta")

#####Importing 2003 data
data<-read.table("/rawdata/resultadosCasilla2003.txt", sep="\t", header=T, fill=TRUE)

#Dropping  polling stations that did not open the election day
data<-data[data$ESTATUS_ACTA_W!="CASILLA NO INSTALADA",]
data<-data[data$ESTATUS_ACTA_W!="PAQUETE NO ENTREGADO",]


#Dropping "Casillas Especiales" and "Casillas Extraordinarias." Voters can use those polling stations for reasons other than their residence.
drops<-c("ESPECIAL","EXTRAORDINARIA")
data<-data[ !(data$TIPO_CASILLA) %in% drops, ]

data$PAN2003<-data$PAN 
data$PRI2003 <-data$PRI
data$PRD2003 <-data$PRD 

data$total2003=data$TOTAL_VOTOS

data$ID=paste(data$ID_ESTADO," _ ",data$SECCION)

data$ln2003<-data$LISTA_NOMINAL_CASILLA

data<-data[is.na(data$ln2003)==FALSE,]

data2003<-aggregate(data[,c(27:30,32)], by=list(seccionID=data$ID), FUN=sum) 

write.dta(data2003,"Precinct2003.dta")

#####Sociodemographics

datademog<-read.dta("/rawdata/sociodemographics.dta")

########
#Proportion of the population over 18
datademog$P18<-datademog$P_18YMAS/datademog$POBTOT

#Proportion of the population under 18
datademog$P18u<-(datademog$POBTOT-datademog$P_18YMAS)/datademog$POBTOT

#Proportion of the population under 14
datademog$P14u<-datademog$POB0_14/datademog$POBTOT

#Proportion of the population over 60
datademog$P60<-datademog$P_60YMAS/datademog$POBTOT

#Proportion of the population over 65
datademog$P65<-datademog$POB65_MAS /datademog$POBTOT


#Population speaking and indigenous language (population over 3 years old)
datademog$INDIGENOUS<-datademog$P3YM_HLI/datademog$P_3YMAS

#Population with any moving disability
datademog$DISABILITY<-datademog$PCLIM_MOT/datademog$POBTOT

#Average scholarship
datademog$EDUCATION<-datademog$GRAPROES

#Proportion of people over 18 years old with completed middle school or more
datademog$POSTEDUC<-datademog$P18YM_PB/datademog$P_18YMAS

#Illiteracy rate for individuals over 15 years old
datademog$ILLITERACY<-datademog$P15YM_AN/datademog$P_15YMAS  

#Population with an economic activity. Poblacion Economicamente Activa (PEA)
datademog$PEAprop<-datademog$PEA/datademog$P_12YMAS 

#Male PEA
datademog$PEAmale<-datademog$PEA_M/datademog$P_12YMAS_M

#Female PEA
datademog$PEAfemale<-datademog$PEA_F/datademog$P_12YMAS_F

#Population without insurance
datademog$NOINSURANCE<-datademog$PSINDER/datademog$POBTOT

#Catholic population
datademog$CATHOLIC<-datademog$PCATOLICA/datademog$POBTOT

#Non-religious population
datademog$NONRELIGIOUS<-datademog$PSIN_RELIG/datademog$POBTOT

#Population living with a female-headed family
datademog$FEMALEJEFA<-datademog$PHOGJEF_F/datademog$POBTOT

#Average persons per room
datademog$PERROOM<-datademog$PRO_OCUP_C

#Houses with dirt floor
datademog$DIRTFLOOR<-datademog$VPH_PISOTI/datademog$VIVPAR_HAB

#Houses with basic services (energy, sewage, and water)
datademog$SERVICES<-datademog$VPH_C_SERV/datademog$VIVPAR_HAB

#Houses with no energy, sewage, AND water
datademog$NO_SERVICES<-datademog$VPH_SNBIEN/datademog$VIVPAR_HAB

#Houses with car
datademog$CAR<-datademog$VPH_AUTOM/datademog$VIVPAR_HAB

#Houses with mobile phone
datademog$CELULAR<-datademog$VPH_CEL/datademog$VIVPAR_HAB

#Houses with internet
datademog$INTERNET<-datademog$VPH_INTER/datademog$VIVPAR_HAB


write.dta(datademog, file = "/rawdata/demographics.dta")

####Merging databases
rm(list=ls(all=TRUE))

data12<-read.dta("Precinct2012.dta")
data09<-read.dta("Precinct2009.dta")
data06<-read.dta("Precinct2006.dta")
data03<-read.dta("Precinct2003.dta")
datademog<-read.dta("demographics.dta")
datademog$seccionID<-paste(datademog$ENTIDAD_x," _ ",datademog$SECCION_x)


data<-merge(data12, data09,by="seccionID")
data<-merge(data,data06, by="seccionID")
data<-merge(data,data03, by="seccionID")
datat<-merge(data,datademog, by="seccionID")

####Keeping Mexico City and the State of Mexico
data<-datat[datat$ENTIDAD_x %in% c(9,15), ]

write.dta(data,"datawork.dta")

####Estimating distances
rm(list=ls(all=TRUE))

library(maptools)
library(ggplot2)
library(rgeos)
library(Imap)
library(spatstat)
library(taRifx.geo)

#Loading shapefiles
shapefile <- readShapeSpatial('rawdata/Shapefiles/Secciones CD+SIJE+Soriana Zoned.shp', proj4string = CRS("+proj=longlat +datum=WGS84"))

#Finding centroids for each Polygon
centroids<-gCentroid(shapefile,byid=TRUE)
centroids<-data.frame(centroids)
centroids<-as.matrix(centroids)


#Loading Soriana Information
sorianas<-read.dta('/rawdata/Coordenadas.dta')
coordinates(sorianas) <- ~ longitude + latitude
sorianas<-as.matrix(data.frame(sorianas))

#Finding the closest Soriana to each Polygon's centroid
closestsoriana<-matrix(NA,nrow(centroids),1)
for(i in 1:nrow(centroids)){
  distances<-matrix(NA,539,2)
  distances[,1]<-	matrix(gdist(centroids[i,1],centroids[i,2], sorianas[,1],sorianas[,2], units = "km"))
  distances[,2]<-seq(1,539,1)
  min<-min(distances[,1])
  closest<-distances[distances[,1]==min,]
  closestsoriana[i,1]<-closest[2]
}

datum<-read.dbf('rawdata/Shapefiles/Secciones CD+SIJE+Soriana Zoned.dbf')


datum$centroidLONG<-centroids[,1]
datum$centroidLAT<-centroids[,2]
datum$closestSorianaID<-closestsoriana

datum$closestSorianaLONG<-NA
datum$closestSorianaLAT<-NA

for (i in 1:nrow(datum)){
  datum$closestSorianaLONG[i]<-sorianas[datum$closestSorianaID[i],1]
  datum$closestSorianaLAT[i]<-sorianas[datum$closestSorianaID[i],2]	
}

datum<-datum[datum$ENTIDAD %in% c(9,15),]

shapefiles<-shapefile[shapefile$CLAVEGEO %in% datum$CLAVEGEO,]

#area<-gArea(spTransform(shapefile, CRS("+proj=utm +zone=14 +datum=WGS84")),byid=TRUE)/1e6
#area<-data.frame(area)
#area<-as.matrix(area)

datum$area<-NA


for(i in 1:nrow(datum)){
  datum$distance[i]<-	matrix(gdist(datum$centroidLONG[i], datum$centroidLAT[i], datum$closestSorianaLONG[i], datum$closestSorianaLAT[i], units = "km"))
  datum$area[i]<-	gArea(spTransform(shapefiles[shapefiles$CLAVEGEO==datum$CLAVEGEO[i],], CRS("+proj=utm +zone=14 +datum=WGS84")),byid=TRUE)/1e6
  }

#Search for a BingMapsKey on https://msdn.microsoft.com/en-us/library/ff428642.aspx
options(BingMapsKey="AtJ1EJJtJZE2sjHLJrh0yrX9i6GjF4Jv4ok8o1wbG8t52ARZDZCXdJeCARsoJykE")

#The system marks error for 5 observations that are far away from the closest store (>=60kms). These observations are removed later in the analysis when only considering obsed with a distance < 40 kms.

#Driving Distances
drivdist<-matrix(NA, nrow(datum),1)
for(i in 1:nrow(datum)){
  xmat1<- matrix(NA, 2,2)
  xmat1[1,]<-c(datum$centroidLAT[i], datum$centroidLONG[i])
  xmat1[2,]<-c(datum$closestSorianaLAT[i], datum$closestSorianaLONG[i])
  colnames(xmat1) <- c( 'lat', 'lon' )
  drivdist[i]<-georoute( xmat1, service = "bing",  returntype = "distance")$distance
}


datum$drivdist<-drivdist



#Loading the location of the WalMarts
walmart<-read.dta('/rawdata/CoordenadasWalMartBodegas.dta')
walmart<-walmart[,c("longitude","latitude")]
coordinates(walmart) <- ~ longitude + latitude
walmart<-as.matrix(data.frame(walmart))



#Finding the closest store to each Polygon's centroid
datum$closestWMID<-matrix(NA,nrow(datum),1)
for(i in 1:nrow(datum)){
  distances<-matrix(NA,253,2)
  distances[,1]<-  matrix(gdist(datum$centroidLONG[i], datum$centroidLAT[i], walmart[,1],walmart[,2], units = "km"))
  distances[,2]<-seq(1,253,1)
  min<-min(distances[,1])
  closest<-distances[distances[,1]==min,]
  a<-length(closest)
  if(a==2){
    datum$closestWMID[i,1]<-closest[2]
  }else
  {
    datum$closestWMID[i,1]<-closest[1,2]
  }
}


datum$closestWMLONG<-NA
datum$closestWMLAT<-NA

for (i in 1:nrow(datum)){
  datum$closestWMLONG[i]<-walmart[datum$closestWMID[i],1]
  datum$closestWMLAT[i]<-walmart[datum$closestWMID[i],2]  
}

datum$distanceWM<-NA
for(i in 1:nrow(datum)){
  datum$distanceWM[i]<-gdist(datum$centroidLONG[i], datum$centroidLAT[i], datum$closestWMLONG[i], datum$closestWMLAT[i], units="km")
}

datum$seccionID<-paste(datum$ENTIDAD," _ ",datum$SECCION)


datum<-datum[,c("seccionID","centroidLONG","centroidLAT","closestSorianaID","closestSorianaLONG","closestSorianaLAT","distance","drivdist",
                "closestWMLONG","closestWMLAT","closestWMID","distanceWM")]

write.dta(datum, file = "Proximities.dta")

#Merging information to the main database

rm(list=ls(all=TRUE))
library(foreign)
datawork<-read.dta("datawork.dta")
datum<-read.dta("Proximities.dta")

data<-merge(datawork,datum,by="seccionID")


data<-data[data$P_18YMAS>0,]
data$lnpop<-log(data$P_18YMAS)
data$density<-data$ln2012/data$area

#######Transforming vote shares to integer numbers 
data$EPNa<-(data$epn2012/data$ln2012)*100
data$JVMa<-(data$jvm2012/data$ln2012)*100
data$AMLOa<-(data$amlo2012/data$ln2012)*100
data$PART<-(data$total2012/data$ln2012)*100

data$PRI09a<-(data$PRI2009/data$ln2009)*100
data$PRD09a<-(data$PRD2009/data$ln2009)*100
data$PAN09a<-(data$PAN2009/data$ln2009)*100
data$PART09<-(data$total2009/data$ln2009)*100

data$PRI06a<-(data$PRI2006/data$ln2006)*100
data$PRD06a<-(data$PRD2006/data$ln2006)*100
data$PAN06a<-(data$PAN2006/data$ln2006)*100
data$PART06<-(data$total2006/data$ln2006)*100

data$PRI03a<-(data$PRI2003/data$ln2003)*100
data$PRD03a<-(data$PRD2003/data$ln2003)*100
data$PAN03a<-(data$PAN2003/data$ln2003)*100
data$PART03<-(data$total2003/data$ln2003)*100

########Alternative variable codings
#log votes
data$EPNlog<-log((data$epn2012/data$ln2012)/(1-(data$epn2012/data$ln2012)))
data$AMLOlog<-log((data$amlo2012/data$ln2012)/(1-(data$amlo2012/data$ln2012)))
data$JVMlog<-log((data$jvm2012/data$ln2012)/(1-(data$jvm2012/data$ln2012)))
data$PARTlog<-log((data$total2012/data$ln2012)/(100-(data$total2012/data$ln2012)))

#Relative vote shares
data$EPN<-(data$epn2012/data$total2012)*100
data$JVM<-(data$jvm2012/data$total2012)*100
data$AMLO<-(data$amlo2012/data$total2012)*100

data$PRI09<-(data$PRI2009/data$total2009)*100
data$PAN09<-(data$PAN2009/data$total2009)*100
data$PRD09<-(data$PRD2009/data$total2009)*100

data$PRI06<-(data$PRI2006/data$total2006)*100
data$PRD06<-(data$PRD2006/data$total2006)*100
data$PAN06<-(data$PAN2006/data$total2006)*100

data$PRI03<-(data$PRI2003/data$total2003)*100
data$PRD03<-(data$PRD2003/data$total2003)*100
data$PAN03<-(data$PAN2003/data$total2003)*100

#Vote changes
data$deltaPRIa<-data$EPNa-data$PRI09a
data$deltaPRDa<-data$AMLOa-data$PRD09a
data$deltaPANa<-data$JVMa-data$PAN09a
data$deltaPART<-data$PART-data$PART09

data$deltaPRI<-data$EPN-data$PRI09
data$deltaPRD<-data$AMLO-data$PRD09
data$deltaPAN<-data$JVM-data$PAN09

#Party strongholds
data$PRIstr<-0
data$PANstr<-0
data$PRDstr<-0
data$highTURNOUT<-0
data$lowTURNOUT<-0

data$PRIstr[data$PRI09>=50 ]<-1
data$PANstr[data$PAN09>=50 ]<-1
data$PRDstr[data$PRD09>=50 ]<-1
data$highTURNOUT[data$PART09>=(mean(data$PART09)+(sd(data$PART09))) ]<-1
data$lowTURNOUT[data$PART09<=(mean(data$PART09)-(sd(data$PART09)))]<-1

data$PRIstr09<-0
data$PANstr09<-0
data$PRDstr09<-0
data$highTURNOUT09<-0
data$lowTURNOUT09<-0

data$PRIstr09[data$PRI06>=50 ]<-1
data$PANstr09[data$PAN06>=50 ]<-1
data$PRDstr09[data$PRD06>=50 ]<-1
data$highTURNOUT09[data$PART06>=(mean(data$PART06, na.rm=TRUE)+(sd(data$PART06, na.rm=TRUE))) ]<-1
data$lowTURNOUT09[data$PART06<=(mean(data$PART06, na.rm=TRUE)-(sd(data$PART06, na.rm=TRUE)))]<-1  


data$PRIstr06<-0
data$PANstr06<-0
data$PRDstr06<-0
data$highTURNOUT06<-0
data$lowTURNOUT06<-0

data$PRIstr06[data$PRI03>=50 ]<-1
data$PANstr06[data$PAN03>=50 ]<-1
data$PRDstr06[data$PRD03>=50 ]<-1
data$highTURNOUT06[data$PART03>=(mean(data$PART03, na.rm=TRUE)+(sd(data$PART03, na.rm=TRUE))) ]<-1
data$lowTURNOUT06[data$PART03<=(mean(data$PART03, na.rm=TRUE)-(sd(data$PART03, na.rm=TRUE)))]<-1 


#Party strongholds (alternative coding)
data$PRIstrA<-0
data$PANstrA<-0
data$PRDstrA<-0
data$highTURNOUTA<-0
data$lowTURNOUTA<-0

data$PRIstrA[data$PRI09>=55]<-1
data$PANstrA[data$PAN09>=55]<-1
data$PRDstrA[data$PRD09>=55]<-1
data$highTURNOUTA[data$PART09>=(mean(data$PART09)+(sd(data$PART09))) & data$PART06>=(mean(data$PART06)+(sd(data$PART06)))]<-1
data$lowTURNOUTA[data$PART09<=(mean(data$PART09)-(sd(data$PART09))) & data$PART06<=(mean(data$PART06)-(sd(data$PART06)))]<-1

data$PRIstrB<-0
data$PANstrB<-0
data$PRDstrB<-0
data$PRIstrB[data$PRI09>=45]<-1
data$PANstrB[data$PAN09>=45]<-1
data$PRDstrB[data$PRD09>=45]<-1

data$PRIstrC<-0
data$PANstrC<-0
data$PRDstrC<-0
data$PRIstrC[data$PRI09>=mean(data$PRI09)+sd(data$PRI09)]<-1
data$PANstrC[data$PAN09>=mean(data$PAN09)+sd(data$PRD09)]<-1
data$PRDstrC[data$PRD09>=mean(data$PRD09)+sd(data$PAN09)]<-1

#Estimating Proximity
data$proximity1<-(1/data$drivdist)
data$proximity<-(1/data$distance)
data$proximityWM<-(1/data$distanceWM)


#Dropping observations at more than 20 kms of distance from the store.
data<-data[data$distance<=20,]

#IDs for municipality and district
data$id_mun<-paste(data$ENTIDAD_x," _ ",data$NOM_MPIO)
data$id_dist<-paste(data$ENTIDAD_x," _ ",data$DISTRITO_x)

data$id_mun<-factor(data$id_mun)
data$id_dist<-factor(data$id_dist)

write.dta(data, "datawork.dta")


