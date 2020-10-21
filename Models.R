####################################
#### "Grocery for Votes" ###########
#### Replication File ##############
#### Benchmark Models ##############
####################################

install.packages("stargazer")
install.packages("lme4")
install.packages("arm")
install.packages("statmod") 
# if this doesn't work, install from https://cran.r-project.org/web/packages/statmod/index.html 
# run something like:
# install.packages("/LOCAL_PATH_TO_STATMOD/statmod_1.4.35.tgz", repos = NULL, type = .Platform$pkgType)
install.packages("data.table")
# if this doesn't work, install from https://cran.r-project.org/web/packages/data.table/index.html
# run something like:
#install.packages("~/Downloads/data.table_1.13.2.tgz", repos = NULL, type = .Platform$pkgType)
install.packages("rms")
install.packages("msm")
install.packages("systemfit")
install.packages("xtable")
install.packages("gtools")
install.packages("multiwayvcov")
install.packages("numDeriv")
install.packages("interplot")
install.packages("modmarg")
# if this doesn't work, install from https://cran.r-project.org/src/contrib/Archive/modmarg/ 
# run something like:
#install.packages("~/Downloads/modmarg_0.9.2.tar.gz", repos = NULL, type = "source")

rm(list=ls(all=TRUE))
library(foreign)
library(ggplot2)
library(stargazer)
library(lme4) #from Molly
library(statmod) #from Molly
library(arm)
library(rms)
library(msm)
library(systemfit)
library(xtable)
library(gtools)
library(multiwayvcov)
library(numDeriv)
library(interplot)
library(modmarg)

#Declare WDs

wd_m <- '/Users/mollyhickey/Documents/Harvard/G1/Quantitative Methods/Final paper/dataverse_files'

#Set WD

setwd(wd_m)

load("datawork.RData") #updated

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)


####Validity of the instrument

robustness1 <- ols( proximity ~ PRI06+PRD06+PAN06+PART06, data=data, x=TRUE, y=TRUE)

robustness2 <- ols( proximity ~ PRI06+PRD06+PAN06+PART06+
                      id_mun, data=data, x=TRUE, y=TRUE)

robustness3 <- ols( proximity ~ PRI06+PRD06+PAN06+PART06+
                      lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                      EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                      PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                      PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                      CAR+CELULAR+INTERNET+
                     id_mun, data=data, x=TRUE, y=TRUE)


robustness4 <- ols( proximity ~ 
                      lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                      EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                      PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                      PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                      CAR+CELULAR+INTERNET+
                      id_mun, data=data, x=TRUE, y=TRUE)



stargazer(robustness1, robustness2, robustness3, robustness4, star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("PRI 2006", "PRD2006", "PAN2006", "Turnout 2006","Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))



##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################### Benchmark Model ############################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################

data$id_mun<-factor(data$id_mun)
data$id_dist<-factor(data$id_dist)

slopesBM<-matrix(NA,3,4)
seBM<-matrix(NA,3,4)


benchmarkPRI.1 <- lm( EPNa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covBMpri <- cluster.vcov(benchmarkPRI.1, data$id_dist)

coeftest(benchmarkPRI.1, covBMpri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPRI.1$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPRI.1$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPRI.1$coef)




slopesBM[1,1] <- benchmarkPRI.1$coefficients[c("proximity")] + benchmarkPRI.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.1$coefficients[c("proximity:PRIstr")]+  benchmarkPRI.1$coefficients[c("proximity:PRIstr:highTURNOUT")]

slopesBM[2,1] <- benchmarkPRI.1$coefficients[c("proximity")] + benchmarkPRI.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.1$coefficients[c("proximity:PRDstr")]+  benchmarkPRI.1$coefficients[c("proximity:highTURNOUT:PRDstr")]

slopesBM[3,1] <- benchmarkPRI.1$coefficients[c("proximity")] + benchmarkPRI.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.1$coefficients[c("proximity:PANstr")]+  benchmarkPRI.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seBM[1,1]<-sqrt(grad_g_pri%*% covBMpri %*% t(grad_g_pri))

seBM[2,1]<-sqrt(grad_g_prd%*% covBMpri %*% t(grad_g_prd))

seBM[3,1]<-sqrt(grad_g_pan%*% covBMpri %*% t(grad_g_pan))


benchmarkPRD.1 <- lm( AMLOa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covBMprd <- cluster.vcov(benchmarkPRD.1, data$id_dist)

coeftest(benchmarkPRD.1, covBMprd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPRD.1$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPRD.1$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPRD.1$coef)


slopesBM[1,2] <- benchmarkPRD.1$coefficients[c("proximity")] + benchmarkPRD.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.1$coefficients[c("proximity:PRIstr")]+  benchmarkPRD.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesBM[2,2] <- benchmarkPRD.1$coefficients[c("proximity")] + benchmarkPRD.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.1$coefficients[c("proximity:PRDstr")]+  benchmarkPRD.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesBM[3,2] <- benchmarkPRD.1$coefficients[c("proximity")] + benchmarkPRD.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.1$coefficients[c("proximity:PANstr")]+  benchmarkPRD.1$coefficients[c("proximity:highTURNOUT:PANstr")]
seBM[1,2]<-sqrt(grad_g_pri%*% covBMprd %*% t(grad_g_pri))
seBM[2,2]<-sqrt(grad_g_prd%*% covBMprd %*% t(grad_g_prd))
seBM[3,2]<-sqrt(grad_g_pan%*% covBMprd %*% t(grad_g_pan))


benchmarkPAN.1 <- lm( JVMa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covBMpan <- cluster.vcov(benchmarkPAN.1, data$id_dist)

coeftest(benchmarkPAN.1, covBMpan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPAN.1$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPAN.1$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPAN.1$coef)


slopesBM[1,3] <- benchmarkPAN.1$coefficients[c("proximity")] + benchmarkPAN.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.1$coefficients[c("proximity:PRIstr")]+  benchmarkPAN.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesBM[2,3] <- benchmarkPAN.1$coefficients[c("proximity")] + benchmarkPAN.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.1$coefficients[c("proximity:PRDstr")]+  benchmarkPAN.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesBM[3,3] <- benchmarkPAN.1$coefficients[c("proximity")] + benchmarkPAN.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.1$coefficients[c("proximity:PANstr")]+  benchmarkPAN.1$coefficients[c("proximity:highTURNOUT:PANstr")]
seBM[1,3]<-sqrt(grad_g_pri%*% covBMpan %*% t(grad_g_pri))
seBM[2,3]<-sqrt(grad_g_prd%*% covBMpan %*% t(grad_g_prd))
seBM[3,3]<-sqrt(grad_g_pan%*% covBMpan %*% t(grad_g_pan))


benchmarkPART.1 <- lm( PART ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                          PRI09a+PRD09a+PAN09a+PART09+
                          lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                          EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                          PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                          PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                          CAR+CELULAR+INTERNET+
                          id_mun, data=data, x=TRUE, y=TRUE)

covBMpart <- cluster.vcov(benchmarkPART.1, data$id_dist)

coeftest(benchmarkPART.1, covBMpart)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPART.1$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPART.1$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPART.1$coef)

slopesBM[1,4] <- benchmarkPART.1$coefficients[c("proximity")] + benchmarkPART.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.1$coefficients[c("proximity:PRIstr")]+  benchmarkPART.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesBM[2,4] <- benchmarkPART.1$coefficients[c("proximity")] + benchmarkPART.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.1$coefficients[c("proximity:PRDstr")]+  benchmarkPART.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesBM[3,4] <- benchmarkPART.1$coefficients[c("proximity")] + benchmarkPART.1$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.1$coefficients[c("proximity:PANstr")]+  benchmarkPART.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seBM[1,4]<-sqrt(grad_g_pri%*% covBMpart %*% t(grad_g_pri))
seBM[2,4]<-sqrt(grad_g_prd%*% covBMpart %*% t(grad_g_prd))
seBM[3,4]<-sqrt(grad_g_pan%*% covBMpart %*% t(grad_g_pan))

#########################################################
############### TABLE 1 #################################
#########################################################

stargazer(coeftest(benchmarkPRI.1, covBMpri), coeftest(benchmarkPRD.1, covBMprd), coeftest(benchmarkPAN.1, covBMpan), coeftest(benchmarkPART.1, covBMpart),style = "qje", star.cutoffs = c(0.05, 0.01, 0.001), digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

#########################################################
############### TABLE 2 #################################
#########################################################


BM<-matrix(NA,6,4)

for(j in 1:4){
  BM[1,j]<-specify_decimal(slopesBM[1,j], 3)
  BM[2,j]<-paste("(",specify_decimal(seBM[1,j], 3),")", sep="")
  BM[3,j]<-specify_decimal(slopesBM[2,j], 3)
  BM[4,j]<-paste("(",specify_decimal(seBM[2,j], 3),")", sep="")
  BM[5,j]<-specify_decimal(slopesBM[3,j], 3)
  BM[6,j]<-paste("(",specify_decimal(seBM[3,j], 3),")", sep="")
}

xtable(BM)

#########################################################
############### FIGURE 2 #################################
#########################################################

####Predicted vote shares in PRD Mobilized Strongholds

g <- glm(EPNa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
           PRI09a+PRD09a+PAN09a+PART09+
           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
           CAR+CELULAR+INTERNET+
           id_mun, data=data, family = 'gaussian')

at.prox <- seq(1/20, 1/2.5, 1/1000)
marg<-marg(mod = g, var_interest = c("proximity"), type = 'levels', data=data[data$PRDstr==1 & data$highTURNOUT==1,],
           at_var_interest = seq(1/20, 1/2.5, 1/1000),  cofint=.95)

mean <- marg[[1]]$Margin
upper <- marg[[1]]$'Lower CI (95%)'
lower <- marg[[1]]$'Upper CI (95%)'

dataPRI<-data.frame(cbind(at.prox, mean, upper, lower))
dataPRI$distance<-(1/dataPRI$at.prox)

g <- glm(AMLOa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
           PRI09a+PRD09a+PAN09a+PART09+
           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
           CAR+CELULAR+INTERNET+
           id_mun, data=data, family = 'gaussian')

marg<-marg(mod = g, var_interest = c("proximity"), type = 'levels', data=data[data$PRDstr==1 & data$highTURNOUT==1,],
           at_var_interest = seq(1/20, 1/2.5, 1/1000), cofint=.95)

mean <- marg[[1]]$Margin
upper <- marg[[1]]$'Lower CI (95%)'
lower <- marg[[1]]$'Upper CI (95%)'

dataPRD<-data.frame(cbind(at.prox, mean, upper, lower))
dataPRD$distance<-(1/dataPRD$at.prox)


g <- glm(JVMa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
           PRI09a+PRD09a+PAN09a+PART09+
           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
           CAR+CELULAR+INTERNET+
           id_mun, data=data, family = 'gaussian')

marg<-marg(mod = g, var_interest = c("proximity"), type = 'levels', data=data[data$PRDstr==1 & data$highTURNOUT==1,],
           at_var_interest = seq(1/20, 1/2.5, 1/1000),cofint=.95)

mean <- marg[[1]]$Margin
upper <- marg[[1]]$'Lower CI (95%)'
lower <- marg[[1]]$'Upper CI (95%)'

dataPAN<-data.frame(cbind(at.prox, mean, upper, lower))
dataPAN$distance<-(1/dataPAN$at.prox)

datasim<-data.frame(rbind(dataPRI, dataPRD, dataPAN))

datasim$dv<-NA
datasim$dv[1:351]<-"Peña Nieto"
datasim$dv[352:702]<-"López Obrador"
datasim$dv[703:1053]<-"Vázquez Mota"

ggplot()+
  geom_line(data=datasim, aes(x=distance, y=mean,  linetype=dv))+
  geom_ribbon(data=datasim, aes(x=distance,  ymin=lower, ymax=upper, fill=dv),  alpha=0.5)+theme_bw()+
  ylab("Predicted vote shares (%)")+
  xlab("Distance to Soriana (km)")+xlim(2.5,20)+ylim(0,50)+ 
  scale_fill_manual(values = c("#F0E442","red", "deepskyblue2"), name="")+
  scale_linetype_manual(values=c("solid", "longdash","dotted"), name="" )+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        legend.text = element_text(size = 18))

######################


estmean<-coef(covBMpri)
var<-vcov(covBMpri)
at.prox <- seq(1/20, 1/2.5, 1/1000)

slopes <- covBMpri$coefficients[c("proximity")]*at.prox + 
  covBMpri$coefficients[c("PRIstr")] + 
  covBMpri$coefficients[c("highTURNOUT")] + 
  covBMpri$coefficients[c("PRIstr * highTURNOUT")] + 
  covBMpri$coefficients[c("proximity * highTURNOUT")]*at.prox + 
  covBMpri$coefficients[c("proximity * PRIstr")]*at.prox +  
  covBMpri$coefficients[c("proximity * PRIstr * highTURNOUT")]*at.prox

SEs <- rep(NA, length(at.prox))

for (i in 1:length(at.prox)){
  j <- at.prox[i]
  SEs[i] <- deltamethod (~ (x2)*j + (x3) + (x4) + (x140)*j +  (x141) + (x139)*j  + (x146)*j, estmean, var)
}

upper <- slopes + 1.96*SEs
lower <- slopes - 1.96*SEs

dataPRI<-data.frame(cbind(at.prox, slopes, upper, lower))
dataPRI$distance<-(1/dataPRI$at.prox)



estmean<-coef(covBMprd)
var<-vcov(covBMprd)
at.prox <- seq(1/20, 1/2.5, 1/1000)

slopes <- covBMprd$coefficients[c("proximity")]*at.prox + 
  covBMprd$coefficients[c("PRIstr")] + 
  covBMprd$coefficients[c("highTURNOUT")] + 
  covBMprd$coefficients[c("PRIstr * highTURNOUT")] + 
  covBMprd$coefficients[c("proximity * highTURNOUT")]*at.prox + 
  covBMprd$coefficients[c("proximity * PRIstr")]*at.prox +  
  covBMprd$coefficients[c("proximity * PRIstr * highTURNOUT")]*at.prox


SEs <- rep(NA, length(at.prox))

for (i in 1:length(at.prox)){
  j <- at.prox[i]
  SEs[i] <- deltamethod (~ (x2)*j + (x3) + (x4) + (x140)*j +  (x141) + (x139)*j  + (x146)*j, estmean, var)
}

upper <- slopes + 1.96*SEs
lower <- slopes - 1.96*SEs

dataPRD<-data.frame(cbind(at.prox, slopes, upper, lower))
dataPRD$distance<-(1/dataPRD$at.prox)



estmean<-coef(covBMpan)
var<-vcov(covBMpan)
at.prox <- seq(1/20, 1/2.5, 1/1000)

slopes <- covBMpan$coefficients[c("proximity")]*at.prox + 
  covBMpan$coefficients[c("PRIstr")] + 
  covBMpan$coefficients[c("highTURNOUT")] + 
  covBMpan$coefficients[c("PRIstr * highTURNOUT")] + 
  covBMpan$coefficients[c("proximity * highTURNOUT")]*at.prox + 
  covBMpan$coefficients[c("proximity * PRIstr")]*at.prox +  
  covBMpan$coefficients[c("proximity * PRIstr * highTURNOUT")]*at.prox


SEs <- rep(NA, length(at.prox))

for (i in 1:length(at.prox)){
  j <- at.prox[i]
  SEs[i] <- deltamethod (~ (x2)*j + (x3) + (x5) + (x140)*j +  (x141) + (x139)*j  + (x146)*j, estmean, var)
}

upper <- slopes + 1.96*SEs
lower <- slopes - 1.96*SEs

dataPAN<-data.frame(cbind(at.prox, slopes, upper, lower))
dataPAN$distance<-(1/dataPAN$at.prox)



datasim<-data.frame(rbind(dataPRI, dataPRD, dataPAN))

datasim$dv<-NA
datasim$dv[1:351]<-"Peña Nieto"
datasim$dv[352:702]<-"López Obrador"
datasim$dv[703:1053]<-"Vázquez Mota"


ggplot()+
  geom_line(data=datasim, aes(x=distance, y=slopes,  linetype=dv))+
  geom_ribbon(data=datasim, aes(x=distance,  ymin=lower, ymax=upper, fill=dv),  alpha=0.5)+theme_bw()+
  ylab("Marginal effect of mobilized PRI strongholds")+
  xlab("Distance to Soriana (km)")+xlim(2,20)+ylim(-20,20)+ 
  scale_fill_manual(values = c("#F0E442","red", "deepskyblue2"), name="Vote Share")+
  scale_linetype_manual(values=c("solid", "longdash","dotted"), name="Vote Share" )+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        legend.text = element_text(size = 18))


######################


estmean<-coef(covBMpri)
var<-vcov(covBMpri)
at.prox <- seq(1/20, 1/2.5, 1/1000)

slopes <- covBMpri$coefficients[c("proximity")]*at.prox + 
  covBMpri$coefficients[c("PANstr")] + 
  covBMpri$coefficients[c("highTURNOUT")] + 
  covBMpri$coefficients[c("highTURNOUT * PANstr")] + 
  covBMpri$coefficients[c("proximity * highTURNOUT")]*at.prox + 
  covBMpri$coefficients[c("proximity * PANstr")]*at.prox +  
  covBMpri$coefficients[c("proximity * highTURNOUT * PANstr")]*at.prox

SEs <- rep(NA, length(at.prox))

for (i in 1:length(at.prox)){
  j <- at.prox[i]
  SEs[i] <- deltamethod (~ (x2)*j + (x4) + (x6) + (x140)*j +  (x145) + (x144)*j  + (x148)*j, estmean, var)
}

upper <- slopes + 1.96*SEs
lower <- slopes - 1.96*SEs

dataPRI<-data.frame(cbind(at.prox, slopes, upper, lower))
dataPRI$distance<-(1/dataPRI$at.prox)



estmean<-coef(covBMprd)
var<-vcov(covBMprd)
at.prox <- seq(1/20, 1/2.5, 1/1000)

slopes <- covBMprd$coefficients[c("proximity")]*at.prox + 
  covBMprd$coefficients[c("PANstr")] + 
  covBMprd$coefficients[c("highTURNOUT")] + 
  covBMprd$coefficients[c("highTURNOUT * PANstr")] + 
  covBMprd$coefficients[c("proximity * highTURNOUT")]*at.prox + 
  covBMprd$coefficients[c("proximity * PANstr")]*at.prox +  
  covBMprd$coefficients[c("proximity * highTURNOUT * PANstr")]*at.prox

SEs <- rep(NA, length(at.prox))

for (i in 1:length(at.prox)){
  j <- at.prox[i]
  SEs[i] <- deltamethod (~ (x2)*j + (x4) + (x6) + (x140)*j +  (x145) + (x144)*j  + (x148)*j, estmean, var)
}

upper <- slopes + 1.96*SEs
lower <- slopes - 1.96*SEs

dataPRD<-data.frame(cbind(at.prox, slopes, upper, lower))
dataPRD$distance<-(1/dataPRD$at.prox)


estmean<-coef(covBMpan)
var<-vcov(covBMpan)
at.prox <- seq(1/20, 1/2.5, 1/1000)

slopes <- covBMpan$coefficients[c("proximity")]*at.prox + 
  covBMpan$coefficients[c("PANstr")] + 
  covBMpan$coefficients[c("highTURNOUT")] + 
  covBMpan$coefficients[c("highTURNOUT * PANstr")] + 
  covBMpan$coefficients[c("proximity * highTURNOUT")]*at.prox + 
  covBMpan$coefficients[c("proximity * PANstr")]*at.prox +  
  covBMpan$coefficients[c("proximity * highTURNOUT * PANstr")]*at.prox

SEs <- rep(NA, length(at.prox))

for (i in 1:length(at.prox)){
  j <- at.prox[i]
  SEs[i] <- deltamethod (~ (x2)*j + (x4) + (x6) + (x140)*j +  (x145) + (x144)*j  + (x148)*j, estmean, var)
}

upper <- slopes + 1.96*SEs
lower <- slopes - 1.96*SEs

dataPAN<-data.frame(cbind(at.prox, slopes, upper, lower))
dataPAN$distance<-(1/dataPAN$at.prox)



datasim<-data.frame(rbind(dataPRI, dataPRD, dataPAN))

datasim$dv<-NA
datasim$dv[1:351]<-"Peña Nieto"
datasim$dv[352:702]<-"López Obrador"
datasim$dv[703:1053]<-"Vázquez Mota"


ggplot()+
  geom_line(data=datasim, aes(x=distance, y=slopes,  linetype=dv))+
  geom_ribbon(data=datasim, aes(x=distance,  ymin=lower, ymax=upper, fill=dv),  alpha=0.5)+theme_bw()+
  ylab("Marginal effect of mobilized PAN strongholds")+
  xlab("Distance to Soriana (km)")+xlim(2,20)+ylim(-20,20)+ 
  scale_fill_manual(values = c("#F0E442","red", "deepskyblue2"), name="Vote Share")+
  scale_linetype_manual(values=c("solid", "longdash","dotted"), name="Vote Share" )+
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        legend.text = element_text(size = 18))


#########################################################
############### TABLE 3 #################################
#########################################################

###########Estimation of the effects

g <- glm(EPNa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
           PRI09a+PRD09a+PAN09a+PART09+
           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
           CAR+CELULAR+INTERNET+
           id_mun, data=data, family = 'gaussian')

marg<-marg(mod = g, var_interest = c("proximity"), type = 'levels', data=data[data$PRDstr==1 & data$highTURNOUT==1,],
           at_var_interest = c(1/15, 1/2.5))

#At 15 km
PRIat15<-c()
PRIat2.5<-c()
low<-round(sum((marg[[1]]$`Lower CI (95%)`[1]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
high<-round(sum((marg[[1]]$`Upper CI (95%)`[1]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
mean<-round(sum((marg[[1]]$`Margin`[1]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))

PRIat15<-c(sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$epn2012)-mean, sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$epn2012)-low, sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$epn2012)-high)

low<-round(sum((marg[[1]]$`Lower CI (95%)`[2]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
high<-round(sum((marg[[1]]$`Upper CI (95%)`[2]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
mean<-round(sum((marg[[1]]$`Margin`[2]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))

PRIat2.5<-c(mean-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$epn2012), low-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$epn2012), high-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$epn2012))


g <- glm(AMLOa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
           PRI09a+PRD09a+PAN09a+PART09+
           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
           CAR+CELULAR+INTERNET+
           id_mun, data=data, family = 'gaussian')

marg<-marg(mod = g, var_interest = c("proximity"), type = 'levels', data=data[data$PRDstr==1 & data$highTURNOUT==1,],
           at_var_interest = c(1/15, 1/2.5))

#At 15 km
PRDat15<-c()
PRDat2.5<-c()
low<-round(sum((marg[[1]]$`Lower CI (95%)`[1]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
high<-round(sum((marg[[1]]$`Upper CI (95%)`[1]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
mean<-round(sum((marg[[1]]$`Margin`[1]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))

PRDat15<-c(mean-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$amlo2012), low-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$amlo2012), high-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$amlo2012))

low<-round(sum((marg[[1]]$`Lower CI (95%)`[2]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
high<-round(sum((marg[[1]]$`Upper CI (95%)`[2]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))
mean<-round(sum((marg[[1]]$`Margin`[2]/100)*data[data$PRDstr==1 & data$highTURNOUT==1,]$ln2012))

PRDat2.5<-c(mean-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$amlo2012), low-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$amlo2012), high-sum(data[data$PRDstr==1 & data$highTURNOUT==1,]$amlo2012))




##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################### Robustness Checks ##########################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################

#a. Votes as proportion of effective votes

slopesRC1<-matrix(NA,3,3)
seRC1<-matrix(NA,3,3)


robustness1PRI.1 <- lm( EPN ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC1pri <- cluster.vcov(robustness1PRI.1, cluster=data$id_dist)

coeftest(robustness1PRI.1, covRC1pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )}

grad_g_pri <-  jacobian(g_pri, robustness1PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness1PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness1PRI.1$coef)


slopesRC1[1,1] <- robustness1PRI.1$coefficients[c("proximity")] + robustness1PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness1PRI.1$coefficients[c("proximity:PRIstr")]+  robustness1PRI.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC1[2,1] <- robustness1PRI.1$coefficients[c("proximity")] + robustness1PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness1PRI.1$coefficients[c("proximity:PRDstr")]+  robustness1PRI.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC1[3,1] <- robustness1PRI.1$coefficients[c("proximity")] + robustness1PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness1PRI.1$coefficients[c("proximity:PANstr")]+  robustness1PRI.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC1[1,1]<-sqrt(grad_g_pri%*% covRC1pri %*% t(grad_g_pri))
seRC1[2,1]<-sqrt(grad_g_prd%*% covRC1pri %*% t(grad_g_prd))
seRC1[3,1]<-sqrt(grad_g_pan%*% covRC1pri %*% t(grad_g_pan))



robustness1PRD.1 <- lm( AMLO ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC1prd <- cluster.vcov(robustness1PRD.1, cluster=data$id_dist)
coeftest(robustness1PRD.1, covRC1prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )}

grad_g_pri <-  jacobian(g_pri, robustness1PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness1PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness1PRD.1$coef)

slopesRC1[1,2] <- robustness1PRD.1$coefficients[c("proximity")] + robustness1PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness1PRD.1$coefficients[c("proximity:PRIstr")]+  robustness1PRD.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC1[2,2] <- robustness1PRD.1$coefficients[c("proximity")] + robustness1PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness1PRD.1$coefficients[c("proximity:PRDstr")]+  robustness1PRD.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC1[3,2] <- robustness1PRD.1$coefficients[c("proximity")] + robustness1PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness1PRD.1$coefficients[c("proximity:PANstr")]+  robustness1PRD.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC1[1,2]<-sqrt(grad_g_pri%*% covRC1prd %*% t(grad_g_pri))
seRC1[2,2]<-sqrt(grad_g_prd%*% covRC1prd %*% t(grad_g_prd))
seRC1[3,2]<-sqrt(grad_g_pan%*% covRC1prd %*% t(grad_g_pan))

robustness1PAN.1 <- lm( JVM ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC1pan <- cluster.vcov(robustness1PAN.1, cluster=data$id_dist)
coeftest(robustness1PAN.1, covRC1pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness1PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness1PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness1PAN.1$coef)

slopesRC1[1,3] <- robustness1PAN.1$coefficients[c("proximity")] + robustness1PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness1PAN.1$coefficients[c("proximity:PRIstr")]+  robustness1PAN.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC1[2,3] <- robustness1PAN.1$coefficients[c("proximity")] + robustness1PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness1PAN.1$coefficients[c("proximity:PRDstr")]+  robustness1PAN.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC1[3,3] <- robustness1PAN.1$coefficients[c("proximity")] + robustness1PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness1PAN.1$coefficients[c("proximity:PANstr")]+  robustness1PAN.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC1[1,3]<-sqrt(grad_g_pri%*% covRC1pan %*% t(grad_g_pri))
seRC1[2,3]<-sqrt(grad_g_prd%*% covRC1pan %*% t(grad_g_prd))
seRC1[3,3]<-sqrt(grad_g_pan%*% covRC1pan %*% t(grad_g_pan))


stargazer(coeftest(robustness1PRI.1, covRC1pri), coeftest(robustness1PRD.1, covRC1prd), coeftest(robustness1PAN.1, covRC1pan), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))


RC1<-matrix(NA,6,3)

for(j in 1:3){
  RC1[1,j]<-specify_decimal(slopesRC1[1,j], 3)
  RC1[2,j]<-paste("(",specify_decimal(seRC1[1,j],3),")", sep="")
  RC1[3,j]<-specify_decimal(slopesRC1[2,j],3)
  RC1[4,j]<-paste("(",specify_decimal(seRC1[2,j],3),")", sep="")
  RC1[5,j]<-specify_decimal(slopesRC1[3,j],3)
  RC1[6,j]<-paste("(",specify_decimal(seRC1[3,j],3),")", sep="")
}

xtable(RC1)



############
#2. Changes in votes 2009-2012
slopesRC2<-matrix(NA,3,4)
seRC2<-matrix(NA,3,4)

robustness2PRI.1 <- lm( deltaPRIa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC2pri <- cluster.vcov(robustness2PRI.1, cluster=data$id_dist)

coeftest(robustness2PRI.1, covRC2pri)

g_pri <- function(b){
  return( b[2] + b[135] + b[136] + b[142] )}

g_prd <- function(b){
  return( b[2] + b[138] + b[136] + b[143] )}

g_pan <- function(b){
  return( b[2] + b[136] + b[140] + b[144] )}

grad_g_pri <-  jacobian(g_pri, robustness2PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness2PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness2PRI.1$coef)


slopesRC2[1,1] <- robustness2PRI.1$coefficients[c("proximity")] + robustness2PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness2PRI.1$coefficients[c("proximity:PRIstr")]+  robustness2PRI.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC2[2,1] <- robustness2PRI.1$coefficients[c("proximity")] + robustness2PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness2PRI.1$coefficients[c("proximity:PRDstr")]+  robustness2PRI.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC2[3,1] <- robustness2PRI.1$coefficients[c("proximity")] + robustness2PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness2PRI.1$coefficients[c("proximity:PANstr")]+  robustness2PRI.1$coefficients[c("proximity:highTURNOUT:PANstr")]


seRC2[1,1]<-sqrt(grad_g_pri%*% covRC2pri %*% t(grad_g_pri))
seRC2[2,1]<-sqrt(grad_g_prd%*% covRC2pri %*% t(grad_g_prd))
seRC2[3,1]<-sqrt(grad_g_pan%*% covRC2pri %*% t(grad_g_pan))

robustness2PRD.1 <- lm( deltaPRDa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC2prd <- cluster.vcov(robustness2PRD.1, cluster=data$id_dist)

coeftest(robustness2PRD.1, covRC2prd)

g_pri <- function(b){
  return( b[2] + b[135] + b[136] + b[142] )}

g_prd <- function(b){
  return( b[2] + b[138] + b[136] + b[143] )}

g_pan <- function(b){
  return( b[2] + b[136] + b[140] + b[144] )}

grad_g_pri <-  jacobian(g_pri, robustness2PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness2PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness2PRD.1$coef)


slopesRC2[1,2] <- robustness2PRD.1$coefficients[c("proximity")] + robustness2PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness2PRD.1$coefficients[c("proximity:PRIstr")]+  robustness2PRD.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC2[2,2] <- robustness2PRD.1$coefficients[c("proximity")] + robustness2PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness2PRD.1$coefficients[c("proximity:PRDstr")]+  robustness2PRD.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC2[3,2] <- robustness2PRD.1$coefficients[c("proximity")] + robustness2PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness2PRD.1$coefficients[c("proximity:PANstr")]+  robustness2PRD.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC2[1,2]<-sqrt(grad_g_pri%*% covRC2prd %*% t(grad_g_pri))
seRC2[2,2]<-sqrt(grad_g_prd%*% covRC2prd %*% t(grad_g_prd))
seRC2[3,2]<-sqrt(grad_g_pan%*% covRC2prd %*% t(grad_g_pan))


robustness2PAN.1 <- lm( deltaPANa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC2pan <- cluster.vcov(robustness2PAN.1, cluster=data$id_dist)

coeftest(robustness2PAN.1, covRC2pan)

g_pri <- function(b){
  return( b[2] + b[135] + b[136] + b[142] )}

g_prd <- function(b){
  return( b[2] + b[138] + b[136] + b[143] )}

g_pan <- function(b){
  return( b[2] + b[136] + b[140] + b[144] )}

grad_g_pri <-  jacobian(g_pri, robustness2PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness2PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness2PAN.1$coef)


slopesRC2[1,3] <- robustness2PAN.1$coefficients[c("proximity")] + robustness2PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness2PAN.1$coefficients[c("proximity:PRIstr")]+  robustness2PAN.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC2[2,3] <- robustness2PAN.1$coefficients[c("proximity")] + robustness2PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness2PAN.1$coefficients[c("proximity:PRDstr")]+  robustness2PAN.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC2[3,3] <- robustness2PAN.1$coefficients[c("proximity")] + robustness2PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness2PAN.1$coefficients[c("proximity:PANstr")]+  robustness2PAN.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC2[1,3]<-sqrt(grad_g_pri%*% covRC2pan %*% t(grad_g_pri))
seRC2[2,3]<-sqrt(grad_g_prd%*% covRC2pan %*% t(grad_g_prd))
seRC2[3,3]<-sqrt(grad_g_pan%*% covRC2pan %*% t(grad_g_pan))


robustness2PART.1 <- lm( deltaPART ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                          lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                          EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                          PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                          PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                          CAR+CELULAR+INTERNET+
                          id_mun, data=data, x=TRUE, y=TRUE)

covRC2part <- cluster.vcov(robustness2PART.1, cluster=data$id_dist)

coeftest(robustness2PART.1, covRC2part)

g_pri <- function(b){
  return( b[2] + b[135] + b[136] + b[142] )}

g_prd <- function(b){
  return( b[2] + b[138] + b[136] + b[143] )}

g_pan <- function(b){
  return( b[2] + b[136] + b[140] + b[144] )}

grad_g_pri <-  jacobian(g_pri, robustness2PART.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness2PART.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness2PART.1$coef)

slopesRC2[1,4] <- robustness2PART.1$coefficients[c("proximity")] + robustness2PART.1$coefficients[c("proximity:highTURNOUT")] + robustness2PART.1$coefficients[c("proximity:PRIstr")]+  robustness2PART.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC2[2,4] <- robustness2PART.1$coefficients[c("proximity")] + robustness2PART.1$coefficients[c("proximity:highTURNOUT")] + robustness2PART.1$coefficients[c("proximity:PRDstr")]+  robustness2PART.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC2[3,4] <- robustness2PART.1$coefficients[c("proximity")] + robustness2PART.1$coefficients[c("proximity:highTURNOUT")] + robustness2PART.1$coefficients[c("proximity:PANstr")]+  robustness2PART.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC2[1,4]<-sqrt(grad_g_pri%*% covRC2part %*% t(grad_g_pri))
seRC2[2,4]<-sqrt(grad_g_prd%*% covRC2part %*% t(grad_g_prd))
seRC2[3,4]<-sqrt(grad_g_pan%*% covRC2part %*% t(grad_g_pan))

stargazer(coeftest(robustness2PRI.1, covRC2pri),coeftest(robustness2PRD.1, covRC2prd), coeftest(robustness2PAN.1, covRC2pan), coeftest(robustness2PART.1, covRC2part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))


RC2<-matrix(NA,6,4)

for(j in 1:4){
  RC2[1,j]<-specify_decimal(slopesRC2[1,j], 3)
  RC2[2,j]<-paste("(",specify_decimal(seRC2[1,j],3),")", sep="")
  RC2[3,j]<-specify_decimal(slopesRC2[2,j],3)
  RC2[4,j]<-paste("(",specify_decimal(seRC2[2,j],3),")", sep="")
  RC2[5,j]<-specify_decimal(slopesRC2[3,j],3)
  RC2[6,j]<-paste("(",specify_decimal(seRC2[3,j],3),")", sep="")
}

xtable(RC2)



#####3. Driving Distance
slopesRC3<-matrix(NA,3,4)
seRC3<-matrix(NA,3,4)


robustness3PRI.1 <- lm( EPN ~ proximity1*PRIstr*highTURNOUT+proximity1*PRDstr*highTURNOUT+proximity1*PANstr*highTURNOUT+
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC3pri <- cluster.vcov(robustness3PRI.1, data$id_dist)

coeftest(robustness3PRI.1, covRC3pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness3PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness3PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness3PRI.1$coef)


slopesRC3[1,1] <- robustness3PRI.1$coefficients[c("proximity1")] + robustness3PRI.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PRI.1$coefficients[c("proximity1:PRIstr")]+  robustness3PRI.1$coefficients[c("proximity1:PRIstr:highTURNOUT")]
slopesRC3[2,1] <- robustness3PRI.1$coefficients[c("proximity1")] + robustness3PRI.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PRI.1$coefficients[c("proximity1:PRDstr")]+  robustness3PRI.1$coefficients[c("proximity1:highTURNOUT:PRDstr")]
slopesRC3[3,1] <- robustness3PRI.1$coefficients[c("proximity1")] + robustness3PRI.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PRI.1$coefficients[c("proximity1:PANstr")]+  robustness3PRI.1$coefficients[c("proximity1:highTURNOUT:PANstr")]

seRC3[1,1]<-sqrt(grad_g_pri%*% covRC3pri %*% t(grad_g_pri))
seRC3[2,1]<-sqrt(grad_g_prd%*% covRC3pri %*% t(grad_g_prd))
seRC3[3,1]<-sqrt(grad_g_pan%*% covRC3pri %*% t(grad_g_pan))

robustness3PRD.1 <- lm( AMLO ~ proximity1*PRIstr*highTURNOUT+proximity1*PRDstr*highTURNOUT+proximity1*PANstr*highTURNOUT+
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+id_mun, data=data, x=TRUE, y=TRUE)

covRC3prd <- cluster.vcov(robustness3PRD.1, data$id_dist)

coeftest(robustness3PRD.1, covRC3prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness3PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness3PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness3PRD.1$coef)


slopesRC3[1,2] <- robustness3PRD.1$coefficients[c("proximity1")] + robustness3PRD.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PRD.1$coefficients[c("proximity1:PRIstr")]+  robustness3PRD.1$coefficients[c("proximity1:PRIstr:highTURNOUT")]
slopesRC3[2,2] <- robustness3PRD.1$coefficients[c("proximity1")] + robustness3PRD.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PRD.1$coefficients[c("proximity1:PRDstr")]+  robustness3PRD.1$coefficients[c("proximity1:highTURNOUT:PRDstr")]
slopesRC3[3,2] <- robustness3PRD.1$coefficients[c("proximity1")] + robustness3PRD.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PRD.1$coefficients[c("proximity1:PANstr")]+  robustness3PRD.1$coefficients[c("proximity1:highTURNOUT:PANstr")]

seRC3[1,2]<-sqrt(grad_g_pri%*% covRC3prd %*% t(grad_g_pri))
seRC3[2,2]<-sqrt(grad_g_prd%*% covRC3prd %*% t(grad_g_prd))
seRC3[3,2]<-sqrt(grad_g_pan%*% covRC3prd %*% t(grad_g_pan))

robustness3PAN.1 <- lm( JVM ~ proximity1*PRIstr*highTURNOUT+proximity1*PRDstr*highTURNOUT+proximity1*PANstr*highTURNOUT+
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC3pan <- cluster.vcov(robustness3PAN.1, data$id_dist)

coeftest(robustness3PAN.1, covRC3pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness3PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness3PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness3PAN.1$coef)


slopesRC3[1,3] <- robustness3PAN.1$coefficients[c("proximity1")] + robustness3PAN.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PAN.1$coefficients[c("proximity1:PRIstr")]+  robustness3PAN.1$coefficients[c("proximity1:PRIstr:highTURNOUT")]
slopesRC3[2,3] <- robustness3PAN.1$coefficients[c("proximity1")] + robustness3PAN.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PAN.1$coefficients[c("proximity1:PRDstr")]+  robustness3PAN.1$coefficients[c("proximity1:highTURNOUT:PRDstr")]
slopesRC3[3,3] <- robustness3PAN.1$coefficients[c("proximity1")] + robustness3PAN.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PAN.1$coefficients[c("proximity1:PANstr")]+  robustness3PAN.1$coefficients[c("proximity1:highTURNOUT:PANstr")]

seRC3[1,3]<-sqrt(grad_g_pri%*% covRC3pan %*% t(grad_g_pri))
seRC3[2,3]<-sqrt(grad_g_prd%*% covRC3pan %*% t(grad_g_prd))
seRC3[3,3]<-sqrt(grad_g_pan%*% covRC3pan %*% t(grad_g_pan))

robustness3PART.1 <- lm( PART ~ proximity1*PRIstr*highTURNOUT+proximity1*PRDstr*highTURNOUT+proximity1*PANstr*highTURNOUT+
                            PRI09a+PRD09a+PAN09a+PART09+
                            lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                            EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                            PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                            PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                            CAR+CELULAR+INTERNET+
                            id_mun, data=data, x=TRUE, y=TRUE)

covRC3part <- cluster.vcov(robustness3PART.1, data$id_dist)

coeftest(robustness3PART.1, covRC3part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness3PART.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness3PART.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness3PART.1$coef)


slopesRC3[1,4] <- robustness3PART.1$coefficients[c("proximity1")] + robustness3PART.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PART.1$coefficients[c("proximity1:PRIstr")]+  robustness3PART.1$coefficients[c("proximity1:PRIstr:highTURNOUT")]
slopesRC3[2,4] <- robustness3PART.1$coefficients[c("proximity1")] + robustness3PART.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PART.1$coefficients[c("proximity1:PRDstr")]+  robustness3PART.1$coefficients[c("proximity1:highTURNOUT:PRDstr")]
slopesRC3[3,4] <- robustness3PART.1$coefficients[c("proximity1")] + robustness3PART.1$coefficients[c("proximity1:highTURNOUT")] + robustness3PART.1$coefficients[c("proximity1:PANstr")]+  robustness3PART.1$coefficients[c("proximity1:highTURNOUT:PANstr")]

seRC3[1,4]<-sqrt(grad_g_pri%*% covRC3part %*% t(grad_g_pri))
seRC3[2,4]<-sqrt(grad_g_prd%*% covRC3part %*% t(grad_g_prd))
seRC3[3,4]<-sqrt(grad_g_pan%*% covRC3part %*% t(grad_g_pan))

stargazer(coeftest(robustness3PRI.1, covRC3pri), coeftest(robustness3PRD.1, covRC3prd), coeftest(robustness3PAN.1, covRC3pan), coeftest(robustness3PART.1, covRC3part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD 2009", "PAN 2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))



RC3<-matrix(NA,6,4)

for(j in 1:4){
  RC3[1,j]<-specify_decimal(slopesRC3[1,j], 3)
  RC3[2,j]<-paste("(",specify_decimal(seRC3[1,j],3),")", sep="")
  RC3[3,j]<-specify_decimal(slopesRC3[2,j],3)
  RC3[4,j]<-paste("(",specify_decimal(seRC3[2,j],3),")", sep="")
  RC3[5,j]<-specify_decimal(slopesRC3[3,j],3)
  RC3[6,j]<-paste("(",specify_decimal(seRC3[3,j],3),")", sep="")
}

xtable(RC3)


#d. Logit transformation of the dependent variable


slopesRC4<-matrix(NA,3,4)
seRC4<-matrix(NA,3,4)


robustness4PRI.1 <- lm( EPNlog ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC4pri <- cluster.vcov(robustness4PRI.1, data$id_dist)

coeftest(robustness4PRI.1, covRC4pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness4PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness4PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness4PRI.1$coef)


slopesRC4[1,1] <- robustness4PRI.1$coefficients[c("proximity")] + robustness4PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness4PRI.1$coefficients[c("proximity:PRIstr")]+  robustness4PRI.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC4[2,1] <- robustness4PRI.1$coefficients[c("proximity")] + robustness4PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness4PRI.1$coefficients[c("proximity:PRDstr")]+  robustness4PRI.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC4[3,1] <- robustness4PRI.1$coefficients[c("proximity")] + robustness4PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness4PRI.1$coefficients[c("proximity:PANstr")]+  robustness4PRI.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC4[1,1]<-sqrt(grad_g_pri%*% covRC4pri %*% t(grad_g_pri))
seRC4[2,1]<-sqrt(grad_g_prd%*% covRC4pri %*% t(grad_g_prd))
seRC4[3,1]<-sqrt(grad_g_pan%*% covRC4pri %*% t(grad_g_pan))

robustness4PRD.1 <- lm( AMLOlog ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC4prd <- cluster.vcov(robustness4PRD.1, data$id_dist)

coeftest(robustness4PRD.1, covRC4prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness4PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness4PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness4PRD.1$coef)


slopesRC4[1,2] <- robustness4PRD.1$coefficients[c("proximity")] + robustness4PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness4PRD.1$coefficients[c("proximity:PRIstr")]+  robustness4PRD.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC4[2,2] <- robustness4PRD.1$coefficients[c("proximity")] + robustness4PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness4PRD.1$coefficients[c("proximity:PRDstr")]+  robustness4PRD.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC4[3,2] <- robustness4PRD.1$coefficients[c("proximity")] + robustness4PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness4PRD.1$coefficients[c("proximity:PANstr")]+  robustness4PRD.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC4[1,2]<-sqrt(grad_g_pri%*% covRC4prd %*% t(grad_g_pri))
seRC4[2,2]<-sqrt(grad_g_prd%*% covRC4prd %*% t(grad_g_prd))
seRC4[3,2]<-sqrt(grad_g_pan%*% covRC4prd %*% t(grad_g_pan))


robustness4PAN.1 <- lm( JVMlog ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC4pan <- cluster.vcov(robustness4PAN.1, data$id_dist)

coeftest(robustness4PAN.1, covRC4pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness4PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness4PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness4PAN.1$coef)


slopesRC4[1,3] <- robustness4PAN.1$coefficients[c("proximity")] + robustness4PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness4PAN.1$coefficients[c("proximity:PRIstr")]+  robustness4PAN.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC4[2,3] <- robustness4PAN.1$coefficients[c("proximity")] + robustness4PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness4PAN.1$coefficients[c("proximity:PRDstr")]+  robustness4PAN.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC4[3,3] <- robustness4PAN.1$coefficients[c("proximity")] + robustness4PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness4PAN.1$coefficients[c("proximity:PANstr")]+  robustness4PAN.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC4[1,3]<-sqrt(grad_g_pri%*% covRC4pan %*% t(grad_g_pri))
seRC4[2,3]<-sqrt(grad_g_prd%*% covRC4pan %*% t(grad_g_prd))
seRC4[3,3]<-sqrt(grad_g_pan%*% covRC4pan %*% t(grad_g_pan))

robustness4PART.1 <- lm( PARTlog ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                            PRI09a+PRD09a+PAN09a+PART09+
                            lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                            EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                            PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                            PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                            CAR+CELULAR+INTERNET+
                            id_mun, data=data, x=TRUE, y=TRUE)

covRC4part <- cluster.vcov(robustness4PART.1, data$id_dist)

coeftest(robustness4PART.1, covRC4part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness4PART.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness4PART.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness4PART.1$coef)


slopesRC4[1,4] <- robustness4PART.1$coefficients[c("proximity")] + robustness4PART.1$coefficients[c("proximity:highTURNOUT")] + robustness4PART.1$coefficients[c("proximity:PRIstr")]+  robustness4PART.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesRC4[2,4] <- robustness4PART.1$coefficients[c("proximity")] + robustness4PART.1$coefficients[c("proximity:highTURNOUT")] + robustness4PART.1$coefficients[c("proximity:PRDstr")]+  robustness4PART.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesRC4[3,4] <- robustness4PART.1$coefficients[c("proximity")] + robustness4PART.1$coefficients[c("proximity:highTURNOUT")] + robustness4PART.1$coefficients[c("proximity:PANstr")]+  robustness4PART.1$coefficients[c("proximity:highTURNOUT:PANstr")]

seRC4[1,4]<-sqrt(grad_g_pri%*% covRC3part %*% t(grad_g_pri))
seRC4[2,4]<-sqrt(grad_g_prd%*% covRC3part %*% t(grad_g_prd))
seRC4[3,4]<-sqrt(grad_g_pan%*% covRC3part %*% t(grad_g_pan))

stargazer(coeftest(robustness4PRI.1, covRC4pri), coeftest(robustness4PRD.1, covRC4prd), coeftest(robustness4PAN.1, covRC4pan), coeftest(robustness4PART.1, covRC4part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))


RC4<-matrix(NA,6,4)

for(j in 1:4){
  RC4[1,j]<-specify_decimal(slopesRC4[1,j], 3)
  RC4[2,j]<-paste("(",specify_decimal(seRC4[1,j],3),")", sep="")
  RC4[3,j]<-specify_decimal(slopesRC4[2,j],3)
  RC4[4,j]<-paste("(",specify_decimal(seRC4[2,j],3),")", sep="")
  RC4[5,j]<-specify_decimal(slopesRC4[3,j],3)
  RC4[6,j]<-paste("(",specify_decimal(seRC4[3,j],3),")", sep="")
}

xtable(RC4)


######Alternative coding for party strongholds (>1 SD from mean party vote)
slopesRC5<-matrix(NA,3,4)
seRC5<-matrix(NA,3,4)


robustness5PRI.1 <- lm( EPNa ~ proximity*PRIstrC*highTURNOUT+proximity*PRDstrC*highTURNOUT+proximity*PANstrC*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC5pri <- cluster.vcov(robustness5PRI.1, data$id_dist)

coeftest(robustness5PRI.1, covRC5pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness5PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness5PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness5PRI.1$coef)


slopesRC5[1,1] <- robustness5PRI.1$coefficients[c("proximity")] + robustness5PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness5PRI.1$coefficients[c("proximity:PRIstrC")]+  robustness5PRI.1$coefficients[c("proximity:PRIstrC:highTURNOUT")]
slopesRC5[2,1] <- robustness5PRI.1$coefficients[c("proximity")] + robustness5PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness5PRI.1$coefficients[c("proximity:PRDstrC")]+  robustness5PRI.1$coefficients[c("proximity:highTURNOUT:PRDstrC")]
slopesRC5[3,1] <- robustness5PRI.1$coefficients[c("proximity")] + robustness5PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness5PRI.1$coefficients[c("proximity:PANstrC")]+  robustness5PRI.1$coefficients[c("proximity:highTURNOUT:PANstrC")]

seRC5[1,1]<-sqrt(grad_g_pri%*% covRC5pri %*% t(grad_g_pri))
seRC5[2,1]<-sqrt(grad_g_prd%*% covRC5pri %*% t(grad_g_prd))
seRC5[3,1]<-sqrt(grad_g_pan%*% covRC5pri %*% t(grad_g_pan))

robustness5PRD.1 <- lm( AMLOa ~ proximity*PRIstrC*highTURNOUT+proximity*PRDstrC*highTURNOUT+proximity*PANstrC*highTURNOUT+     
                          PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
                          lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                          EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                          PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                          PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                          CAR+CELULAR+INTERNET+
                          id_mun, data=data, x=TRUE, y=TRUE)

covRC5prd <- cluster.vcov(robustness5PRD.1, data$id_dist)

coeftest(robustness5PRD.1, covRC5prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness5PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness5PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness5PRD.1$coef)


slopesRC5[1,2] <- robustness5PRD.1$coefficients[c("proximity")] + robustness5PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness5PRD.1$coefficients[c("proximity:PRIstrC")]+  robustness5PRD.1$coefficients[c("proximity:PRIstrC:highTURNOUT")]
slopesRC5[2,2] <- robustness5PRD.1$coefficients[c("proximity")] + robustness5PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness5PRD.1$coefficients[c("proximity:PRDstrC")]+  robustness5PRD.1$coefficients[c("proximity:highTURNOUT:PRDstrC")]
slopesRC5[3,2] <- robustness5PRD.1$coefficients[c("proximity")] + robustness5PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness5PRD.1$coefficients[c("proximity:PANstrC")]+  robustness5PRD.1$coefficients[c("proximity:highTURNOUT:PANstrC")]

seRC5[1,2]<-sqrt(grad_g_pri%*% covRC5prd %*% t(grad_g_pri))
seRC5[2,2]<-sqrt(grad_g_prd%*% covRC5prd %*% t(grad_g_prd))
seRC5[3,2]<-sqrt(grad_g_pan%*% covRC5prd %*% t(grad_g_pan))


robustness5PAN.1 <- lm( JVMa ~ proximity*PRIstrC*highTURNOUT+proximity*PRDstrC*highTURNOUT+proximity*PANstrC*highTURNOUT+   
                           PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC5pan <- cluster.vcov(robustness5PAN.1, data$id_dist)

coeftest(robustness5PAN.1, covRC5pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness5PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness5PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness5PAN.1$coef)


slopesRC5[1,3] <- robustness5PAN.1$coefficients[c("proximity")] + robustness5PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness5PAN.1$coefficients[c("proximity:PRIstrC")]+  robustness5PAN.1$coefficients[c("proximity:PRIstrC:highTURNOUT")]
slopesRC5[2,3] <- robustness5PAN.1$coefficients[c("proximity")] + robustness5PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness5PAN.1$coefficients[c("proximity:PRDstrC")]+  robustness5PAN.1$coefficients[c("proximity:highTURNOUT:PRDstrC")]
slopesRC5[3,3] <- robustness5PAN.1$coefficients[c("proximity")] + robustness5PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness5PAN.1$coefficients[c("proximity:PANstrC")]+  robustness5PAN.1$coefficients[c("proximity:highTURNOUT:PANstrC")]

seRC5[1,3]<-sqrt(grad_g_pri%*% covRC5pan %*% t(grad_g_pri))
seRC5[2,3]<-sqrt(grad_g_prd%*% covRC5pan %*% t(grad_g_prd))
seRC5[3,3]<-sqrt(grad_g_pan%*% covRC5pan %*% t(grad_g_pan))

robustness5PART.1 <- lm( PART ~ proximity*PRIstrC*highTURNOUT+proximity*PRDstrC*highTURNOUT+proximity*PANstrC*highTURNOUT+   
                            PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
                            lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                            EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                            PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                            PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                            CAR+CELULAR+INTERNET+
                            id_mun, data=data, x=TRUE, y=TRUE)

covRC5part <- cluster.vcov(robustness5PART.1, data$id_dist)

coeftest(robustness5PART.1, covRC5part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness5PART.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness5PART.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness5PART.1$coef)


slopesRC5[1,4] <- robustness5PART.1$coefficients[c("proximity")] + robustness5PART.1$coefficients[c("proximity:highTURNOUT")] + robustness5PART.1$coefficients[c("proximity:PRIstrC")]+  robustness5PART.1$coefficients[c("proximity:PRIstrC:highTURNOUT")]
slopesRC5[2,4] <- robustness5PART.1$coefficients[c("proximity")] + robustness5PART.1$coefficients[c("proximity:highTURNOUT")] + robustness5PART.1$coefficients[c("proximity:PRDstrC")]+  robustness5PART.1$coefficients[c("proximity:highTURNOUT:PRDstrC")]
slopesRC5[3,4] <- robustness5PART.1$coefficients[c("proximity")] + robustness5PART.1$coefficients[c("proximity:highTURNOUT")] + robustness5PART.1$coefficients[c("proximity:PANstrC")]+  robustness5PART.1$coefficients[c("proximity:highTURNOUT:PANstrC")]

seRC5[1,4]<-sqrt(grad_g_pri%*% covRC5part %*% t(grad_g_pri))
seRC5[2,4]<-sqrt(grad_g_prd%*% covRC5part %*% t(grad_g_prd))
seRC5[3,4]<-sqrt(grad_g_pan%*% covRC5part %*% t(grad_g_pan))

stargazer(coeftest(robustness5PRI.1, covRC5pri), coeftest(robustness5PRD.1, covRC5prd), coeftest(robustness5PAN.1, covRC5pan), coeftest(robustness5PART.1, covRC5part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

RC5<-matrix(NA,6,4)

for(j in 1:4){
  RC5[1,j]<-specify_decimal(slopesRC5[1,j], 3)
  RC5[2,j]<-paste("(",specify_decimal(seRC5[1,j],3),")", sep="")
  RC5[3,j]<-specify_decimal(slopesRC5[2,j],3)
  RC5[4,j]<-paste("(",specify_decimal(seRC5[2,j],3),")", sep="")
  RC5[5,j]<-specify_decimal(slopesRC5[3,j],3)
  RC5[6,j]<-paste("(",specify_decimal(seRC5[3,j],3),")", sep="")
}

xtable(RC5)


#########Alternative specification of party stronghold (Party vote share >45%)

slopesRC6<-matrix(NA,3,4)
seRC6<-matrix(NA,3,4)


robustness6PRI.1 <- lm( EPNa ~ proximity*PRIstrB*highTURNOUT+proximity*PRDstrB*highTURNOUT+proximity*PANstrB*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC6pri <- cluster.vcov(robustness6PRI.1, data$id_dist)

coeftest(robustness6PRI.1, covRC6pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness6PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness6PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness6PRI.1$coef)


slopesRC6[1,1] <- robustness6PRI.1$coefficients[c("proximity")] + robustness6PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness6PRI.1$coefficients[c("proximity:PRIstrB")]+  robustness6PRI.1$coefficients[c("proximity:PRIstrB:highTURNOUT")]
slopesRC6[2,1] <- robustness6PRI.1$coefficients[c("proximity")] + robustness6PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness6PRI.1$coefficients[c("proximity:PRDstrB")]+  robustness6PRI.1$coefficients[c("proximity:highTURNOUT:PRDstrB")]
slopesRC6[3,1] <- robustness6PRI.1$coefficients[c("proximity")] + robustness6PRI.1$coefficients[c("proximity:highTURNOUT")] + robustness6PRI.1$coefficients[c("proximity:PANstrB")]+  robustness6PRI.1$coefficients[c("proximity:highTURNOUT:PANstrB")]

seRC6[1,1]<-sqrt(grad_g_pri%*% covRC6pri %*% t(grad_g_pri))
seRC6[2,1]<-sqrt(grad_g_prd%*% covRC6pri %*% t(grad_g_prd))
seRC6[3,1]<-sqrt(grad_g_pan%*% covRC6pri %*% t(grad_g_pan))


robustness6PRD.1 <- lm( AMLOa ~ proximity*PRIstrB*highTURNOUT+proximity*PRDstrB*highTURNOUT+proximity*PANstrB*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC6prd <- cluster.vcov(robustness6PRD.1, data$id_dist)

coeftest(robustness6PRD.1, covRC6prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness6PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness6PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness6PRD.1$coef)


slopesRC6[1,2] <- robustness6PRD.1$coefficients[c("proximity")] + robustness6PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness6PRD.1$coefficients[c("proximity:PRIstrB")]+  robustness6PRD.1$coefficients[c("proximity:PRIstrB:highTURNOUT")]
slopesRC6[2,2] <- robustness6PRD.1$coefficients[c("proximity")] + robustness6PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness6PRD.1$coefficients[c("proximity:PRDstrB")]+  robustness6PRD.1$coefficients[c("proximity:highTURNOUT:PRDstrB")]
slopesRC6[3,2] <- robustness6PRD.1$coefficients[c("proximity")] + robustness6PRD.1$coefficients[c("proximity:highTURNOUT")] + robustness6PRD.1$coefficients[c("proximity:PANstrB")]+  robustness6PRD.1$coefficients[c("proximity:highTURNOUT:PANstrB")]

seRC6[1,2]<-sqrt(grad_g_pri%*% covRC6prd %*% t(grad_g_pri))
seRC6[2,2]<-sqrt(grad_g_prd%*% covRC6prd %*% t(grad_g_prd))
seRC6[3,2]<-sqrt(grad_g_pan%*% covRC6prd %*% t(grad_g_pan))

robustness6PAN.1 <- lm( JVMa ~ proximity*PRIstrB*highTURNOUT+proximity*PRDstrB*highTURNOUT+proximity*PANstrB*highTURNOUT+     
                           PRI09a+PRD09a+PAN09a+PART09+
                           lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                           EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                           PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                           PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                           CAR+CELULAR+INTERNET+
                           id_mun, data=data, x=TRUE, y=TRUE)

covRC6pan <- cluster.vcov(robustness6PAN.1, data$id_dist)

coeftest(robustness6PAN.1, covRC6pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness6PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness6PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness6PAN.1$coef)


slopesRC6[1,3] <- robustness6PAN.1$coefficients[c("proximity")] + robustness6PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness6PAN.1$coefficients[c("proximity:PRIstrB")]+  robustness6PAN.1$coefficients[c("proximity:PRIstrB:highTURNOUT")]
slopesRC6[2,3] <- robustness6PAN.1$coefficients[c("proximity")] + robustness6PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness6PAN.1$coefficients[c("proximity:PRDstrB")]+  robustness6PAN.1$coefficients[c("proximity:highTURNOUT:PRDstrB")]
slopesRC6[3,3] <- robustness6PAN.1$coefficients[c("proximity")] + robustness6PAN.1$coefficients[c("proximity:highTURNOUT")] + robustness6PAN.1$coefficients[c("proximity:PANstrB")]+  robustness6PAN.1$coefficients[c("proximity:highTURNOUT:PANstrB")]

seRC6[1,3]<-sqrt(grad_g_pri%*% covRC6pan %*% t(grad_g_pri))
seRC6[2,3]<-sqrt(grad_g_prd%*% covRC6pan %*% t(grad_g_prd))
seRC6[3,3]<-sqrt(grad_g_pan%*% covRC6pan %*% t(grad_g_pan))

robustness6PART.1 <- lm( PART ~ proximity*PRIstrB*highTURNOUT+proximity*PRDstrB*highTURNOUT+proximity*PANstrB*highTURNOUT+     
                            PRI09a+PRD09a+PAN09a+PART09+
                            lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                            EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                            PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                            PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                            CAR+CELULAR+INTERNET+
                            id_mun, data=data, x=TRUE, y=TRUE)

covRC6part <- cluster.vcov(robustness6PART.1, data$id_dist)

coeftest(robustness6PART.1, covRC6part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, robustness6PART.1$coef)
grad_g_prd <-  jacobian(g_prd, robustness6PART.1$coef)
grad_g_pan <-  jacobian(g_pan, robustness6PART.1$coef)


slopesRC6[1,4] <- robustness6PART.1$coefficients[c("proximity")] + robustness6PART.1$coefficients[c("proximity:highTURNOUT")] + robustness6PART.1$coefficients[c("proximity:PRIstrB")]+  robustness6PART.1$coefficients[c("proximity:PRIstrB:highTURNOUT")]
slopesRC6[2,4] <- robustness6PART.1$coefficients[c("proximity")] + robustness6PART.1$coefficients[c("proximity:highTURNOUT")] + robustness6PART.1$coefficients[c("proximity:PRDstrB")]+  robustness6PART.1$coefficients[c("proximity:highTURNOUT:PRDstrB")]
slopesRC6[3,4] <- robustness6PART.1$coefficients[c("proximity")] + robustness6PART.1$coefficients[c("proximity:highTURNOUT")] + robustness6PART.1$coefficients[c("proximity:PANstrB")]+  robustness6PART.1$coefficients[c("proximity:highTURNOUT:PANstrB")]

seRC6[1,4]<-sqrt(grad_g_pri%*% covRC6part %*% t(grad_g_pri))
seRC6[2,4]<-sqrt(grad_g_prd%*% covRC6part %*% t(grad_g_prd))
seRC6[3,4]<-sqrt(grad_g_pan%*% covRC6part %*% t(grad_g_pan))


stargazer(coeftest(robustness6PRI.1, covRC6pri), coeftest(robustness6PRD.1, covRC6prd), coeftest(robustness6PAN.1, covRC6pan), coeftest(robustness6PART.1, covRC6part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

RC6<-matrix(NA,6,4)

for(j in 1:4){
  RC6[1,j]<-specify_decimal(slopesRC6[1,j], 3)
  RC6[2,j]<-paste("(",specify_decimal(seRC6[1,j],3),")", sep="")
  RC6[3,j]<-specify_decimal(slopesRC6[2,j],3)
  RC6[4,j]<-paste("(",specify_decimal(seRC6[2,j],3),")", sep="")
  RC6[5,j]<-specify_decimal(slopesRC6[3,j],3)
  RC6[6,j]<-paste("(",specify_decimal(seRC6[3,j],3),")", sep="")
}

xtable(RC6)


######Full identification of proximity

slopesBM1<-matrix(NA,3,4)
seBM1<-matrix(NA,3,4)


benchmarkPRI.2 <- lm( EPNa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         proximity*PRI09a+proximity*PRD09a+proximity*PAN09a+proximity*PART09+
                         proximity*lnpop+proximity*density+proximity*P18+proximity*P65+
                         proximity*EDUCATION+proximity*POSTEDUC+proximity*ILLITERACY+proximity*PROM_HNV+
                         proximity*PEAprop+proximity*PEAfemale+proximity*NOINSURANCE+proximity*FEMALEJEFA+
                         proximity*PERROOM+proximity*DIRTFLOOR+proximity*SERVICES+proximity*NO_SERVICES+
                         proximity*CAR+proximity*CELULAR+proximity*INTERNET+proximity*area+
                         id_mun, data=data, x=TRUE, y=TRUE)

covBMpri.2 <- cluster.vcov(benchmarkPRI.2, data$id_dist)

coeftest(benchmarkPRI.2, covBMpri.2)

g_pri <- function(b){
  return( b[2] + b[136] + b[137] + b[143] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[167])
}

g_prd <- function(b){
  return( b[2] + b[137] + b[139] + b[144] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[168] )
}

g_pan <- function(b){
  return( b[2] + b[137] + b[141] + b[145] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[169] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPRI.2$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPRI.2$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPRI.2$coef)



slopesBM1[1,1] <- benchmarkPRI.2$coefficients[c("proximity")] + benchmarkPRI.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.2$coefficients[c("proximity:PRIstr")]+  benchmarkPRI.2$coefficients[c("proximity:PRIstr:highTURNOUT")] +  benchmarkPRI.2$coefficients[c("proximity:lnpop")]+  benchmarkPRI.2$coefficients[c("proximity:density")]+  benchmarkPRI.2$coefficients[c("proximity:P18")]+  benchmarkPRI.2$coefficients[c("proximity:P65")]+  benchmarkPRI.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPRI.2$coefficients[c("proximity:POSTEDUC")]+benchmarkPRI.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPRI.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPRI.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPRI.2$coefficients[c("proximity:PERROOM")]+  benchmarkPRI.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPRI.2$coefficients[c("proximity:SERVICES")]+  benchmarkPRI.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPRI.2$coefficients[c("proximity:CAR")]+  benchmarkPRI.2$coefficients[c("proximity:CELULAR")]+  benchmarkPRI.2$coefficients[c("proximity:INTERNET")]+  benchmarkPRI.2$coefficients[c("proximity:area")]
slopesBM1[2,1] <- benchmarkPRI.2$coefficients[c("proximity")] + benchmarkPRI.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.2$coefficients[c("proximity:PRDstr")]+  benchmarkPRI.2$coefficients[c("proximity:highTURNOUT:PRDstr")] +  benchmarkPRI.2$coefficients[c("proximity:lnpop")]+  benchmarkPRI.2$coefficients[c("proximity:density")]+  benchmarkPRI.2$coefficients[c("proximity:P18")]+  benchmarkPRI.2$coefficients[c("proximity:P65")]+  benchmarkPRI.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPRI.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPRI.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPRI.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPRI.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPRI.2$coefficients[c("proximity:PERROOM")]+  benchmarkPRI.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPRI.2$coefficients[c("proximity:SERVICES")]+  benchmarkPRI.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPRI.2$coefficients[c("proximity:CAR")]+  benchmarkPRI.2$coefficients[c("proximity:CELULAR")]+  benchmarkPRI.2$coefficients[c("proximity:INTERNET")]+  benchmarkPRI.2$coefficients[c("proximity:area")]
slopesBM1[3,1] <- benchmarkPRI.2$coefficients[c("proximity")] + benchmarkPRI.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.2$coefficients[c("proximity:PANstr")]+  benchmarkPRI.2$coefficients[c("proximity:highTURNOUT:PANstr")] +  benchmarkPRI.2$coefficients[c("proximity:lnpop")]+  benchmarkPRI.2$coefficients[c("proximity:density")]+  benchmarkPRI.2$coefficients[c("proximity:P18")]+  benchmarkPRI.2$coefficients[c("proximity:P65")]+  benchmarkPRI.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPRI.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPRI.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPRI.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPRI.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPRI.2$coefficients[c("proximity:PERROOM")]+  benchmarkPRI.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPRI.2$coefficients[c("proximity:SERVICES")]+  benchmarkPRI.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPRI.2$coefficients[c("proximity:CAR")]+  benchmarkPRI.2$coefficients[c("proximity:CELULAR")]+  benchmarkPRI.2$coefficients[c("proximity:INTERNET")]+  benchmarkPRI.2$coefficients[c("proximity:area")]

seBM1[1,1]<-sqrt(grad_g_pri%*% covBMpri.2 %*% t(grad_g_pri))

seBM1[2,1]<-sqrt(grad_g_prd%*% covBMpri.2 %*% t(grad_g_prd))

seBM1[3,1]<-sqrt(grad_g_pan%*% covBMpri.2 %*% t(grad_g_pan))


benchmarkPRD.2 <- lm( AMLOa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         proximity*PRI09a+proximity*PRD09a+proximity*PAN09a+proximity*PART09+
                         proximity*lnpop+proximity*density+proximity*P18+proximity*P65+
                         proximity*EDUCATION+proximity*POSTEDUC+proximity*ILLITERACY+proximity*PROM_HNV+
                         proximity*PEAprop+proximity*PEAfemale+proximity*NOINSURANCE+proximity*FEMALEJEFA+
                         proximity*PERROOM+proximity*DIRTFLOOR+proximity*SERVICES+proximity*NO_SERVICES+
                         proximity*CAR+proximity*CELULAR+proximity*INTERNET+proximity*area+
                         id_mun,data=data, x=TRUE, y=TRUE)

covBMprd.2 <- cluster.vcov(benchmarkPRD.2, data$id_dist)

coeftest(benchmarkPRD.2, covBMprd.2)

g_pri <- function(b){
  return( b[2] + b[136] + b[137] + b[143] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[167])
}

g_prd <- function(b){
  return( b[2] + b[137] + b[139] + b[144] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[168] )
}

g_pan <- function(b){
  return( b[2] + b[137] + b[141] + b[145] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[169] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPRD.2$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPRD.2$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPRD.2$coef)



slopesBM1[1,2] <- benchmarkPRD.2$coefficients[c("proximity")] + benchmarkPRD.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.2$coefficients[c("proximity:PRIstr")]+  benchmarkPRD.2$coefficients[c("proximity:PRIstr:highTURNOUT")] +  benchmarkPRD.2$coefficients[c("proximity:lnpop")]+  benchmarkPRD.2$coefficients[c("proximity:density")]+  benchmarkPRD.2$coefficients[c("proximity:P18")]+  benchmarkPRD.2$coefficients[c("proximity:P65")]+  benchmarkPRD.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPRD.2$coefficients[c("proximity:POSTEDUC")]+benchmarkPRD.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPRD.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPRD.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPRD.2$coefficients[c("proximity:PERROOM")]+  benchmarkPRD.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPRD.2$coefficients[c("proximity:SERVICES")]+  benchmarkPRD.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPRD.2$coefficients[c("proximity:CAR")]+  benchmarkPRD.2$coefficients[c("proximity:CELULAR")]+  benchmarkPRD.2$coefficients[c("proximity:INTERNET")]+  benchmarkPRD.2$coefficients[c("proximity:area")]
slopesBM1[2,2] <- benchmarkPRD.2$coefficients[c("proximity")] + benchmarkPRD.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.2$coefficients[c("proximity:PRDstr")]+  benchmarkPRD.2$coefficients[c("proximity:highTURNOUT:PRDstr")] +  benchmarkPRD.2$coefficients[c("proximity:lnpop")]+  benchmarkPRD.2$coefficients[c("proximity:density")]+  benchmarkPRD.2$coefficients[c("proximity:P18")]+  benchmarkPRD.2$coefficients[c("proximity:P65")]+  benchmarkPRD.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPRD.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPRD.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPRD.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPRD.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPRD.2$coefficients[c("proximity:PERROOM")]+  benchmarkPRD.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPRD.2$coefficients[c("proximity:SERVICES")]+  benchmarkPRD.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPRD.2$coefficients[c("proximity:CAR")]+  benchmarkPRD.2$coefficients[c("proximity:CELULAR")]+  benchmarkPRD.2$coefficients[c("proximity:INTERNET")]+  benchmarkPRD.2$coefficients[c("proximity:area")]
slopesBM1[3,2] <- benchmarkPRD.2$coefficients[c("proximity")] + benchmarkPRD.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.2$coefficients[c("proximity:PANstr")]+  benchmarkPRD.2$coefficients[c("proximity:highTURNOUT:PANstr")] +  benchmarkPRD.2$coefficients[c("proximity:lnpop")]+  benchmarkPRD.2$coefficients[c("proximity:density")]+  benchmarkPRD.2$coefficients[c("proximity:P18")]+  benchmarkPRD.2$coefficients[c("proximity:P65")]+  benchmarkPRD.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPRD.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPRD.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPRD.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPRD.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPRD.2$coefficients[c("proximity:PERROOM")]+  benchmarkPRD.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPRD.2$coefficients[c("proximity:SERVICES")]+  benchmarkPRD.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPRD.2$coefficients[c("proximity:CAR")]+  benchmarkPRD.2$coefficients[c("proximity:CELULAR")]+  benchmarkPRD.2$coefficients[c("proximity:INTERNET")]+  benchmarkPRD.2$coefficients[c("proximity:area")]
seBM1[1,2]<-sqrt(grad_g_pri%*% covBMprd.2 %*% t(grad_g_pri))
seBM1[2,2]<-sqrt(grad_g_prd%*% covBMprd.2 %*% t(grad_g_prd))
seBM1[3,2]<-sqrt(grad_g_pan%*% covBMprd.2 %*% t(grad_g_pan))



benchmarkPAN.2 <- lm( JVMa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         proximity*PRI09a+proximity*PRD09a+proximity*PAN09a+proximity*PART09+
                         proximity*lnpop+proximity*density+proximity*P18+proximity*P65+
                         proximity*EDUCATION+proximity*POSTEDUC+proximity*ILLITERACY+proximity*PROM_HNV+
                         proximity*PEAprop+proximity*PEAfemale+proximity*NOINSURANCE+proximity*FEMALEJEFA+
                         proximity*PERROOM+proximity*DIRTFLOOR+proximity*SERVICES+proximity*NO_SERVICES+
                         proximity*CAR+proximity*CELULAR+proximity*INTERNET+proximity*area+
                         id_mun, data=data, x=TRUE, y=TRUE)

covBMpan.2 <- cluster.vcov(benchmarkPAN.2, data$id_dist)

coeftest(benchmarkPAN.2, covBMpan.2)

g_pri <- function(b){
  return( b[2] + b[136] + b[137] + b[143] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[167])
}

g_prd <- function(b){
  return( b[2] + b[137] + b[139] + b[144] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[168] )
}

g_pan <- function(b){
  return( b[2] + b[137] + b[141] + b[145] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[169] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPAN.2$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPAN.2$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPAN.2$coef)



slopesBM1[1,3] <- benchmarkPAN.2$coefficients[c("proximity")] + benchmarkPAN.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.2$coefficients[c("proximity:PRIstr")]+  benchmarkPAN.2$coefficients[c("proximity:PRIstr:highTURNOUT")] +  benchmarkPAN.2$coefficients[c("proximity:lnpop")]+  benchmarkPAN.2$coefficients[c("proximity:density")]+  benchmarkPAN.2$coefficients[c("proximity:P18")]+  benchmarkPAN.2$coefficients[c("proximity:P65")]+  benchmarkPAN.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPAN.2$coefficients[c("proximity:POSTEDUC")]+benchmarkPAN.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPAN.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPAN.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPAN.2$coefficients[c("proximity:PERROOM")]+  benchmarkPAN.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPAN.2$coefficients[c("proximity:SERVICES")]+  benchmarkPAN.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPAN.2$coefficients[c("proximity:CAR")]+  benchmarkPAN.2$coefficients[c("proximity:CELULAR")]+  benchmarkPAN.2$coefficients[c("proximity:INTERNET")]+  benchmarkPAN.2$coefficients[c("proximity:area")]
slopesBM1[2,3] <- benchmarkPAN.2$coefficients[c("proximity")] + benchmarkPAN.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.2$coefficients[c("proximity:PRDstr")]+  benchmarkPAN.2$coefficients[c("proximity:highTURNOUT:PRDstr")] +  benchmarkPAN.2$coefficients[c("proximity:lnpop")]+  benchmarkPAN.2$coefficients[c("proximity:density")]+  benchmarkPAN.2$coefficients[c("proximity:P18")]+  benchmarkPAN.2$coefficients[c("proximity:P65")]+  benchmarkPAN.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPAN.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPAN.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPAN.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPAN.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPAN.2$coefficients[c("proximity:PERROOM")]+  benchmarkPAN.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPAN.2$coefficients[c("proximity:SERVICES")]+  benchmarkPAN.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPAN.2$coefficients[c("proximity:CAR")]+  benchmarkPAN.2$coefficients[c("proximity:CELULAR")]+  benchmarkPAN.2$coefficients[c("proximity:INTERNET")]+  benchmarkPAN.2$coefficients[c("proximity:area")]
slopesBM1[3,3] <- benchmarkPAN.2$coefficients[c("proximity")] + benchmarkPAN.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.2$coefficients[c("proximity:PANstr")]+  benchmarkPAN.2$coefficients[c("proximity:highTURNOUT:PANstr")] +  benchmarkPAN.2$coefficients[c("proximity:lnpop")]+  benchmarkPAN.2$coefficients[c("proximity:density")]+  benchmarkPAN.2$coefficients[c("proximity:P18")]+  benchmarkPAN.2$coefficients[c("proximity:P65")]+  benchmarkPAN.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPAN.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPAN.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPAN.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPAN.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPAN.2$coefficients[c("proximity:PERROOM")]+  benchmarkPAN.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPAN.2$coefficients[c("proximity:SERVICES")]+  benchmarkPAN.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPAN.2$coefficients[c("proximity:CAR")]+  benchmarkPAN.2$coefficients[c("proximity:CELULAR")]+  benchmarkPAN.2$coefficients[c("proximity:INTERNET")]+  benchmarkPAN.2$coefficients[c("proximity:area")]
seBM1[1,3]<-sqrt(grad_g_pri%*% covBMpan.2 %*% t(grad_g_pri))
seBM1[2,3]<-sqrt(grad_g_prd%*% covBMpan.2 %*% t(grad_g_prd))
seBM1[3,3]<-sqrt(grad_g_pan%*% covBMpan.2 %*% t(grad_g_pan))

benchmarkPART.2 <- lm( PART ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                          proximity*PRI09a+proximity*PRD09a+proximity*PAN09a+proximity*PART09+
                          proximity*lnpop+proximity*density+proximity*P18+proximity*P65+
                          proximity*EDUCATION+proximity*POSTEDUC+proximity*ILLITERACY+proximity*PROM_HNV+
                          proximity*PEAprop+proximity*PEAfemale+proximity*NOINSURANCE+proximity*FEMALEJEFA+
                          proximity*PERROOM+proximity*DIRTFLOOR+proximity*SERVICES+proximity*NO_SERVICES+
                          proximity*CAR+proximity*CELULAR+proximity*INTERNET+proximity*area+
                          id_mun, data=data, x=TRUE, y=TRUE)

covBMpart.2 <- cluster.vcov(benchmarkPART.2, data$id_dist)

coeftest(benchmarkPART.2, covBMpart.2)

g_pri <- function(b){
  return( b[2] + b[136] + b[137] + b[143] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[167])
}

g_prd <- function(b){
  return( b[2] + b[137] + b[139] + b[144] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[168] )
}

g_pan <- function(b){
  return( b[2] + b[137] + b[141] + b[145] + b[147] + b[148] + b[149] + b[150] + b[151] + b[152] + b[153] + b[154] +
            b[155] + b[156] + b[157] + b[158] + b[159] + b[160] + b[161] + b[162] + b[163] + b[164] +
            b[165] + b[166] + b[169] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPART.2$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPART.2$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPART.2$coef)



slopesBM1[1,4] <- benchmarkPART.2$coefficients[c("proximity")] + benchmarkPART.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.2$coefficients[c("proximity:PRIstr")]+  benchmarkPART.2$coefficients[c("proximity:PRIstr:highTURNOUT")] +  benchmarkPART.2$coefficients[c("proximity:lnpop")]+  benchmarkPART.2$coefficients[c("proximity:density")]+  benchmarkPART.2$coefficients[c("proximity:P18")]+  benchmarkPART.2$coefficients[c("proximity:P65")]+  benchmarkPART.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPART.2$coefficients[c("proximity:POSTEDUC")]+benchmarkPART.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPART.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPART.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPART.2$coefficients[c("proximity:PERROOM")]+  benchmarkPART.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPART.2$coefficients[c("proximity:SERVICES")]+  benchmarkPART.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPART.2$coefficients[c("proximity:CAR")]+  benchmarkPART.2$coefficients[c("proximity:CELULAR")]+  benchmarkPART.2$coefficients[c("proximity:INTERNET")]+  benchmarkPART.2$coefficients[c("proximity:area")]
slopesBM1[2,4] <- benchmarkPART.2$coefficients[c("proximity")] + benchmarkPART.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.2$coefficients[c("proximity:PRDstr")]+  benchmarkPART.2$coefficients[c("proximity:highTURNOUT:PRDstr")] +  benchmarkPART.2$coefficients[c("proximity:lnpop")]+  benchmarkPART.2$coefficients[c("proximity:density")]+  benchmarkPART.2$coefficients[c("proximity:P18")]+  benchmarkPART.2$coefficients[c("proximity:P65")]+  benchmarkPART.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPART.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPART.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPART.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPART.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPART.2$coefficients[c("proximity:PERROOM")]+  benchmarkPART.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPART.2$coefficients[c("proximity:SERVICES")]+  benchmarkPART.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPART.2$coefficients[c("proximity:CAR")]+  benchmarkPART.2$coefficients[c("proximity:CELULAR")]+  benchmarkPART.2$coefficients[c("proximity:INTERNET")]+  benchmarkPART.2$coefficients[c("proximity:area")]
slopesBM1[3,4] <- benchmarkPART.2$coefficients[c("proximity")] + benchmarkPART.2$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.2$coefficients[c("proximity:PANstr")]+  benchmarkPART.2$coefficients[c("proximity:highTURNOUT:PANstr")] +  benchmarkPART.2$coefficients[c("proximity:lnpop")]+  benchmarkPART.2$coefficients[c("proximity:density")]+  benchmarkPART.2$coefficients[c("proximity:P18")]+  benchmarkPART.2$coefficients[c("proximity:P65")]+  benchmarkPART.2$coefficients[c("proximity:EDUCATION")]+  benchmarkPART.2$coefficients[c("proximity:POSTEDUC")]+ benchmarkPART.2$coefficients[c("proximity:PEAfemale")]+  benchmarkPART.2$coefficients[c("proximity:NOINSURANCE")]+  benchmarkPART.2$coefficients[c("proximity:FEMALEJEFA")]+  benchmarkPART.2$coefficients[c("proximity:PERROOM")]+  benchmarkPART.2$coefficients[c("proximity:DIRTFLOOR")]+  benchmarkPART.2$coefficients[c("proximity:SERVICES")]+  benchmarkPART.2$coefficients[c("proximity:NO_SERVICES")]+  benchmarkPART.2$coefficients[c("proximity:CAR")]+  benchmarkPART.2$coefficients[c("proximity:CELULAR")]+  benchmarkPART.2$coefficients[c("proximity:INTERNET")]+  benchmarkPART.2$coefficients[c("proximity:area")]
seBM1[1,4]<-sqrt(grad_g_pri%*% covBMpart.2 %*% t(grad_g_pri))
seBM1[2,4]<-sqrt(grad_g_prd%*% covBMpart.2 %*% t(grad_g_prd))
seBM1[3,4]<-sqrt(grad_g_pan%*% covBMpart.2 %*% t(grad_g_pan))


stargazer(coeftest(benchmarkPRI.2, covBMpri.2), coeftest(benchmarkPRD.2, covBMprd.2), coeftest(benchmarkPAN.2, covBMpan.2), coeftest(benchmarkPART.2, covBMpart.2), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

BM1<-matrix(NA,6,4)

for(j in 1:4){
  BM1[1,j]<-specify_decimal(slopesBM1[1,j], 3)
  BM1[2,j]<-paste("(",specify_decimal(seBM1[1,j],3),")", sep="")
  BM1[3,j]<-specify_decimal(slopesBM1[2,j],3)
  BM1[4,j]<-paste("(",specify_decimal(seBM1[2,j],3),")", sep="")
  BM1[5,j]<-specify_decimal(slopesBM1[3,j],3)
  BM1[6,j]<-paste("(",specify_decimal(seBM1[3,j],3),")", sep="")
}

xtable(BM1)


######SUR

pri <- EPN ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
  PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
  lnpop+area+P18+P65+
  EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
  PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
  PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
  CAR+CELULAR+INTERNET+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS

prd <- AMLO ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
  PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
  lnpop+area+P18+P65+
  EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
  PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
  PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
  CAR+CELULAR+INTERNET+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS

pan <- JVM ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
  PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
  lnpop+area+P18+P65+
  EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
  PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
  PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
  CAR+CELULAR+INTERNET+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS

part <- PART ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
  PRI09a+PRD09a+PAN09a+PART09+PRI06a+PRD06a+PAN06a+PART06+
  lnpop+area+P18+P65+
  EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
  PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
  PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
  CAR+CELULAR+INTERNET+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS

data0<-data[is.na(data$PART)==FALSE,]

sur<-systemfit(list(pri=pri, prd=prd, pan=pan),data=data0, "SUR")
summary(sur)

xtable(sur)

####### Proximity recoded as log(distance)
proximity2<-log(data$distance)

slopesLOG<-matrix(NA,3,4)
seLOG<-matrix(NA,3,4)


logPRI.1 <- lm( EPNa ~ proximity2*PRIstr*highTURNOUT+proximity2*PRDstr*highTURNOUT+proximity2*PANstr*highTURNOUT+     
                        PRI09a+PRD09a+PAN09a+PART09+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covLOGpri <- cluster.vcov(logPRI.1, data$id_dist)

coeftest(logPRI.1, covLOGpri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, logPRI.1$coef)
grad_g_prd <-  jacobian(g_prd, logPRI.1$coef)
grad_g_pan <-  jacobian(g_pan, logPRI.1$coef)




slopesLOG[1,1] <- logPRI.1$coefficients[c("proximity2")] + logPRI.1$coefficients[c("proximity2:highTURNOUT")] + logPRI.1$coefficients[c("proximity2:PRIstr")]+  logPRI.1$coefficients[c("proximity2:PRIstr:highTURNOUT")]

slopesLOG[2,1] <- logPRI.1$coefficients[c("proximity2")] + logPRI.1$coefficients[c("proximity2:highTURNOUT")] + logPRI.1$coefficients[c("proximity2:PRDstr")]+  logPRI.1$coefficients[c("proximity2:highTURNOUT:PRDstr")]

slopesLOG[3,1] <- logPRI.1$coefficients[c("proximity2")] + logPRI.1$coefficients[c("proximity2:highTURNOUT")] + logPRI.1$coefficients[c("proximity2:PANstr")]+  logPRI.1$coefficients[c("proximity2:highTURNOUT:PANstr")]

seLOG[1,1]<-sqrt(grad_g_pri%*% covLOGpri %*% t(grad_g_pri))

seLOG[2,1]<-sqrt(grad_g_prd%*% covLOGpri %*% t(grad_g_prd))

seLOG[3,1]<-sqrt(grad_g_pan%*% covLOGpri %*% t(grad_g_pan))


logPRD.1 <- lm( AMLOa ~ proximity2*PRIstr*highTURNOUT+proximity2*PRDstr*highTURNOUT+proximity2*PANstr*highTURNOUT+     
                        PRI09a+PRD09a+PAN09a+PART09+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covLOGprd <- cluster.vcov(logPRD.1, data$id_dist)

coeftest(logPRD.1, covLOGprd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, logPRD.1$coef)
grad_g_prd <-  jacobian(g_prd, logPRD.1$coef)
grad_g_pan <-  jacobian(g_pan, logPRD.1$coef)


slopesLOG[1,2] <- logPRD.1$coefficients[c("proximity2")] + logPRD.1$coefficients[c("proximity2:highTURNOUT")] + logPRD.1$coefficients[c("proximity2:PRIstr")]+  logPRD.1$coefficients[c("proximity2:PRIstr:highTURNOUT")]
slopesLOG[2,2] <- logPRD.1$coefficients[c("proximity2")] + logPRD.1$coefficients[c("proximity2:highTURNOUT")] + logPRD.1$coefficients[c("proximity2:PRDstr")]+  logPRD.1$coefficients[c("proximity2:highTURNOUT:PRDstr")]
slopesLOG[3,2] <- logPRD.1$coefficients[c("proximity2")] + logPRD.1$coefficients[c("proximity2:highTURNOUT")] + logPRD.1$coefficients[c("proximity2:PANstr")]+  logPRD.1$coefficients[c("proximity2:highTURNOUT:PANstr")]
seLOG[1,2]<-sqrt(grad_g_pri%*% covLOGprd %*% t(grad_g_pri))
seLOG[2,2]<-sqrt(grad_g_prd%*% covLOGprd %*% t(grad_g_prd))
seLOG[3,2]<-sqrt(grad_g_pan%*% covLOGprd %*% t(grad_g_pan))


logPAN.1 <- lm( JVMa ~ proximity2*PRIstr*highTURNOUT+proximity2*PRDstr*highTURNOUT+proximity2*PANstr*highTURNOUT+     
                        PRI09a+PRD09a+PAN09a+PART09+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covLOGpan <- cluster.vcov(logPAN.1, data$id_dist)

coeftest(logPAN.1, covLOGpan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, logPAN.1$coef)
grad_g_prd <-  jacobian(g_prd, logPAN.1$coef)
grad_g_pan <-  jacobian(g_pan, logPAN.1$coef)


slopesLOG[1,3] <- logPAN.1$coefficients[c("proximity2")] + logPAN.1$coefficients[c("proximity2:highTURNOUT")] + logPAN.1$coefficients[c("proximity2:PRIstr")]+  logPAN.1$coefficients[c("proximity2:PRIstr:highTURNOUT")]
slopesLOG[2,3] <- logPAN.1$coefficients[c("proximity2")] + logPAN.1$coefficients[c("proximity2:highTURNOUT")] + logPAN.1$coefficients[c("proximity2:PRDstr")]+  logPAN.1$coefficients[c("proximity2:highTURNOUT:PRDstr")]
slopesLOG[3,3] <- logPAN.1$coefficients[c("proximity2")] + logPAN.1$coefficients[c("proximity2:highTURNOUT")] + logPAN.1$coefficients[c("proximity2:PANstr")]+  logPAN.1$coefficients[c("proximity2:highTURNOUT:PANstr")]
seLOG[1,3]<-sqrt(grad_g_pri%*% covLOGpan %*% t(grad_g_pri))
seLOG[2,3]<-sqrt(grad_g_prd%*% covLOGpan %*% t(grad_g_prd))
seLOG[3,3]<-sqrt(grad_g_pan%*% covLOGpan %*% t(grad_g_pan))


logPART.1 <- lm( PART ~ proximity2*PRIstr*highTURNOUT+proximity2*PRDstr*highTURNOUT+proximity2*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covLOGpart <- cluster.vcov(logPART.1, data$id_dist)

coeftest(logPART.1, covLOGpart)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, logPART.1$coef)
grad_g_prd <-  jacobian(g_prd, logPART.1$coef)
grad_g_pan <-  jacobian(g_pan, logPART.1$coef)

slopesLOG[1,4] <- logPART.1$coefficients[c("proximity2")] + logPART.1$coefficients[c("proximity2:highTURNOUT")] + logPART.1$coefficients[c("proximity2:PRIstr")]+  logPART.1$coefficients[c("proximity2:PRIstr:highTURNOUT")]
slopesLOG[2,4] <- logPART.1$coefficients[c("proximity2")] + logPART.1$coefficients[c("proximity2:highTURNOUT")] + logPART.1$coefficients[c("proximity2:PRDstr")]+  logPART.1$coefficients[c("proximity2:highTURNOUT:PRDstr")]
slopesLOG[3,4] <- logPART.1$coefficients[c("proximity2")] + logPART.1$coefficients[c("proximity2:highTURNOUT")] + logPART.1$coefficients[c("proximity2:PANstr")]+  logPART.1$coefficients[c("proximity2:highTURNOUT:PANstr")]

seLOG[1,4]<-sqrt(grad_g_pri%*% covLOGpart %*% t(grad_g_pri))
seLOG[2,4]<-sqrt(grad_g_prd%*% covLOGpart %*% t(grad_g_prd))
seLOG[3,4]<-sqrt(grad_g_pan%*% covLOGpart %*% t(grad_g_pan))


stargazer(coeftest(logPRI.1, covLOGpri), coeftest(logPRD.1, covLOGprd), coeftest(logPAN.1, covLOGpan), coeftest(logPART.1, covLOGpart),style = "qje", star.cutoffs = c(0.05, 0.01, 0.001), digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

LOG<-matrix(NA,6,4)

for(j in 1:4){
  LOG[1,j]<-specify_decimal(slopesLOG[1,j], 3)
  LOG[2,j]<-paste("(",specify_decimal(seLOG[1,j], 3),")", sep="")
  LOG[3,j]<-specify_decimal(slopesLOG[2,j], 3)
  LOG[4,j]<-paste("(",specify_decimal(seLOG[2,j], 3),")", sep="")
  LOG[5,j]<-specify_decimal(slopesLOG[3,j], 3)
  LOG[6,j]<-paste("(",specify_decimal(seLOG[3,j], 3),")", sep="")
}

xtable(LOG)


#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
######################Placebo Tests##################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################


#Walmarts

slopesPL1<-matrix(NA,3,4)
sePL1<-matrix(NA,3,4)


placebo1PRI.1 <- lm( EPNa ~ proximityWM*PRIstr*highTURNOUT+proximityWM*PRDstr*highTURNOUT+proximityWM*PANstr*highTURNOUT+     
                        PRI09a+PRD09a+PAN09a+PART09+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL1pri <- cluster.vcov(placebo1PRI.1 , data$id_dist)

coeftest(placebo1PRI.1, covPL1pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo1PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo1PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo1PRI.1$coef)

slopesPL1[1,1] <- placebo1PRI.1$coefficients[c("proximityWM")] + placebo1PRI.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PRI.1$coefficients[c("proximityWM:PRIstr")]+  placebo1PRI.1$coefficients[c("proximityWM:PRIstr:highTURNOUT")]
slopesPL1[2,1] <- placebo1PRI.1$coefficients[c("proximityWM")] + placebo1PRI.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PRI.1$coefficients[c("proximityWM:PRDstr")]+  placebo1PRI.1$coefficients[c("proximityWM:highTURNOUT:PRDstr")]
slopesPL1[3,1] <- placebo1PRI.1$coefficients[c("proximityWM")] + placebo1PRI.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PRI.1$coefficients[c("proximityWM:PANstr")]+  placebo1PRI.1$coefficients[c("proximityWM:highTURNOUT:PANstr")]

sePL1[1,1]<-sqrt(grad_g_pri%*% covPL1pri %*% t(grad_g_pri))

sePL1[2,1]<-sqrt(grad_g_prd%*% covPL1pri %*% t(grad_g_prd))

sePL1[3,1]<-sqrt(grad_g_pan%*% covPL1pri %*% t(grad_g_pan))

placebo1PRD.1 <- lm( AMLOa ~ proximityWM*PRIstr*highTURNOUT+proximityWM*PRDstr*highTURNOUT+proximityWM*PANstr*highTURNOUT+     
                        PRI09a+PRD09a+PAN09a+PART09+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL1prd <- cluster.vcov(placebo1PRD.1 , data$id_dist)

coeftest(placebo1PRD.1, covPL1prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo1PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo1PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo1PRD.1$coef)

slopesPL1[1,2] <- placebo1PRD.1$coefficients[c("proximityWM")] + placebo1PRD.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PRD.1$coefficients[c("proximityWM:PRIstr")]+  placebo1PRD.1$coefficients[c("proximityWM:PRIstr:highTURNOUT")]
slopesPL1[2,2] <- placebo1PRD.1$coefficients[c("proximityWM")] + placebo1PRD.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PRD.1$coefficients[c("proximityWM:PRDstr")]+  placebo1PRD.1$coefficients[c("proximityWM:highTURNOUT:PRDstr")]
slopesPL1[3,2] <- placebo1PRD.1$coefficients[c("proximityWM")] + placebo1PRD.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PRD.1$coefficients[c("proximityWM:PANstr")]+  placebo1PRD.1$coefficients[c("proximityWM:highTURNOUT:PANstr")]

sePL1[1,2]<-sqrt(grad_g_pri%*% covPL1prd %*% t(grad_g_pri))

sePL1[2,2]<-sqrt(grad_g_prd%*% covPL1prd %*% t(grad_g_prd))

sePL1[3,2]<-sqrt(grad_g_pan%*% covPL1prd %*% t(grad_g_pan))

placebo1PAN.1 <- lm( JVMa ~ proximityWM*PRIstr*highTURNOUT+proximityWM*PRDstr*highTURNOUT+proximityWM*PANstr*highTURNOUT+     
                        PRI09a+PRD09a+PAN09a+PART09+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL1pan <- cluster.vcov(placebo1PAN.1 , data$id_dist)

coeftest(placebo1PAN.1, covPL1pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo1PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo1PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo1PAN.1$coef)

slopesPL1[1,3] <- placebo1PAN.1$coefficients[c("proximityWM")] + placebo1PAN.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PAN.1$coefficients[c("proximityWM:PRIstr")]+  placebo1PAN.1$coefficients[c("proximityWM:PRIstr:highTURNOUT")]
slopesPL1[2,3] <- placebo1PAN.1$coefficients[c("proximityWM")] + placebo1PAN.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PAN.1$coefficients[c("proximityWM:PRDstr")]+  placebo1PAN.1$coefficients[c("proximityWM:highTURNOUT:PRDstr")]
slopesPL1[3,3] <- placebo1PAN.1$coefficients[c("proximityWM")] + placebo1PAN.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PAN.1$coefficients[c("proximityWM:PANstr")]+  placebo1PAN.1$coefficients[c("proximityWM:highTURNOUT:PANstr")]

sePL1[1,3]<-sqrt(grad_g_pri%*% covPL1pan %*% t(grad_g_pri))

sePL1[2,3]<-sqrt(grad_g_prd%*% covPL1pan %*% t(grad_g_prd))

sePL1[3,3]<-sqrt(grad_g_pan%*% covPL1pan %*% t(grad_g_pan))

placebo1PART.1 <- lm( PART ~ proximityWM*PRIstr*highTURNOUT+proximityWM*PRDstr*highTURNOUT+proximityWM*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covPL1part <- cluster.vcov(placebo1PART.1 , data$id_dist)

coeftest(placebo1PART.1, covPL1part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo1PART.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo1PART.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo1PART.1$coef)

slopesPL1[1,4] <- placebo1PART.1$coefficients[c("proximityWM")] + placebo1PART.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PART.1$coefficients[c("proximityWM:PRIstr")]+  placebo1PART.1$coefficients[c("proximityWM:PRIstr:highTURNOUT")]
slopesPL1[2,4] <- placebo1PART.1$coefficients[c("proximityWM")] + placebo1PART.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PART.1$coefficients[c("proximityWM:PRDstr")]+  placebo1PART.1$coefficients[c("proximityWM:highTURNOUT:PRDstr")]
slopesPL1[3,4] <- placebo1PART.1$coefficients[c("proximityWM")] + placebo1PART.1$coefficients[c("proximityWM:highTURNOUT")] + placebo1PART.1$coefficients[c("proximityWM:PANstr")]+  placebo1PART.1$coefficients[c("proximityWM:highTURNOUT:PANstr")]

sePL1[1,4]<-sqrt(grad_g_pri%*% covPL1part %*% t(grad_g_pri))

sePL1[2,4]<-sqrt(grad_g_prd%*% covPL1part %*% t(grad_g_prd))

sePL1[3,4]<-sqrt(grad_g_pan%*% covPL1part %*% t(grad_g_pan))


stargazer(coeftest(placebo1PRD.1, covPL1prd), coeftest(placebo1PRI.1, covPL1pri), coeftest(placebo1PAN.1, covPL1pan), coeftest(placebo1PART.1, covPL1part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                                                                                                                                        "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                                                                                                                                        "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                                                                                                                                        "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                                                                                                                                        "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                                                                                                                                        "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                                                                                                                                        "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                                                                                                                                        "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                                                                                                                                        "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                                                                                                                                        "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                                                                                                                                        "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                                                                                                                                        "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

PL1<-matrix(NA,6,4)

for(j in 1:4){
  PL1[1,j]<-specify_decimal(slopesPL1[1,j], 3)
  PL1[2,j]<-paste("(",specify_decimal(sePL1[1,j],3),")", sep="")
  PL1[3,j]<-specify_decimal(slopesPL1[2,j],3)
  PL1[4,j]<-paste("(",specify_decimal(sePL1[2,j],3),")", sep="")
  PL1[5,j]<-specify_decimal(slopesPL1[3,j],3)
  PL1[6,j]<-paste("(",specify_decimal(sePL1[3,j],3),")", sep="")
}

xtable(PL1)


#2006 results

slopesPL2<-matrix(NA,3,4)
sePL2<-matrix(NA,3,4)


placebo2PRI.1 <- lm( PRI06a ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                        PRI03a+PRD03a+PAN03a+PART03+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL2pri <- cluster.vcov(placebo2PRI.1 , data$id_dist)

coeftest(placebo2PRI.1, covPL2pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo2PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo2PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo2PRI.1$coef)

slopesPL2[1,1] <- placebo2PRI.1$coefficients[c("proximity")] + placebo2PRI.1$coefficients[c("proximity:highTURNOUT")] + placebo2PRI.1$coefficients[c("proximity:PRIstr")]+  placebo2PRI.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL2[2,1] <- placebo2PRI.1$coefficients[c("proximity")] + placebo2PRI.1$coefficients[c("proximity:highTURNOUT")] + placebo2PRI.1$coefficients[c("proximity:PRDstr")]+  placebo2PRI.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL2[3,1] <- placebo2PRI.1$coefficients[c("proximity")] + placebo2PRI.1$coefficients[c("proximity:highTURNOUT")] + placebo2PRI.1$coefficients[c("proximity:PANstr")]+  placebo2PRI.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL2[1,1]<-sqrt(grad_g_pri%*% covPL2pri %*% t(grad_g_pri))
sePL2[2,1]<-sqrt(grad_g_prd%*% covPL2pri %*% t(grad_g_prd))
sePL2[3,1]<-sqrt(grad_g_pan%*% covPL2pri %*% t(grad_g_pan))


placebo2PRD.1 <- lm( PRD06a ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                        PRI03a+PRD03a+PAN03a+PART03+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL2prd <- cluster.vcov(placebo2PRD.1 , data$id_dist)

coeftest(placebo2PRD.1, covPL2prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo2PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo2PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo2PRD.1$coef)

slopesPL2[1,2] <- placebo2PRD.1$coefficients[c("proximity")] + placebo2PRD.1$coefficients[c("proximity:highTURNOUT")] + placebo2PRD.1$coefficients[c("proximity:PRIstr")]+  placebo2PRD.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL2[2,2] <- placebo2PRD.1$coefficients[c("proximity")] + placebo2PRD.1$coefficients[c("proximity:highTURNOUT")] + placebo2PRD.1$coefficients[c("proximity:PRDstr")]+  placebo2PRD.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL2[3,2] <- placebo2PRD.1$coefficients[c("proximity")] + placebo2PRD.1$coefficients[c("proximity:highTURNOUT")] + placebo2PRD.1$coefficients[c("proximity:PANstr")]+  placebo2PRD.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL2[1,2]<-sqrt(grad_g_pri%*% covPL2prd %*% t(grad_g_pri))
sePL2[2,2]<-sqrt(grad_g_prd%*% covPL2prd %*% t(grad_g_prd))
sePL2[3,2]<-sqrt(grad_g_pan%*% covPL2prd %*% t(grad_g_pan))

placebo2PAN.1 <- lm( PAN06a ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                        PRI03a+PRD03a+PAN03a+PART03+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL2pan <- cluster.vcov(placebo2PAN.1 , data$id_dist)

coeftest(placebo2PAN.1, covPL2pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo2PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo2PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo2PAN.1$coef)

slopesPL2[1,3] <- placebo2PAN.1$coefficients[c("proximity")] + placebo2PAN.1$coefficients[c("proximity:highTURNOUT")] + placebo2PAN.1$coefficients[c("proximity:PRIstr")]+  placebo2PAN.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL2[2,3] <- placebo2PAN.1$coefficients[c("proximity")] + placebo2PAN.1$coefficients[c("proximity:highTURNOUT")] + placebo2PAN.1$coefficients[c("proximity:PRDstr")]+  placebo2PAN.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL2[3,3] <- placebo2PAN.1$coefficients[c("proximity")] + placebo2PAN.1$coefficients[c("proximity:highTURNOUT")] + placebo2PAN.1$coefficients[c("proximity:PANstr")]+  placebo2PAN.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL2[1,3]<-sqrt(grad_g_pri%*% covPL2pan %*% t(grad_g_pri))
sePL2[2,3]<-sqrt(grad_g_prd%*% covPL2pan %*% t(grad_g_prd))
sePL2[3,3]<-sqrt(grad_g_pan%*% covPL2pan %*% t(grad_g_pan))

placebo2PART.1 <- lm( PART06 ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI03a+PRD03a+PAN03a+PART03+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covPL2part <- cluster.vcov(placebo2PART.1 , data$id_dist)

coeftest(placebo2PART.1, covPL2part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo2PART.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo2PART.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo2PART.1$coef)

slopesPL2[1,4] <- placebo2PART.1$coefficients[c("proximity")] + placebo2PART.1$coefficients[c("proximity:highTURNOUT")] + placebo2PART.1$coefficients[c("proximity:PRIstr")]+  placebo2PART.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL2[2,4] <- placebo2PART.1$coefficients[c("proximity")] + placebo2PART.1$coefficients[c("proximity:highTURNOUT")] + placebo2PART.1$coefficients[c("proximity:PRDstr")]+  placebo2PART.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL2[3,4] <- placebo2PART.1$coefficients[c("proximity")] + placebo2PART.1$coefficients[c("proximity:highTURNOUT")] + placebo2PART.1$coefficients[c("proximity:PANstr")]+  placebo2PART.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL2[1,4]<-sqrt(grad_g_pri%*% covPL2part %*% t(grad_g_pri))
sePL2[2,4]<-sqrt(grad_g_prd%*% covPL2part %*% t(grad_g_prd))
sePL2[3,4]<-sqrt(grad_g_pan%*% covPL2part %*% t(grad_g_pan))

stargazer(coeftest(placebo2PRI.1, covPL2pri), coeftest(placebo2PRD.1, covPL2prd), coeftest(placebo2PAN.1, covPL2pan), coeftest(placebo2PART.1, covPL2part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

PL2<-matrix(NA,6,4)

for(j in 1:4){
  PL2[1,j]<-specify_decimal(slopesPL2[1,j], 3)
  PL2[2,j]<-paste("(",specify_decimal(sePL2[1,j],3),")", sep="")
  PL2[3,j]<-specify_decimal(slopesPL2[2,j],3)
  PL2[4,j]<-paste("(",specify_decimal(sePL2[2,j],3),")", sep="")
  PL2[5,j]<-specify_decimal(slopesPL2[3,j],3)
  PL2[6,j]<-paste("(",specify_decimal(sePL2[3,j],3),")", sep="")
}

xtable(PL2)



#2009 results

slopesPL3<-matrix(NA,3,4)
sePL3<-matrix(NA,3,4)


placebo3PRI.1 <- lm( PRI09a ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                        PRI06a+PRD06a+PAN06a+PART06+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL3pri <- cluster.vcov(placebo3PRI.1 , data$id_dist)

coeftest(placebo3PRI.1, covPL3pri)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo3PRI.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo3PRI.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo3PRI.1$coef)

slopesPL3[1,1] <- placebo3PRI.1$coefficients[c("proximity")] + placebo3PRI.1$coefficients[c("proximity:highTURNOUT")] + placebo3PRI.1$coefficients[c("proximity:PRIstr")]+  placebo3PRI.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL3[2,1] <- placebo3PRI.1$coefficients[c("proximity")] + placebo3PRI.1$coefficients[c("proximity:highTURNOUT")] + placebo3PRI.1$coefficients[c("proximity:PRDstr")]+  placebo3PRI.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL3[3,1] <- placebo3PRI.1$coefficients[c("proximity")] + placebo3PRI.1$coefficients[c("proximity:highTURNOUT")] + placebo3PRI.1$coefficients[c("proximity:PANstr")]+  placebo3PRI.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL3[1,1]<-sqrt(grad_g_pri%*% covPL3pri %*% t(grad_g_pri))
sePL3[2,1]<-sqrt(grad_g_prd%*% covPL3pri %*% t(grad_g_prd))
sePL3[3,1]<-sqrt(grad_g_pan%*% covPL3pri %*% t(grad_g_pan))

placebo3PRD.1 <- lm( PRD09a ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                        PRI06a+PRD06a+PAN06a+PART06+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)


covPL3prd <- cluster.vcov(placebo3PRD.1 , data$id_dist)

coeftest(placebo3PRD.1, covPL3prd)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo3PRD.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo3PRD.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo3PRD.1$coef)

slopesPL3[1,2] <- placebo3PRD.1$coefficients[c("proximity")] + placebo3PRD.1$coefficients[c("proximity:highTURNOUT")] + placebo3PRD.1$coefficients[c("proximity:PRIstr")]+  placebo3PRD.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL3[2,2] <- placebo3PRD.1$coefficients[c("proximity")] + placebo3PRD.1$coefficients[c("proximity:highTURNOUT")] + placebo3PRD.1$coefficients[c("proximity:PRDstr")]+  placebo3PRD.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL3[3,2] <- placebo3PRD.1$coefficients[c("proximity")] + placebo3PRD.1$coefficients[c("proximity:highTURNOUT")] + placebo3PRD.1$coefficients[c("proximity:PANstr")]+  placebo3PRD.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL3[1,2]<-sqrt(grad_g_pri%*% covPL3prd %*% t(grad_g_pri))
sePL3[2,2]<-sqrt(grad_g_prd%*% covPL3prd %*% t(grad_g_prd))
sePL3[3,2]<-sqrt(grad_g_pan%*% covPL3prd %*% t(grad_g_pan))

placebo3PAN.1 <- lm( PAN09a ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                        PRI06a+PRD06a+PAN06a+PART06+
                        lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                        EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                        PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                        PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                        CAR+CELULAR+INTERNET+
                        id_mun, data=data, x=TRUE, y=TRUE)

covPL3pan <- cluster.vcov(placebo3PAN.1 , data$id_dist)

coeftest(placebo3PAN.1, covPL3pan)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo3PAN.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo3PAN.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo3PAN.1$coef)

slopesPL3[1,3] <- placebo3PAN.1$coefficients[c("proximity")] + placebo3PAN.1$coefficients[c("proximity:highTURNOUT")] + placebo3PAN.1$coefficients[c("proximity:PRIstr")]+  placebo3PAN.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL3[2,3] <- placebo3PAN.1$coefficients[c("proximity")] + placebo3PAN.1$coefficients[c("proximity:highTURNOUT")] + placebo3PAN.1$coefficients[c("proximity:PRDstr")]+  placebo3PAN.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL3[3,3] <- placebo3PAN.1$coefficients[c("proximity")] + placebo3PAN.1$coefficients[c("proximity:highTURNOUT")] + placebo3PAN.1$coefficients[c("proximity:PANstr")]+  placebo3PAN.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL3[1,3]<-sqrt(grad_g_pri%*% covPL3pan %*% t(grad_g_pri))
sePL3[2,3]<-sqrt(grad_g_prd%*% covPL3pan %*% t(grad_g_prd))
sePL3[3,3]<-sqrt(grad_g_pan%*% covPL3pan %*% t(grad_g_pan))

placebo3PART.1 <- lm( PART09 ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI06a+PRD06a+PAN06a+PART06+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covPL3part <- cluster.vcov(placebo3PART.1 , data$id_dist)

coeftest(placebo3PART.1, covPL3part)

g_pri <- function(b){
  return( b[2] + b[139] + b[140] + b[146] )
}

g_prd <- function(b){
  return( b[2] + b[140] + b[142] + b[147] )
}

g_pan <- function(b){
  return( b[2] + b[140] + b[144] + b[148] )
}

grad_g_pri <-  jacobian(g_pri, placebo3PART.1$coef)
grad_g_prd <-  jacobian(g_prd, placebo3PART.1$coef)
grad_g_pan <-  jacobian(g_pan, placebo3PART.1$coef)

slopesPL3[1,4] <- placebo3PART.1$coefficients[c("proximity")] + placebo3PART.1$coefficients[c("proximity:highTURNOUT")] + placebo3PART.1$coefficients[c("proximity:PRIstr")]+  placebo3PART.1$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL3[2,4] <- placebo3PART.1$coefficients[c("proximity")] + placebo3PART.1$coefficients[c("proximity:highTURNOUT")] + placebo3PART.1$coefficients[c("proximity:PRDstr")]+  placebo3PART.1$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL3[3,4] <- placebo3PART.1$coefficients[c("proximity")] + placebo3PART.1$coefficients[c("proximity:highTURNOUT")] + placebo3PART.1$coefficients[c("proximity:PANstr")]+  placebo3PART.1$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL3[1,4]<-sqrt(grad_g_pri%*% covPL3part %*% t(grad_g_pri))
sePL3[2,4]<-sqrt(grad_g_prd%*% covPL3part %*% t(grad_g_prd))
sePL3[3,4]<-sqrt(grad_g_pan%*% covPL3part %*% t(grad_g_pan))

stargazer(coeftest(placebo3PRI.1, covPL3pri), coeftest(placebo3PRD.1, covPL3prd), coeftest(placebo3PAN.1, covPL3pan), coeftest(placebo3PART.1, covPL3part), star.cutoffs = c(0.05, 0.01, 0.001), style = "qje", digits=3,
          covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                               "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                               "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet", "ACULCO" , "ALMOLOYA DE JUAREZ " , 
                               "ALMOLOYA DEL RIO" , "AMANALCO" , "AMECAMECA" , "APAXCO" , "ATENCO" , "ATIZAPAN" , "ATIZAPAN DE ZARAGOZA" , "AXAPUSCO" , "AYAPANGO" , "CALIMAYA" , "CAPULHUAC" , "CHALCO" , "CHAPULTEPEC" ,
                               "CHIAUTLA" , "CHICOLOAPAN" , "CHICONCUAC" , "CHIMALHUACAN" , "COACALCO DE BERRIOZABAL" , "COCOTITLAN" , "COYOTEPEC" , "CUAUTITLAN" , "CUAUTITLAN IZCALLI" , "ECATEPEC DE MORELOS" , "HUEHUETOCA",
                               "HUEYPOXTLA" , "HUIXQUILUCAN" , "ISIDRO FABELA" , "IXTAPALUCA" , "IXTLAHUACA" , "JALTENCO" , "JILOTEPEC" , "JILOTZINGO" , "JIQUIPILCO" , "JOCOTITLAN" , "JOQUICINGO" , "JUCHITEPEC" , "LA PAZ" ,
                               "LERMA" , "MELCHOR OCAMPO" , "METEPEC" , "MEXICALTZINGO" , "MORELOS" , "NAUCALPAN DE JUAREZ" , "NEXTLALPAN" , "NEZAHUALCOYOTL" , "NICOLAS ROMERO" , "OCOYOACAC" , "OCUILAN" , "OTUMBA" , 
                               "OTZOLOTEPEC" , "PAPALOTLA" , "POLOTITLAN" , "RAYON" , "SAN ANTONIO LA ISLA" , "SAN FELIPE DEL PROGRESO" , "SAN MARTIN DE LAS PIRAMIDES" , "SAN MATEO ATENCO" , "SOYANIQUILPAN DE JUAREZ" , "TECAMAC" ,
                               "TEMAMATLA" , "TEMASCALAPA" , "TEMOAYA" , "TENANGO DEL AIRE" , "TENANGO DEL VALLE" , "TEOLOYUCAN" , "TEOTIHUACAN" , "TEPETLAOXTOC" , "TEPOTZOTLAN" , "TEQUIXQUIAC" , "TEXCALYACAC" , "TEXCOCO" , "TEZOYUCA" ,
                               "TIANGUISTENCO" , "TLALMANALCO" , "TLALNEPANTLA DE BAZ" , "TOLUCA" , "TONANITLA" , "TULTEPEC" , "TULTITLAN" , "VALLE DE CHALCO SOLIDARIDAD" , "VILLA DE ALLEND" , "VILLA DEL CARBON" , "VILLA VICTORIA" , 
                               "XALATLACO" , "XONACATLAN" , "ZINACANTEPEC" , "ZUMPANGO" , "ALVARO OBREGON" , "AZCAPOTZALCO" , "BENITO JUAREZ" , "COYOACAN" , "CUAJIMALPA DE MORELOS" , "CUAUHTEMOC" , "GUSTAVO A. MADERO" , "IZTACALCO" , 
                               "IZTAPALAPA" , "MAGDALENA CONTRERAS" , "MIGUEL HIDALGO" , "MILPA ALTA" , "TLAHUAC" , "TLALPAN" , "VENUSTIANO CARRANZA" , "XOCHIMILCO"))

PL3<-matrix(NA,6,4)

for(j in 1:4){
  PL3[1,j]<-specify_decimal(slopesPL3[1,j], 3)
  PL3[2,j]<-paste("(",specify_decimal(sePL3[1,j],3),")", sep="")
  PL3[3,j]<-specify_decimal(slopesPL3[2,j],3)
  PL3[4,j]<-paste("(",specify_decimal(sePL3[2,j],3),")", sep="")
  PL3[5,j]<-specify_decimal(slopesPL3[3,j],3)
  PL3[6,j]<-paste("(",specify_decimal(sePL3[3,j],3),")", sep="")
}

xtable(PL3)

#####Outside Mexico City and the State of Mexico

data<-read.dta("/rawdata/dataworkOutsideDF.dta")

data$id_mun<-factor(data$muniID)
data$id_dist<-factor(data$id_dist)
data<-data[data$distance<=20,]


slopesPL4<-matrix(NA,3,4)
sePL4<-matrix(NA,3,4)

benchmarkPRI.1nac <- lm( EPNa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covPL4pri <- cluster.vcov(benchmarkPRI.1nac , data$id_dist)

coeftest(benchmarkPRI.1nac, covPL4pri)

g_pri <- function(b){
  return( b[2] + b[689] + b[690] + b[696] )
}

g_prd <- function(b){
  return( b[2] + b[690] + b[692] + b[697] )
}

g_pan <- function(b){
  return( b[2] + b[690] + b[694] + b[698] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPRI.1nac$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPRI.1nac$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPRI.1nac$coef)

slopesPL4[1,1] <- benchmarkPRI.1nac$coefficients[c("proximity")] + benchmarkPRI.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.1nac$coefficients[c("proximity:PRIstr")]+  benchmarkPRI.1nac$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL4[2,1] <- benchmarkPRI.1nac$coefficients[c("proximity")] + benchmarkPRI.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.1nac$coefficients[c("proximity:PRDstr")]+  benchmarkPRI.1nac$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL4[3,1] <- benchmarkPRI.1nac$coefficients[c("proximity")] + benchmarkPRI.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPRI.1nac$coefficients[c("proximity:PANstr")]+  benchmarkPRI.1nac$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL4[1,1]<-sqrt(grad_g_pri%*% covPL4pri %*% t(grad_g_pri))
sePL4[2,1]<-sqrt(grad_g_prd%*% covPL4pri %*% t(grad_g_prd))
sePL4[3,1]<-sqrt(grad_g_pan%*% covPL4pri %*% t(grad_g_pan))

benchmarkPRD.1nac <- lm( AMLOa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)

covPL4prd <- cluster.vcov(benchmarkPRD.1nac , data$id_dist)

coeftest(benchmarkPRD.1nac, covPL4prd)

g_pri <- function(b){
  return( b[2] + b[689] + b[690] + b[696] )
}

g_prd <- function(b){
  return( b[2] + b[690] + b[692] + b[697] )
}

g_pan <- function(b){
  return( b[2] + b[690] + b[694] + b[698] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPRD.1nac$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPRD.1nac$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPRD.1nac$coef)

slopesPL4[1,2] <- benchmarkPRD.1nac$coefficients[c("proximity")] + benchmarkPRD.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.1nac$coefficients[c("proximity:PRIstr")]+  benchmarkPRD.1nac$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL4[2,2] <- benchmarkPRD.1nac$coefficients[c("proximity")] + benchmarkPRD.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.1nac$coefficients[c("proximity:PRDstr")]+  benchmarkPRD.1nac$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL4[3,2] <- benchmarkPRD.1nac$coefficients[c("proximity")] + benchmarkPRD.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPRD.1nac$coefficients[c("proximity:PANstr")]+  benchmarkPRD.1nac$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL4[1,2]<-sqrt(grad_g_pri%*% covPL4prd %*% t(grad_g_pri))
sePL4[2,2]<-sqrt(grad_g_prd%*% covPL4prd %*% t(grad_g_prd))
sePL4[3,2]<-sqrt(grad_g_pan%*% covPL4prd %*% t(grad_g_pan))

benchmarkPAN.1nac <- lm( JVMa ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                         PRI09a+PRD09a+PAN09a+PART09+
                         lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                         EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                         PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                         PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                         CAR+CELULAR+INTERNET+
                         id_mun, data=data, x=TRUE, y=TRUE)


covPL4pan <- cluster.vcov(benchmarkPAN.1nac , data$id_dist)

coeftest(benchmarkPAN.1nac, covPL4pan)

g_pri <- function(b){
  return( b[2] + b[689] + b[690] + b[696] )
}

g_prd <- function(b){
  return( b[2] + b[690] + b[692] + b[697] )
}

g_pan <- function(b){
  return( b[2] + b[690] + b[694] + b[698] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPAN.1nac$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPAN.1nac$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPAN.1nac$coef)

slopesPL4[1,3] <- benchmarkPAN.1nac$coefficients[c("proximity")] + benchmarkPAN.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.1nac$coefficients[c("proximity:PRIstr")]+  benchmarkPAN.1nac$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL4[2,3] <- benchmarkPAN.1nac$coefficients[c("proximity")] + benchmarkPAN.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.1nac$coefficients[c("proximity:PRDstr")]+  benchmarkPAN.1nac$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL4[3,3] <- benchmarkPAN.1nac$coefficients[c("proximity")] + benchmarkPAN.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPAN.1nac$coefficients[c("proximity:PANstr")]+  benchmarkPAN.1nac$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL4[1,3]<-sqrt(grad_g_pri%*% covPL4pan %*% t(grad_g_pri))
sePL4[2,3]<-sqrt(grad_g_prd%*% covPL4pan %*% t(grad_g_prd))
sePL4[3,3]<-sqrt(grad_g_pan%*% covPL4pan %*% t(grad_g_pan))

benchmarkPART.1nac <- lm( PART ~ proximity*PRIstr*highTURNOUT+proximity*PRDstr*highTURNOUT+proximity*PANstr*highTURNOUT+     
                          PRI09a+PRD09a+PAN09a+PART09+
                          lnpop+P18+P65+area+density+INDIGENOUS+CATHOLIC+NONRELIGIOUS+
                          EDUCATION+POSTEDUC+ILLITERACY+PROM_HNV+
                          PEAprop+PEAfemale+NOINSURANCE+FEMALEJEFA+
                          PERROOM+DIRTFLOOR+SERVICES+NO_SERVICES+
                          CAR+CELULAR+INTERNET+
                          id_mun, data=data, x=TRUE, y=TRUE)

covPL4part <- cluster.vcov(benchmarkPART.1nac , data$id_dist)

coeftest(benchmarkPART.1nac, covPL4part)

g_pri <- function(b){
  return( b[2] + b[689] + b[690] + b[696] )
}

g_prd <- function(b){
  return( b[2] + b[690] + b[692] + b[697] )
}

g_pan <- function(b){
  return( b[2] + b[690] + b[694] + b[698] )
}

grad_g_pri <-  jacobian(g_pri, benchmarkPART.1nac$coef)
grad_g_prd <-  jacobian(g_prd, benchmarkPART.1nac$coef)
grad_g_pan <-  jacobian(g_pan, benchmarkPART.1nac$coef)

slopesPL4[1,4] <- benchmarkPART.1nac$coefficients[c("proximity")] + benchmarkPART.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.1nac$coefficients[c("proximity:PRIstr")]+  benchmarkPART.1nac$coefficients[c("proximity:PRIstr:highTURNOUT")]
slopesPL4[2,4] <- benchmarkPART.1nac$coefficients[c("proximity")] + benchmarkPART.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.1nac$coefficients[c("proximity:PRDstr")]+  benchmarkPART.1nac$coefficients[c("proximity:highTURNOUT:PRDstr")]
slopesPL4[3,4] <- benchmarkPART.1nac$coefficients[c("proximity")] + benchmarkPART.1nac$coefficients[c("proximity:highTURNOUT")] + benchmarkPART.1nac$coefficients[c("proximity:PANstr")]+  benchmarkPART.1nac$coefficients[c("proximity:highTURNOUT:PANstr")]

sePL4[1,4]<-sqrt(grad_g_pri%*% covPL4part %*% t(grad_g_pri))
sePL4[2,4]<-sqrt(grad_g_prd%*% covPL4part %*% t(grad_g_prd))
sePL4[3,4]<-sqrt(grad_g_pan%*% covPL4part %*% t(grad_g_pan))

BMnac<-matrix(NA,6,4)

for(j in 1:4){
  BMnac[1,j]<-specify_decimal(slopesPL4[1,j], 4)
  BMnac[2,j]<-paste("(",specify_decimal(sePL4[1,j],4),")", sep="")
  BMnac[3,j]<-specify_decimal(slopesPL4[2,j],4)
  BMnac[4,j]<-paste("(",specify_decimal(sePL4[2,j],4),")", sep="")
  BMnac[5,j]<-specify_decimal(slopesPL4[3,j],4)
  BMnac[6,j]<-paste("(",specify_decimal(sePL4[3,j],4),")", sep="")
}

xtable(BMnac)

a<-stargazer(coeftest(benchmarkPRI.1nac, covPL4pri), coeftest(benchmarkPRD.1nac, covPL4prd), coeftest(benchmarkPAN.1nac, covPL4pan), coeftest(benchmarkPART.1nac, covPL4part),style = "qje", star.cutoffs = c(0.05, 0.01, 0.001), digits=3,
             covariate.labels = c("Proximity", "PRI stronghold", "High Mobilization", "PRD stronghold","PAN stronghold", "PRI 2009", "PRD2009", "PAN2009", "Turnout 2009", "Population Log", "Population over 18 ",
                                  "Population over 65", "Area", "Density", "Indigenous","Catholic","Nonreligious","Education","College degree","Illiteracy","Inhabitants per house","Population in the labor market",
                                  "Female population in the labor market", "No insurance","Female head of household","Inhabitants per room", "Dirt floor", "All services", "No services", "Car", "Mobile phone", "Internet"))


write.table(a, "National.txt")

#########################################################
############### FIGURE 3 ################################
#########################################################

results <- read.table(header=T, con <- textConnection('
Stronghold DV	Coefficient	StdError	Model	 Order
                                                      PRD "PRI"	36.173	16.187	"Model 1: Relative vote share"	1	
                                                      PRD "PRD"	-32.573	7.584	"Model 1: Relative vote share"	2	
                                                      PRD "PRI"	32.166	11.547	"Model 2: Vote share change 2009-2012"	1	
                                                      PRD "PRD"	-29.444	8.562	"Model 2: Vote share change 2009-2012"	2	
                                                      PRD "PRI"	49.939	15.148	"Model 3: Driving Proximity"	1	
                                                      PRD "PRD"	-47.396	25.959	"Model 3: Driving Proximity"	2	
                                                      PRI "PRI"	-0.020	0.071	"Model 1: Relative vote share"	1	
                                                      PRI "PRD"	0.065	0.051	"Model 1: Relative vote share"	2	
                                                      PRI "PRI"	-0.029	0.080	"Model 2: Vote share change 2009-2012"	1
                                                      PRI "PRD"	-0.002	0.046	"Model 2: Vote share change 2009-2012"	2
                                                      PRI "PRI"	-0.009	0.001	"Model 3: Driving Proximity"	1
                                                      PRI "PRD"	0.006	0.002	"Model 3: Driving Proximity"	2
                                                      PAN "PRI"	-2.298	2.553	"Model 1: Relative vote share"	1
                                                      PAN "PRD"	-3.507	1.248	"Model 1: Relative vote share"	2
                                                      PAN "PRI"	-2.163	1.630	"Model 2: Vote share change 2009-2012"	1
                                                      PAN "PRD"	-4.113	0.763	"Model 2: Vote share change 2009-2012"	2
                                                      PAN "PRI"	-2.777	4.976	"Model 3: Driving Proximity"	1
                                                      PAN "PRD"	-0.700	4.262	"Model 3: Driving Proximity"	2	
                                                      '))
close(con)

results$min<-results$Coefficient-(1.96*results$StdError)
results$max<-results$Coefficient+(1.96*results$StdError)

results$Model <- factor(results$Model, levels=results$Model[order(results$Order)])


ggplot(results, aes(y=Model, x = Coefficient, shape=Stronghold, color=Stronghold)) +
  #  scale_colour_gradient(low="white", high="black")+
  geom_point(size=5) +
  facet_grid( ~ DV)+
  geom_errorbarh(aes(xmin=min, xmax=max), height = 0) +
  geom_vline(xintercept = 0, linetype=2, color="red") +
  labs( x = "Marginal Effect of Proximity", y = "") +
  theme_bw()+
  theme(legend.text = element_text(size = 18),
        legend.title = element_text(siz=18, face="bold"),
        legend.position="bottom")+
  theme(
    axis.title.x = element_text(face="bold", size=18),
    axis.title.y = element_text(face="bold", size=18, angle=90),
    axis.text.x  = element_text( size=18),
    strip.text.x = element_text(size=18),
    axis.text.y  = element_text( size=18))+ scale_y_discrete(limit = c("Model 3: Driving Proximity",  "Model 2: Vote share change 2009-2012", "Model 1: Relative vote share" ),
                                                             labels = c("Driving\nProximity",  "Vote share\nchange\n2009-2012", "Alternative\ncoding for\nvote share" ))+
  scale_colour_manual(name="Stronghold",  
                      values = c("PAN"="steelblue3", "PRI"="red", "PRD"="goldenrod3"))


##########################
##########################

results <- read.table(header=T, con <- textConnection('
                                                      Stronghold DV	Coefficient	StdError	Model	 Order
                                                      PRD "PRI"	4.458	6.992	"Model 1: 2006 Election"	1	
                                                      PRD "PRD"	21.475	18.431	"Model 1: 2006 Election"	1	
                                                      PRD "PRI"	-11.068	9.221	"Model 2: 2009 Election"	2	
                                                      PRD "PRD"	8.086	20.695	"Model 2: 2009 Election"	2	
                                                      PRD "PRI"	0.338	0.822	"Model 3: Precincts outside Mexico City and State of Mexico"	3	
                                                      PRD "PRD"	0.257	0.8695	"Model 3: Precincts outside Mexico City and State of Mexico"	3	
                                                      PRD "PRI"	3.671	2.273	"Model 4: Proximity to WalMart"	4	
                                                      PRD "PRD"	-2.366	3.041	"Model 4: Proximity to WalMart"	4
                                                      PRI "PRI"	-0.078	0.034	"Model 1: 2006 Election"	1	
                                                      PRI "PRD"	0.141	0.063	"Model 1: 2006 Election"	1	
                                                      PRI "PRI"	-0.070	0.098	"Model 2: 2009 Election"	2	
                                                      PRI "PRD"	0.006	0.039	"Model 2: 2009 Election"	2	
                                                      PRI "PRI"	0.077	0.194	"Model 3: Precincts outside Mexico City and State of Mexico"	3	
                                                      PRI "PRD"	0.107	0.234	"Model 3: Precincts outside Mexico City and State of Mexico"	3	
                                                      PRI "PRI"	0.100	0.196	"Model 4: Proximity to WalMart"	4	
                                                      PRI "PRD"	-0.080	0.125	"Model 4: Proximity to WalMart"	4
                                                      PAN "PRI"	-1.063	1.177	"Model 1: 2006 Election"	1	
                                                      PAN "PRD"	-3.356	1.362	"Model 1: 2006 Election"	1	
                                                      PAN "PRI"	-2.269	0.959	"Model 2: 2009 Election"	2	
                                                      PAN "PRD"	3.347	0.674	"Model 2: 2009 Election"	2	
                                                      PAN "PRI"	-0.233	0.201	"Model 3: Precincts outside Mexico City and State of Mexico"	3	
                                                      PAN "PRD"	-0.209	0.253	"Model 3: Precincts outside Mexico City and State of Mexico"	3	
                                                      PAN "PRI"	-0.730	0.624	"Model 4: Proximity to WalMart"	4	
                                                      PAN "PRD"	-1.517	1.472	"Model 4: Proximity to WalMart"	4
                                                      '))
close(con)

results$min<-results$Coefficient-(1.96*results$StdError)
results$max<-results$Coefficient+(1.96*results$StdError)

ggplot(results, aes(y=Model, x = Coefficient, shape=Stronghold, color=Stronghold)) +
  #  scale_colour_gradient(low="white", high="black")+
  geom_point(size=5) +
  facet_grid( ~ DV)+
  geom_errorbarh(aes(xmin=min, xmax=max), height = 0) +
  geom_vline(xintercept = 0, linetype=2, color="red") +
  labs( x = "Marginal Effect of Proximity", y = "") +
  theme_bw()+
  theme(legend.text = element_text(size = 18),
        legend.title = element_text(siz=18, face="bold"),
        legend.position="bottom")+
  theme(
    axis.title.x = element_text(face="bold", size=18),
    axis.title.y = element_text(face="bold", size=18, angle=90),
    axis.text.x  = element_text( size=18),
    strip.text.x = element_text(size=18),
    axis.text.y  = element_text( size=18))+ scale_y_discrete(limit = c("Model 4: Proximity to WalMart", 	"Model 3: Precincts outside Mexico City and State of Mexico", "Model 2: 2009 Election", "Model 1: 2006 Election" ),
                                                             labels=c("Proximity\nto WalMart",  "Outside\nMexico City\nand State\nof Mexico", "2009\nElection", "2006\nElection" ))+
  scale_colour_manual(name="Stronghold",  
                      values = c("PAN"="steelblue3", "PRI"="red", "PRD"="goldenrod3"))

