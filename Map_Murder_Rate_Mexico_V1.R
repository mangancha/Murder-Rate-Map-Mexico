########################################################################
### Required libraries
########################################################################
library(car)  #For recode
library(maps) # For maps
library(mapdata) # More maps for maps
library(sp) # For spplot
library(maptools) # To read shapefiles to sp objects
library(colorRamps)
library("gplots")
library(raster) # For getData (download files from gadm)
library(rgdal) # To import shapefiles

########################################################################
#Import and extract Municipios and Municipal Populations
#from Census/Counts data (1990, 1995, 2000, 2005, 2010)
########################################################################
#Censo 1990
ITER_NALTXT90 <- read.delim("./State-Data/ITER_NALTXT90.txt", fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT90, entidad >0 & mun >0 & loc == 0)
Mun.Pop.90 <- data.frame(MUNICIPIO = tmp$nom_mun)
Mun.Pop.90$POBLACION <- tmp$p_total
Mun.Pop.90$ID <- as.numeric(paste(tmp$entidad, "000", tmp$mun, sep=""))
#Censo 1995
ITER_NALTXT95 <- read.delim("./State-Data/ITER_NALTXT95.txt", header=FALSE, fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT95, V1 >0 & V3 >0 & V5 == 0)
Mun.Pop.95 <- data.frame(MUNICIPIO = tmp$V4)
Mun.Pop.95$POBLACION <- tmp$V10
Mun.Pop.95$ID <- as.numeric(paste(tmp$V1, "000", tmp$V3, sep=""))
#Censo 2000
ITER_NALTXT00 <- read.delim("./State-Data/ITER_NALTXT00.txt", header=FALSE, fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT00, V1 >0 & V3 >0 & V5 == 0)
Mun.Pop.00 <- data.frame(MUNICIPIO = tmp$V4)
Mun.Pop.00$POBLACION <- tmp$V10
Mun.Pop.00$ID <- as.numeric(paste(tmp$V1, "000", tmp$V3, sep=""))
#Censo 2005
ITER_NALTXT05 <- read.delim("./State-Data/ITER_NALTXT05.txt", header=FALSE, fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT05, V1 >0 & V3 >0 & V5 == 0)
Mun.Pop.05 <- data.frame(MUNICIPIO = tmp$V4)
Mun.Pop.05$POBLACION <- tmp$V10
Mun.Pop.05$ID <- as.numeric(paste(tmp$V1, "000", tmp$V3, sep=""))
#Censo 2010
ITER_NALTXT10 <- read.delim("./State-Data/ITER_NALTXT10.TXT", fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT10, ENTIDAD >0 & MUN >0 & LOC == 0)
Mun.Pop.10 <- data.frame(MUNICIPIO = tmp$NOM_MUN)
Mun.Pop.10$POBLACION <- tmp$POBTOT
Mun.Pop.10$ID <- as.numeric(paste(tmp$ENTIDAD, "000", tmp$MUN, sep=""))
Mun.Pop.10$ENT <- tmp$ENTIDAD
Mun.Pop.10$MUN <- tmp$MUN
Mun.Pop.10$NOM_ENT <- tmp$NOM_ENT

########################################################################
#Generate data for intercensus years and Consolidate in one data frame
########################################################################
MAIN.POP <- data.frame(ID = Mun.Pop.10$ID)
MAIN.POP$MUNICIPIO <- Mun.Pop.10$MUNICIPIO
P.10 <- Mun.Pop.10$POBLACION

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.05<-Mun.Pop.05[2][Mun.Pop.05[3]==Mun.Pop.10[i,3]]
  } else {
    if(is.integer(Mun.Pop.05[2][Mun.Pop.05[3]==Mun.Pop.10[i,3]]) 
       && length(Mun.Pop.05[2][Mun.Pop.05[3]==Mun.Pop.10[i,3]]) == 0L) {
      P.05<-c(P.05, "NA")
    } else {
      P.05<-c(P.05, Mun.Pop.05[2][Mun.Pop.05[3]==Mun.Pop.10[i,3]])
    }
  }
}
P.05 <- as.numeric(P.05)

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    if(is.na(P.10[i]) | is.na(P.05[i])) {
      P.09.08.07.06 <- c("NA", "NA", "NA", "NA")
      P.12.11 <- c("NA","NA")
    } else {
      Step <- round((P.10[i]-P.05[i])/5,0)
      P.09.08.07.06 <- c(P.05[i]+Step+Step+Step+Step, P.05[i]+Step+Step+Step, P.05[i]+Step+Step, P.05[i]+Step)
      P.12.11 <- c(P.10[i]+Step+Step, P.10[i]+Step)
    }
  } else {
    if(is.na(P.10[i]) | is.na(P.05[i])) {
      P.09.08.07.06 <- c(P.09.08.07.06, "NA", "NA", "NA", "NA")
      P.12.11 <- c(P.12.11, "NA", "NA")
    } else {
      Step <- round((P.10[i]-P.05[i])/5,0)
      P.09.08.07.06 <- c(P.09.08.07.06, P.05[i]+Step+Step+Step+Step, P.05[i]+Step+Step+Step, P.05[i]+Step+Step, P.05[i]+Step)
      P.12.11 <- c(P.12.11, P.10[i]+Step+Step, P.10[i]+Step)
    }
  }
}
P.09.08.07.06 <- matrix(P.09.08.07.06,nrow = length(Mun.Pop.10$ID),ncol = 4, byrow=TRUE)
P.12.11 <- matrix(P.12.11,nrow = length(Mun.Pop.10$ID),ncol = 2, byrow=TRUE)
MAIN.POP$P.12 <- P.12.11[,1]
MAIN.POP$P.11 <- P.12.11[,2]
MAIN.POP$P.10 <- P.10
MAIN.POP$P.09 <- P.09.08.07.06[,1]
MAIN.POP$P.08 <- P.09.08.07.06[,2]
MAIN.POP$P.07 <- P.09.08.07.06[,3]
MAIN.POP$P.06 <- P.09.08.07.06[,4]
MAIN.POP$P.05 <- P.05

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.00<-Mun.Pop.00[2][Mun.Pop.00[3]==Mun.Pop.10[i,3]]
  } else {
    if(is.integer(Mun.Pop.00[2][Mun.Pop.00[3]==Mun.Pop.10[i,3]]) 
       && length(Mun.Pop.00[2][Mun.Pop.00[3]==Mun.Pop.10[i,3]]) == 0L) {
      P.00<-c(P.00, "NA")
    } else {
      P.00<-c(P.00, Mun.Pop.00[2][Mun.Pop.00[3]==Mun.Pop.10[i,3]])
    }
  }
}
P.00 <- as.numeric(P.00)

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    if(is.na(P.05[i]) | is.na(P.00[i])) {
      P.04.03.02.01 <- c("NA", "NA", "NA", "NA")
    } else {
      Step <- round((P.05[i]-P.00[i])/5,0)
      P.04.03.02.01 <- c(P.00[i]+Step+Step+Step+Step, P.00[i]+Step+Step+Step, P.00[i]+Step+Step, P.00[i]+Step)
    }
  } else {
    if(is.na(P.05[i]) | is.na(P.00[i])) {
      P.04.03.02.01 <- c(P.04.03.02.01, "NA", "NA", "NA", "NA")
    } else {
      Step <- round((P.05[i]-P.00[i])/5,0)
      P.04.03.02.01 <- c(P.04.03.02.01, P.00[i]+Step+Step+Step+Step, P.00[i]+Step+Step+Step, P.00[i]+Step+Step, P.00[i]+Step)
    }
  } 
}
P.04.03.02.01 <- matrix(P.04.03.02.01,nrow = length(Mun.Pop.10$ID),ncol = 4, byrow=TRUE)
MAIN.POP$P.04 <- P.04.03.02.01[,1]
MAIN.POP$P.03 <- P.04.03.02.01[,2]
MAIN.POP$P.02 <- P.04.03.02.01[,3]
MAIN.POP$P.01 <- P.04.03.02.01[,4]
MAIN.POP$P.00 <- P.00

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.95<-Mun.Pop.95[2][Mun.Pop.95[3]==Mun.Pop.10[i,3]]
  } else {
    if(is.integer(Mun.Pop.95[2][Mun.Pop.95[3]==Mun.Pop.10[i,3]]) 
       && length(Mun.Pop.95[2][Mun.Pop.95[3]==Mun.Pop.10[i,3]]) == 0L) {
      P.95<-c(P.95, "NA")
    } else {
      P.95<-c(P.95, Mun.Pop.95[2][Mun.Pop.95[3]==Mun.Pop.10[i,3]])
    }
  }
}
P.95 <- as.numeric(P.95)

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    if(is.na(P.00[i]) | is.na(P.95[i])) {
      P.99.98.97.96 <- c("NA", "NA", "NA", "NA")
    } else {
      Step <- round((P.00[i]-P.95[i])/5,0)
      P.99.98.97.96 <- c(P.95[i]+Step+Step+Step+Step, P.95[i]+Step+Step+Step, P.95[i]+Step+Step, P.95[i]+Step)
    }
  } else {
    if(is.na(P.00[i]) | is.na(P.95[i])) {
      P.99.98.97.96 <- c(P.99.98.97.96, "NA", "NA", "NA", "NA")
    } else {
      Step <- round((P.05[i]-P.00[i])/5,0)
      P.99.98.97.96 <- c(P.99.98.97.96, P.95[i]+Step+Step+Step+Step, P.95[i]+Step+Step+Step, P.95[i]+Step+Step, P.95[i]+Step)
    }
  } 
}
P.99.98.97.96 <- matrix(P.99.98.97.96,nrow = length(Mun.Pop.10$ID),ncol = 4, byrow=TRUE)
MAIN.POP$P.99 <- P.99.98.97.96[,1]
MAIN.POP$P.98 <- P.99.98.97.96[,2]
MAIN.POP$P.97 <- P.99.98.97.96[,3]
MAIN.POP$P.96 <- P.99.98.97.96[,4]
MAIN.POP$P.95 <- P.95

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.90<-Mun.Pop.90[2][Mun.Pop.90[3]==Mun.Pop.10[i,3]]
  } else {
    if(is.integer(Mun.Pop.90[2][Mun.Pop.90[3]==Mun.Pop.10[i,3]]) 
       && length(Mun.Pop.90[2][Mun.Pop.90[3]==Mun.Pop.10[i,3]]) == 0L) {
      P.90<-c(P.90, "NA")
    } else {
      P.90<-c(P.90, Mun.Pop.90[2][Mun.Pop.90[3]==Mun.Pop.10[i,3]])
    }
  }
}
P.90 <- as.numeric(P.90)

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    if(is.na(P.95[i]) | is.na(P.90[i])) {
      P.94.93.92.91 <- c("NA", "NA", "NA", "NA")
    } else {
      Step <- round((P.95[i]-P.90[i])/5,0)
      P.94.93.92.91 <- c(P.90[i]+Step+Step+Step+Step, P.90[i]+Step+Step+Step, P.90[i]+Step+Step, P.90[i]+Step)
    }
  } else {
    if(is.na(P.95[i]) | is.na(P.90[i])) {
      P.94.93.92.91 <- c(P.94.93.92.91, "NA", "NA", "NA", "NA")
    } else {
      Step <- round((P.95[i]-P.90[i])/5,0)
      P.94.93.92.91 <- c(P.94.93.92.91, P.90[i]+Step+Step+Step+Step, P.90[i]+Step+Step+Step, P.90[i]+Step+Step, P.90[i]+Step)
    }
  } 
}
P.94.93.92.91 <- matrix(P.94.93.92.91,nrow = length(Mun.Pop.10$ID),ncol = 4, byrow=TRUE)
MAIN.POP$P.94 <- P.94.93.92.91[,1]
MAIN.POP$P.93 <- P.94.93.92.91[,2]
MAIN.POP$P.92 <- P.94.93.92.91[,3]
MAIN.POP$P.91 <- P.94.93.92.91[,4]
MAIN.POP$P.90 <- P.90


########################################################################
#Import Homicidios
########################################################################
Murders.tmp <- read.delim("./state-data/141016-Homicidios-INEGI.txt", header=FALSE, fileEncoding="ISO-8859-1")

Murders.tmp <- t(Murders.tmp)
colnames(Murders.tmp) = Murders.tmp[1, ]
Murders.tmp = Murders.tmp[-1,]   

Murders <- data.frame(ID = as.numeric(paste(Murders.tmp[,2], "000", Murders.tmp[,3], sep="")))
Murders$NOM <- Murders.tmp[,1]
Murders$ENT <- as.numeric(Murders.tmp[,2])
Murders$MUN <- as.numeric(Murders.tmp[,3])
Murders$M.12 <- Murders.tmp[,26]
Murders$M.11 <- Murders.tmp[,25]
Murders$M.10 <- Murders.tmp[,24]
Murders$M.09 <- Murders.tmp[,23]
Murders$M.08 <- Murders.tmp[,22]
Murders$M.07 <- Murders.tmp[,21]
Murders$M.06 <- Murders.tmp[,20]
Murders$M.05 <- Murders.tmp[,19]
Murders$M.04 <- Murders.tmp[,18]
Murders$M.03 <- Murders.tmp[,17]
Murders$M.02 <- Murders.tmp[,16]
Murders$M.01 <- Murders.tmp[,15]
Murders$M.00 <- Murders.tmp[,14]
Murders$M.99 <- Murders.tmp[,13]
Murders$M.98 <- Murders.tmp[,12]
Murders$M.97 <- Murders.tmp[,11]
Murders$M.96 <- Murders.tmp[,10]
Murders$M.95 <- Murders.tmp[,9]
Murders$M.94 <- Murders.tmp[,8]
Murders$M.93 <- Murders.tmp[,7]
Murders$M.92 <- Murders.tmp[,6]
Murders$M.91 <- Murders.tmp[,5]
Murders$M.90 <- Murders.tmp[,4]

Murders <- subset(Murders, !is.na(Murders$ID))
Murders <- subset(Murders, Murders$MUN>0)
Murders <- subset(Murders, Murders$ENT<33)
#Murders <- data.frame(matrix(unlist(Murders), nrow=length(Murders$ID), byrow=F))
Murders <- as.data.frame(Murders)
for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    Murders.Fin<-Murders[i,]
  } else {
    if( identical(subset(Murders, Murders$ID == Mun.Pop.10[i,3])$ID, numeric(0))) {
      Murders.Fin<-rbind(Murders.Fin, rep("NA", length=ncol(Murders)))
    } else {
      Murders.Fin<-rbind(Murders.Fin, subset(Murders, Murders$ID == Mun.Pop.10[i,3]))
    }
  }
}
Murders.Fin[Murders.Fin==""]<-0

########################################################################
#Calculate murder rate
########################################################################

Murder.Rate <- data.frame(ID = Mun.Pop.10$ID)
Murder.Rate$MUNICIPIO <- Mun.Pop.10$MUNICIPIO
Murder.Rate$ENT <- Mun.Pop.10$ENT
Murder.Rate$MUN <- Mun.Pop.10$MUN
Murder.Rate$NOM_ENT <- Mun.Pop.10$NOM_ENT
Murder.Rate$R.12 <- round(((as.numeric(gsub(",","",Murders.Fin$M.12)))/(as.numeric(gsub(",","",MAIN.POP$P.12)))*10000),2)
Murder.Rate$R.11 <- round(((as.numeric(gsub(",","",Murders.Fin$M.11)))/(as.numeric(gsub(",","",MAIN.POP$P.11)))*10000),2)
Murder.Rate$R.10 <- round(((as.numeric(gsub(",","",Murders.Fin$M.10)))/(as.numeric(gsub(",","",MAIN.POP$P.10)))*10000),2)
Murder.Rate$R.09 <- round(((as.numeric(gsub(",","",Murders.Fin$M.09)))/(as.numeric(gsub(",","",MAIN.POP$P.09)))*10000),2)
Murder.Rate$R.08 <- round(((as.numeric(gsub(",","",Murders.Fin$M.08)))/(as.numeric(gsub(",","",MAIN.POP$P.08)))*10000),2)
Murder.Rate$R.07 <- round(((as.numeric(gsub(",","",Murders.Fin$M.07)))/(as.numeric(gsub(",","",MAIN.POP$P.07)))*10000),2)
Murder.Rate$R.06 <- round(((as.numeric(gsub(",","",Murders.Fin$M.06)))/(as.numeric(gsub(",","",MAIN.POP$P.06)))*10000),2)
Murder.Rate$R.05 <- round(((as.numeric(gsub(",","",Murders.Fin$M.05)))/(as.numeric(gsub(",","",MAIN.POP$P.05)))*10000),2)
Murder.Rate$R.04 <- round(((as.numeric(gsub(",","",Murders.Fin$M.04)))/(as.numeric(gsub(",","",MAIN.POP$P.04)))*10000),2)
Murder.Rate$R.03 <- round(((as.numeric(gsub(",","",Murders.Fin$M.03)))/(as.numeric(gsub(",","",MAIN.POP$P.03)))*10000),2)
Murder.Rate$R.02 <- round(((as.numeric(gsub(",","",Murders.Fin$M.02)))/(as.numeric(gsub(",","",MAIN.POP$P.02)))*10000),2)
Murder.Rate$R.01 <- round(((as.numeric(gsub(",","",Murders.Fin$M.01)))/(as.numeric(gsub(",","",MAIN.POP$P.01)))*10000),2)
Murder.Rate$R.00 <- round(((as.numeric(gsub(",","",Murders.Fin$M.00)))/(as.numeric(gsub(",","",MAIN.POP$P.00)))*10000),2)
Murder.Rate$R.99 <- round(((as.numeric(gsub(",","",Murders.Fin$M.99)))/(as.numeric(gsub(",","",MAIN.POP$P.99)))*10000),2)
Murder.Rate$R.98 <- round(((as.numeric(gsub(",","",Murders.Fin$M.98)))/(as.numeric(gsub(",","",MAIN.POP$P.98)))*10000),2)
Murder.Rate$R.97 <- round(((as.numeric(gsub(",","",Murders.Fin$M.97)))/(as.numeric(gsub(",","",MAIN.POP$P.97)))*10000),2)
Murder.Rate$R.96 <- round(((as.numeric(gsub(",","",Murders.Fin$M.96)))/(as.numeric(gsub(",","",MAIN.POP$P.96)))*10000),2)
Murder.Rate$R.95 <- round(((as.numeric(gsub(",","",Murders.Fin$M.95)))/(as.numeric(gsub(",","",MAIN.POP$P.95)))*10000),2)
Murder.Rate$R.94 <- round(((as.numeric(gsub(",","",Murders.Fin$M.94)))/(as.numeric(gsub(",","",MAIN.POP$P.94)))*10000),2)
Murder.Rate$R.93 <- round(((as.numeric(gsub(",","",Murders.Fin$M.93)))/(as.numeric(gsub(",","",MAIN.POP$P.93)))*10000),2)
Murder.Rate$R.92 <- round(((as.numeric(gsub(",","",Murders.Fin$M.92)))/(as.numeric(gsub(",","",MAIN.POP$P.92)))*10000),2)
Murder.Rate$R.91 <- round(((as.numeric(gsub(",","",Murders.Fin$M.91)))/(as.numeric(gsub(",","",MAIN.POP$P.91)))*10000),2)
Murder.Rate$R.90 <- round(((as.numeric(gsub(",","",Murders.Fin$M.90)))/(as.numeric(gsub(",","",MAIN.POP$P.90)))*10000),2)


  
########################################################################
#Load and prepare shapefile (GADM). Some recoding needed since some of 
#the municipalty names are different (INEGI vs GADM)
########################################################################

load("./Shapefiles/gadm/MEX_adm2.RData")

gadm <- gadm[order(gadm$ID_1, gadm$NAME_2),]

Murder.Rate$NOM_ENT<-recode(Murder.Rate$NOM_ENT, " 'Coahuila de Zaragoza'='Coahuila';
                            'Veracruz de Ignacio de la Llave'='Veracruz';
                            'Michoacán de Ocampo' = 'Michoacán'")
Murder.Rate$MUNICIPIO<-recode(Murder.Rate$MUNICIPIO, " 'Cuatro Ciénegas'='Cuatrociénegas';
                              'San Juan de Sabinas'='Nueva Rosita';
                              'Gral. Bravo'='General Bravo';
                              'Gral. Escobedo'='General Escobedo';
                              'Gral. Terán'='General Terán';
                              'Gral. Treviño'='General Treviño';
                              'Gral. Zaragoza'='General Zaragoza';
                              'Gral. Zuazua'='General Zuazua';
                              'Dr. Arroyo'='Doctor Arroyo';
                              'Dr. Coss'='Doctor Coss';
                              'Dr. González'='Doctor González';
                              'Gustavo Díaz Ordaz'='Gustavo Díaz Ordáz';
                              'Soto la Marina'='Soto La Marina';
                              'Gómez Farías'='Gómez Farias';
                              'Guachochi'='Guachochic';
                              'Carichí'='Carichic';
                              'Maguarichi'='Maguarichic';
                              'Uruachi'='Uruachic';
                              'Cusihuiriachi'='Cusihuiriachic';
                              'Santa Isabel'='General Trias';
                              'Matachí'='Matachic';
                              'Alamos'='Álamos';
                              'El Rosario'='Rosario';
                              'Del Nayar'='El Nayar';
                              'Sain Alto'='Saín Alto';
                              'Teúl de González Ortega'='Teul de González Ortega';
                              'San Luis del Cordero'='San Luis de Cordero';
                              'Guadalcázar'='Guadalcazar';
                              'Tampacán'='Tampacan';
                              'Tancanhuitz'='Tancanhuitz de Santos';
                              'Noria de Ángeles'='Noria de Angeles';
                              'Cuquío'='Cuquito';
                              'San Miguel de Allende'='Allende';
                              'Dolores Hidalgo Cuna de la Independencia Nacional'='Dolores Hidalgo';
                              'La Unión de Isidoro Montes de Oca'='La Union';
                              'Zihuatanejo de Azueta'='José Azueta';
                              'Chilapa de Álvarez'='Chilapa de Alvarez';
                              'Atoyac de Álvarez'='Atoyac de Alvarez';
                              'Indaparapeo'='Indoparapeo';
                              'Nuevo Parangaricutiro'='Nuevo Paranguricutiro';
                              'Tingüindín'='Tinguindín';
                              'Villa de Álvarez'='Villa de Alvarez';
                              'Zapotlán el Grande'='Ciudad Guzman';
                              'Gómez Farias'='Gómez Farías';  
                              'Atotonilco el Alto'='Atotonilco El Alto';
                              'Santa María de los Ángeles'='Santa María de los Angeles';
                              'San Gabriel'='Ciudad Venustiano Carranza';
                              'San Juanito de Escobedo'='Antonio Escobedo';
                              'Amealco de Bonfil'='Amealco de Bonfin';
                              'Ozuluama de Mascareñas'='Ozuluama';
                              'Álamo Temapache'='Temapache';
                              'Álvaro Obregón'='Alvaro Obregón';
                              'La Magdalena Contreras'='Magdalena Contreras';
                              'Zacualtipán de Ángeles'='Zacualtipán de Angeles';
                              'Xalatlaco'='Jalatlaco';
                              'San Antonio la Isla'='San Antonio La Isla';
                              'San Martín de las Pirámides'='San Martín de las Piráámides';
                              'Teoloyucan'='Teoloyucán';
                              'Tlalnepantla de Baz'='Tlalnepantla';
                              'Zinacantepec'='Zinacatepec';
                              'Zumpahuacán'='Zumpahuacan';
                              'Zacatepec'='Zacatepec de Hidalgo';
                              'Jalpan'='Xalpan';
                              'Cohuecan'='Cohuecán';
                              'General Felipe Ángeles'='General Felipe Angeles';
                              'Zoquitlán'='Zoquitlan';
                              'Atlequizayan'='Ignacio Allende';
                              'Ziltlaltépec de Trinidad Sánchez Santos'='Zitlaltepec de Trinidad Sánchez Santos';
                              'Atltzayanca'='Altzayanca';
                              'Yauhquemehcan'='Yauhquemecan';
                              'Zontecomatlán de López y Fuentes'='Zontecomatlán';
                              'Ursulo Galván'='Úrsulo Galván';
                              'Tlaquilpa'='Tlaquilpan';
                              'Tlacojalpan'='Tlajocalpan';
                              'Coahuitlán'='Progreso de Zaragoza';
                              'Miahuatlán'='Mihuatlán';
                              'Jalacingo'='Jalancingo';
                              'Coxquihui'='Coxquihi';
                              'Cosamaloapan de Carpio'='Cosamaloapan';
                              'Amatlán de los Reyes'='Amatitlán de los Reyes';
                              'Chocamán'='Chocomán';
                              'Camarón de Tejeda'='Camarón de Tejada';
                              'Angel R. Cabada'='Ángel R. Cabada';
                              'Naranjos Amatlán'='Amatlan Tuxpan';
                              'Dzán'='Dzan';
                              'Tinum'='Tinúm';
                              'Tixmehuac'='Tixméhuac';
                              'El Plateado de Joaquín Amaro'='General Joaquin Amaro'
                              ")

#Hay en Chihuahua y Sonora 1 municipio llamado El Rosario, Rosario en GADM. Hay en Sinaloa 1 municipio llamado Rosario, El Rosario en GADM
#Hay en Jalisco un municipio llamado Santa MAria del Oro (igual que en Nayarit), llamado Manuel M. DIeguez en gadm Jaiisco
#Hay en Guerrero un Erongaricuaro que en realidad es de Michoacan
#En Michoacan un Zirandaro que es de Guerrero
#Hay un Alvaro Obregon en Michoacan y DF
#Hay un Gomez Farias en NL y Jalisco

gadm$NAME_2<-recode(gadm$NAME_2, " 'Isla Tiburon'='Hermosillo'")
gadm$NAME_2[gadm$NAME_2=="El Rosario" & gadm$NAME_1=="Sinaloa"]<-"Rosario"
gadm$NAME_2[gadm$NAME_2=="Manuel M. Dieguez" & gadm$NAME_1=="Jalisco"]<-"Santa María del Oro"
gadm$NAME_2[gadm$NAME_2=="Gómez Farías" & gadm$NAME_1=="Jalisco"]<-"Gómez Farias"
gadm$NAME_2[gadm$NAME_2=="Álvaro Obregón" & gadm$NAME_1=="Michoacán"]<-"Alvaro Obregón"
gadm$NAME_2[gadm$NAME_2=="Erongarícuaro" & gadm$NAME_1=="Guerrero"]<-"Coahuayutla de José María Izazaga"




DATA <- cbind (c(as.character(gadm$NAME_1), rep("NA", (length(Murder.Rate$ID) - length(gadm$NAME_2)))),
               c(as.character(gadm$NAME_2), rep("NA", (length(Murder.Rate$ID) - length(gadm$NAME_2)))), 
               c(as.character(gadm$ID_1), rep("NA", (length(Murder.Rate$ID) - length(gadm$NAME_2)))), 
               as.character(Murder.Rate$NOM_ENT),
               as.character(Murder.Rate$MUNICIPIO))


#subset(DATA, gadm$NAME_1=="Oaxaca")[,1:2]
#subset(DATA, Murder.Rate$NOM_ENT=="Guerrero")[,4:5]

#mexico<-read.shapefile("/home/marino/Desktop/Maps-No-Usados/scince/mexico_municipal")
mexico<-readOGR("./Shapefiles/INEGI/mex-selec", 
                layer="mexico_municipal_SELEC")

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.12<- as.numeric(Murder.Rate$R.12[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.12<- c(R.12, "100000")
    } else {
      R.12<-c(R.12, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                                               Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.12)
    }
  }
}

MR12<-as.factor(t(as.numeric(R.12,2)))
gadm$MR12 <- MR12
MISSING<-data.frame(Municipio=subset(gadm, gadm$MR12==100000)$NAME_2)
MISSING$ENNTIDAD<-subset(gadm, gadm$MR12==100000)$NAME_1

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.11<- as.numeric(Murder.Rate$R.11[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.11<- c(R.11, "100000")
    } else {
      R.11<-c(R.11, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.11)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.10<- as.numeric(Murder.Rate$R.10[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.10<- c(R.10, "100000")
    } else {
      R.10<-c(R.10, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.10)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.09<- as.numeric(Murder.Rate$R.09[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.09<- c(R.09, "100000")
    } else {
      R.09<-c(R.09, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.09)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.08<- as.numeric(Murder.Rate$R.08[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.08<- c(R.08, "100000")
    } else {
      R.08<-c(R.08, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.08)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.07<- as.numeric(Murder.Rate$R.07[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.07<- c(R.07, "100000")
    } else {
      R.07<-c(R.07, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.07)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.06<- as.numeric(Murder.Rate$R.06[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.06<- c(R.06, "100000")
    } else {
      R.06<-c(R.06, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.06)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.05<- as.numeric(Murder.Rate$R.05[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.05<- c(R.05, "100000")
    } else {
      R.05<-c(R.05, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.05)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.04<- as.numeric(Murder.Rate$R.04[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.04<- c(R.04, "100000")
    } else {
      R.04<-c(R.04, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.04)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.03<- as.numeric(Murder.Rate$R.03[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.03<- c(R.03, "100000")
    } else {
      R.03<-c(R.03, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.03)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.02<- as.numeric(Murder.Rate$R.02[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.02<- c(R.02, "100000")
    } else {
      R.02<-c(R.02, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.02)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.01<- as.numeric(Murder.Rate$R.01[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.01<- c(R.01, "100000")
    } else {
      R.01<-c(R.01, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.01)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.00<- as.numeric(Murder.Rate$R.00[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.00<- c(R.00, "100000")
    } else {
      R.00<-c(R.00, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.00)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.99<- as.numeric(Murder.Rate$R.99[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.99<- c(R.99, "100000")
    } else {
      R.99<-c(R.99, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.99)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.98<- as.numeric(Murder.Rate$R.98[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.98<- c(R.98, "100000")
    } else {
      R.98<-c(R.98, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.98)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.97<- as.numeric(Murder.Rate$R.97[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.97<- c(R.97, "100000")
    } else {
      R.97<-c(R.97, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.97)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.96<- as.numeric(Murder.Rate$R.96[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.96<- c(R.96, "100000")
    } else {
      R.96<-c(R.96, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.96)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.95<- as.numeric(Murder.Rate$R.95[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.95<- c(R.95, "100000")
    } else {
      R.95<-c(R.95, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.95)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.94<- as.numeric(Murder.Rate$R.94[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.94<- c(R.94, "100000")
    } else {
      R.94<-c(R.94, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.94)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.93<- as.numeric(Murder.Rate$R.93[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.93<- c(R.93, "100000")
    } else {
      R.93<-c(R.93, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.93)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.92<- as.numeric(Murder.Rate$R.92[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.92<- c(R.92, "100000")
    } else {
      R.92<-c(R.92, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.92)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.91<- as.numeric(Murder.Rate$R.91[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.91<- c(R.91, "100000")
    } else {
      R.91<-c(R.91, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.91)
    }
  }
}

for(i in 1:length(gadm$ID_2)) {
  if(i==1) {
    R.90<- as.numeric(Murder.Rate$R.90[i])
  } else {
    if( identical(subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                           Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$ID, numeric(0))) {
      R.90<- c(R.90, "100000")
    } else {
      R.90<-c(R.90, subset(Murder.Rate, Murder.Rate$NOM_ENT == gadm$NAME_1[i] &
                             Murder.Rate$MUNICIPIO == gadm$NAME_2[i])$R.90)
    }
  }
}

########################################################################
#Generating Map
########################################################################

H<-as.factor(c(t(as.numeric(R.12,2)), 
               t(as.numeric(R.11,2)),
               t(as.numeric(R.10,2)),
               t(as.numeric(R.09,2)),
               t(as.numeric(R.08,2)),
               t(as.numeric(R.07,2)),
               t(as.numeric(R.06,2)),
               t(as.numeric(R.05,2)),
               t(as.numeric(R.04,2)),
               t(as.numeric(R.03,2)),
               t(as.numeric(R.02,2)),
               t(as.numeric(R.01,2)),
               t(as.numeric(R.00,2)),
               t(as.numeric(R.99,2)),
               t(as.numeric(R.98,2)),
               t(as.numeric(R.97,2)),
               t(as.numeric(R.96,2)),
               t(as.numeric(R.95,2)),
               t(as.numeric(R.94,2)),
               t(as.numeric(R.93,2)),
               t(as.numeric(R.92,2)),
               t(as.numeric(R.91,2)),
               t(as.numeric(R.90,2))
               ))

#H[which(is.na(H))] <- 1000000 
#H<-as.factor(H)
#Col = blue2red(length(levels(H)))
Col = rev(heat.colors(length(levels(H))))


MR12<-as.factor(t(as.numeric(R.12,2)))
gadm$MR12 <- MR12
num.col=length(levels(gadm$MR12))-1
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR12)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR12)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#library(raster)
load("./Shapefiles/gadm/Estados.RData")
#png(file="C:/Users/fernamar/Dropbox/Proyecto-Homicidios/MR12.png")
spplot(gadm, "MR12", col.regions=col, colorkey=FALSE, main = "Murder Rate 2012", 
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()


MR11<-as.factor(t(as.numeric(R.11,2)))
gadm$MR11 <- MR11
num.col=length(levels(gadm$MR11))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR11)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR11)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR11.png")
spplot(gadm, "MR11", col.regions=col, colorkey=FALSE, main = "Murder Rate 2011",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR10<-as.factor(t(as.numeric(R.10,2)))
gadm$MR10 <- MR10
num.col=length(levels(gadm$MR10))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR10)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR10)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR10.png")
spplot(gadm, "MR10", col.regions=col, colorkey=FALSE, main = "Murder Rate 2010",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR09<-as.factor(t(as.numeric(R.09,2)))
gadm$MR09 <- MR09
num.col=length(levels(gadm$MR09))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR09)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR09)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR09.png")
spplot(gadm, "MR09", col.regions=col, colorkey=FALSE, main = "Murder Rate 2009",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR08<-as.factor(t(as.numeric(R.08,2)))
gadm$MR08 <- MR08
num.col=length(levels(gadm$MR08))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR08)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR08)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR08.png")
spplot(gadm, "MR08", col.regions=col, colorkey=FALSE, main = "Murder Rate 2008",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR07<-as.factor(t(as.numeric(R.07,2)))
gadm$MR07 <- MR07
num.col=length(levels(gadm$MR07))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR07)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR07)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR07.png")
spplot(gadm, "MR07", col.regions=col, colorkey=FALSE, main = "Murder Rate 2007",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR06<-as.factor(t(as.numeric(R.06,2)))
gadm$MR06 <- MR06
num.col=length(levels(gadm$MR06))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR06)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR06)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR06.png")
spplot(gadm, "MR06", col.regions=col, colorkey=FALSE, main = "Murder Rate 2006",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR05<-as.factor(t(as.numeric(R.05,2)))
gadm$MR05 <- MR05
num.col=length(levels(gadm$MR05))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR05)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR05)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR05.png")
spplot(gadm, "MR05", col.regions=col, colorkey=FALSE, main = "Murder Rate 2005",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR04<-as.factor(t(as.numeric(R.04,2)))
gadm$MR04 <- MR04
num.col=length(levels(gadm$MR04))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR04)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR04)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR04.png")
spplot(gadm, "MR04", col.regions=col, colorkey=FALSE, main = "Murder Rate 2004",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR03<-as.factor(t(as.numeric(R.03,2)))
gadm$MR03 <- MR03
num.col=length(levels(gadm$MR03))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR03)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR03)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR03.png")
spplot(gadm, "MR03", col.regions=col, colorkey=FALSE, main = "Murder Rate 2003",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR02<-as.factor(t(as.numeric(R.02,2)))
gadm$MR02 <- MR02
num.col=length(levels(gadm$MR02))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR02)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR02)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR02.png")
spplot(gadm, "MR02", col.regions=col, colorkey=FALSE, main = "Murder Rate 2002",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR01<-as.factor(t(as.numeric(R.01,2)))
gadm$MR01 <- MR01
num.col=length(levels(gadm$MR01))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR01)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR01)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR01.png")
spplot(gadm, "MR01", col.regions=col, colorkey=FALSE, main = "Murder Rate 2001",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR00<-as.factor(t(as.numeric(R.00,2)))
gadm$MR00 <- MR00
num.col=length(levels(gadm$MR00))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR00)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR00)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR00.png")
spplot(gadm, "MR00", col.regions=col, colorkey=FALSE, main = "Murder Rate 2000",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR99<-as.factor(t(as.numeric(R.99,2)))
gadm$MR99 <- MR99
num.col=length(levels(gadm$MR99))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR99)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR99)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR99.png")
spplot(gadm, "MR99", col.regions=col, colorkey=FALSE, main = "Murder Rate 1999",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR98<-as.factor(t(as.numeric(R.98,2)))
gadm$MR98 <- MR98
num.col=length(levels(gadm$MR98))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR98)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR98)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR98.png")
spplot(gadm, "MR98", col.regions=col, colorkey=FALSE, main = "Murder Rate 1998",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR97<-as.factor(t(as.numeric(R.97,2)))
gadm$MR97 <- MR97
num.col=length(levels(gadm$MR97))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR97)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR97)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR97.png")
spplot(gadm, "MR97", col.regions=col, colorkey=FALSE, main = "Murder Rate 1997",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR96<-as.factor(t(as.numeric(R.96,2)))
gadm$MR96 <- MR96
num.col=length(levels(gadm$MR96))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR96)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR96)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR96.png")
spplot(gadm, "MR96", col.regions=col, colorkey=FALSE, main = "Murder Rate 1996",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR95<-as.factor(t(as.numeric(R.95,2)))
gadm$MR95 <- MR95
num.col=length(levels(gadm$MR95))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR95)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR95)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR95.png")
spplot(gadm, "MR95", col.regions=col, colorkey=FALSE, main = "Murder Rate 1995",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR94<-as.factor(t(as.numeric(R.94,2)))
gadm$MR94 <- MR94
num.col=length(levels(gadm$MR94))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR94)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR94)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR94.png")
spplot(gadm, "MR94", col.regions=col, colorkey=FALSE, main = "Murder Rate 1994",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR93<-as.factor(t(as.numeric(R.93,2)))
gadm$MR93 <- MR93
num.col=length(levels(gadm$MR93))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR93)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR93)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR93.png")
spplot(gadm, "MR93", col.regions=col, colorkey=FALSE, main = "Murder Rate 1993",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR92<-as.factor(t(as.numeric(R.92,2)))
gadm$MR92 <- MR92
num.col=length(levels(gadm$MR92))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR92)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR92)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR92.png")
spplot(gadm, "MR92", col.regions=col, colorkey=FALSE, main = "Murder Rate 1992",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR91<-as.factor(t(as.numeric(R.91,2)))
gadm$MR91 <- MR91
num.col=length(levels(gadm$MR91))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR91)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR91)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR91.png")
spplot(gadm, "MR91", col.regions=col, colorkey=FALSE, main = "Murder Rate 1991",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()

MR90<-as.factor(t(as.numeric(R.90,2)))
gadm$MR90 <- MR90
num.col=length(levels(gadm$MR90))
i=1
for ( i in 1:num.col) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR90)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR90)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
  }
}
col<-c(col, "#080808")
#png(file="MR90.png")
spplot(gadm, "MR90", col.regions=col, colorkey=FALSE, main = "Murder Rate 1990",
       axes=F, col="transparent", sp.layout = list("sp.polygons", Estados)) 
#dev.off()
#dev.new()



#spplot(mexico, "CVEGEO")