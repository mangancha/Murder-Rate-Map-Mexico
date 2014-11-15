##Required Libraries#################################################################################

library(car)  #For recode
library(maps) # For maps
library(mapdata) # More maps for maps
library(sp) # For spplot
library(maptools) # To read shapefiles to sp objects
library(colorRamps)
library("gplots")
library(raster) # For getData (download files from gadm)
library(rgdal) # To import shapefiles


##Import and extract Municipios and Municipal Populations from Census/Counts data####################

#Censo 1990
ITER_NALTXT90 <- read.delim("./Primary_data/ITER_NALTXT90.txt", fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT90, entidad >0 & mun >0 & loc == 0)
Mun.Pop.90 <- data.frame(MUNICIPIO = tmp$nom_mun)
Mun.Pop.90$POBLACION <- tmp$p_total
for(i in 1:length(tmp$entidad)) {
  if(i==1) {
    Mun.Pop.90.ID <- "01001"
  } else {
    if(as.numeric(tmp$entidad[i]<10)) {
      if(as.numeric(tmp$mun[i]<10)) {
        Mun.Pop.90.ID <- c(Mun.Pop.90.ID, as.character(paste("0", tmp$entidad[i], "00", tmp$mun[i], sep="")))
      } else if (as.numeric(tmp$mun[i]>9) & tmp$mun[i]<100){
        Mun.Pop.90.ID <- c(Mun.Pop.90.ID, as.character(paste("0", tmp$entidad[i], "0", tmp$mun[i], sep="")))
      } else {
        Mun.Pop.90.ID <- c(Mun.Pop.90.ID, as.character(paste("0", tmp$entidad[i], tmp$mun[i], sep="")))
      }
    } else {
      if(as.numeric(tmp$mun[i]<10)) {
        Mun.Pop.90.ID <- c(Mun.Pop.90.ID, as.character(paste(tmp$entidad[i], "00", tmp$mun[i], sep="")))
      } else if (as.numeric(tmp$mun[i]>9) & tmp$mun[i]<100){
        Mun.Pop.90.ID <- c(Mun.Pop.90.ID, as.character(paste(tmp$entidad[i], "0", tmp$mun[i], sep="")))
      } else {
        Mun.Pop.90.ID <- c(Mun.Pop.90.ID, as.character(paste(tmp$entidad[i], tmp$mun[i], sep="")))
      }
    } 
  }
}
Mun.Pop.90$ID <- cbind(Mun.Pop.90.ID)

#Censo 1995
ITER_NALTXT95 <- read.delim("./Primary_data/ITER_NALTXT95.txt", header=FALSE, fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT95, V1 >0 & V3 >0 & V5 == 0)
Mun.Pop.95 <- data.frame(MUNICIPIO = tmp$V4)
Mun.Pop.95$POBLACION <- tmp$V10
Mun.Pop.95$ID <- as.numeric(paste(tmp$V1, "000", tmp$V3, sep=""))
for(i in 1:length(tmp$V1)) {
  if(i==1) {
    Mun.Pop.95.ID <- "01001"
  } else {
    if(as.numeric(tmp$V1[i]<10)) {
      if(as.numeric(tmp$V3[i]<10)) {
        Mun.Pop.95.ID <- c(Mun.Pop.95.ID, as.character(paste("0", tmp$V1[i], "00", tmp$V3[i], sep="")))
      } else if (as.numeric(tmp$V3[i]>9) & tmp$V3[i]<100){
        Mun.Pop.95.ID <- c(Mun.Pop.95.ID, as.character(paste("0", tmp$V1[i], "0", tmp$V3[i], sep="")))
      } else {
        Mun.Pop.95.ID <- c(Mun.Pop.95.ID, as.character(paste("0", tmp$V1[i], tmp$V3[i], sep="")))
      }
    } else {
      if(as.numeric(tmp$V3[i]<10)) {
        Mun.Pop.95.ID <- c(Mun.Pop.95.ID, as.character(paste(tmp$V1[i], "00", tmp$V3[i], sep="")))
      } else if (as.numeric(tmp$V3[i]>9) & tmp$V3[i]<100){
        Mun.Pop.95.ID <- c(Mun.Pop.95.ID, as.character(paste(tmp$V1[i], "0", tmp$V3[i], sep="")))
      } else {
        Mun.Pop.95.ID <- c(Mun.Pop.95.ID, as.character(paste(tmp$V1[i], tmp$V3[i], sep="")))
      }
    } 
  }
}
Mun.Pop.95$ID <- cbind(Mun.Pop.95.ID)

#Censo 2000
ITER_NALTXT00 <- read.delim("./Primary_data/ITER_NALTXT00.txt", header=FALSE, fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT00, V1 >0 & V3 >0 & V5 == 0)
Mun.Pop.00 <- data.frame(MUNICIPIO = tmp$V4)
Mun.Pop.00$POBLACION <- tmp$V10
Mun.Pop.00$ID <- as.numeric(paste(tmp$V1, "000", tmp$V3, sep=""))
for(i in 1:length(tmp$V1)) {
  if(i==1) {
    Mun.Pop.00.ID <- "01001"
  } else {
    if(as.numeric(tmp$V1[i]<10)) {
      if(as.numeric(tmp$V3[i]<10)) {
        Mun.Pop.00.ID <- c(Mun.Pop.00.ID, as.character(paste("0", tmp$V1[i], "00", tmp$V3[i], sep="")))
      } else if (as.numeric(tmp$V3[i]>9) & tmp$V3[i]<100){
        Mun.Pop.00.ID <- c(Mun.Pop.00.ID, as.character(paste("0", tmp$V1[i], "0", tmp$V3[i], sep="")))
      } else {
        Mun.Pop.00.ID <- c(Mun.Pop.00.ID, as.character(paste("0", tmp$V1[i], tmp$V3[i], sep="")))
      }
    } else {
      if(as.numeric(tmp$V3[i]<10)) {
        Mun.Pop.00.ID <- c(Mun.Pop.00.ID, as.character(paste(tmp$V1[i], "00", tmp$V3[i], sep="")))
      } else if (as.numeric(tmp$V3[i]>9) & tmp$V3[i]<100){
        Mun.Pop.00.ID <- c(Mun.Pop.00.ID, as.character(paste(tmp$V1[i], "0", tmp$V3[i], sep="")))
      } else {
        Mun.Pop.00.ID <- c(Mun.Pop.00.ID, as.character(paste(tmp$V1[i], tmp$V3[i], sep="")))
      }
    } 
  }
}
Mun.Pop.00$ID <- cbind(Mun.Pop.00.ID)


#Censo 2005
ITER_NALTXT05 <- read.delim("./Primary_data/ITER_NALTXT05.txt", header=FALSE, fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT05, V1 >0 & V3 >0 & V5 == 0)
Mun.Pop.05 <- data.frame(MUNICIPIO = tmp$V4)
Mun.Pop.05$POBLACION <- tmp$V10
Mun.Pop.05$ID <- as.numeric(paste(tmp$V1, "000", tmp$V3, sep=""))
for(i in 1:length(tmp$V1)) {
  if(i==1) {
    Mun.Pop.05.ID <- "01001"
  } else {
    if(as.numeric(tmp$V1[i]<10)) {
      if(as.numeric(tmp$V3[i]<10)) {
        Mun.Pop.05.ID <- c(Mun.Pop.05.ID, as.character(paste("0", tmp$V1[i], "00", tmp$V3[i], sep="")))
      } else if (as.numeric(tmp$V3[i]>9) & tmp$V3[i]<100){
        Mun.Pop.05.ID <- c(Mun.Pop.05.ID, as.character(paste("0", tmp$V1[i], "0", tmp$V3[i], sep="")))
      } else {
        Mun.Pop.05.ID <- c(Mun.Pop.05.ID, as.character(paste("0", tmp$V1[i], tmp$V3[i], sep="")))
      }
    } else {
      if(as.numeric(tmp$V3[i]<10)) {
        Mun.Pop.05.ID <- c(Mun.Pop.05.ID, as.character(paste(tmp$V1[i], "00", tmp$V3[i], sep="")))
      } else if (as.numeric(tmp$V3[i]>9) & tmp$V3[i]<100){
        Mun.Pop.05.ID <- c(Mun.Pop.05.ID, as.character(paste(tmp$V1[i], "0", tmp$V3[i], sep="")))
      } else {
        Mun.Pop.05.ID <- c(Mun.Pop.05.ID, as.character(paste(tmp$V1[i], tmp$V3[i], sep="")))
      }
    } 
  }
}
Mun.Pop.05$ID <- cbind(Mun.Pop.05.ID)

#Censo 2010
ITER_NALTXT10 <- read.delim("./Primary_data/ITER_NALTXT10.TXT", fileEncoding="ISO-8859-1")
tmp <- subset(ITER_NALTXT10, ENTIDAD >0 & MUN >0 & LOC == 0)
Mun.Pop.10 <- data.frame(MUNICIPIO = tmp$NOM_MUN)
Mun.Pop.10$POBLACION <- tmp$POBTOT
for(i in 1:length(tmp$ENTIDAD)) {
  if(i==1) {
    Mun.Pop.10.ID <- "01001"
  } else {
    if(as.numeric(tmp$ENTIDAD[i]<10)) {
      if(as.numeric(tmp$MUN[i]<10)) {
        Mun.Pop.10.ID <- c(Mun.Pop.10.ID, as.character(paste("0", tmp$ENTIDAD[i], "00", tmp$MUN[i], sep="")))
      } else if (as.numeric(tmp$MUN[i]>9) & tmp$MUN[i]<100){
        Mun.Pop.10.ID <- c(Mun.Pop.10.ID, as.character(paste("0", tmp$ENTIDAD[i], "0", tmp$MUN[i], sep="")))
      } else {
        Mun.Pop.10.ID <- c(Mun.Pop.10.ID, as.character(paste("0", tmp$ENTIDAD[i], tmp$MUN[i], sep="")))
      }
    } else {
      if(as.numeric(tmp$MUN[i]<10)) {
        Mun.Pop.10.ID <- c(Mun.Pop.10.ID, as.character(paste(tmp$ENTIDAD[i], "00", tmp$MUN[i], sep="")))
      } else if (as.numeric(tmp$MUN[i]>9) & tmp$MUN[i]<100){
        Mun.Pop.10.ID <- c(Mun.Pop.10.ID, as.character(paste(tmp$ENTIDAD[i], "0", tmp$MUN[i], sep="")))
      } else {
        Mun.Pop.10.ID <- c(Mun.Pop.10.ID, as.character(paste(tmp$ENTIDAD[i], tmp$MUN[i], sep="")))
      }
    } 
  }
}
Mun.Pop.10$ID <- cbind(Mun.Pop.10.ID)
Mun.Pop.10$ENT <- tmp$ENTIDAD
Mun.Pop.10$MUN <- tmp$MUN
Mun.Pop.10$NOM_ENT <- tmp$NOM_ENT

##Generate data for intercensus years and Consolidate in one data frame####################################
POP.INEGI.EST <- data.frame(ID = Mun.Pop.10$ID)
POP.INEGI.EST$MUNICIPIO <- Mun.Pop.10$MUNICIPIO
P.10 <- Mun.Pop.10$POBLACION
Mun.Pop.05$POBLACION[Mun.Pop.05$ID==Mun.Pop.10$ID[i]]

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.05<-Mun.Pop.05$POBLACION[Mun.Pop.05$ID==Mun.Pop.10$ID[i]]
  } else {
    if(is.integer(Mun.Pop.05$POBLACION[Mun.Pop.05$ID==Mun.Pop.10$ID[i]]) 
       && length(Mun.Pop.05$POBLACION[Mun.Pop.05$ID==Mun.Pop.10$ID[i]]) == 0L) {
      P.05<-c(P.05, "NA")
    } else {
      P.05<-c(P.05, Mun.Pop.05$POBLACION[Mun.Pop.05$ID==Mun.Pop.10$ID[i]])
    }
  }
}
P.05 <- as.numeric(P.05)

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    if(is.na(P.10[i]) | is.na(P.05[i])) {
      P.09.08.07.06 <- c("NA", "NA", "NA", "NA")
      P.13.12.11 <- c("NA","NA", "NA")
    } else {
      Step <- round((P.10[i]-P.05[i])/5,0)
      P.09.08.07.06 <- c(P.05[i]+Step+Step+Step+Step, P.05[i]+Step+Step+Step, P.05[i]+Step+Step, P.05[i]+Step)
      P.13.12.11 <- c(P.10[i]+Step+Step+Step, P.10[i]+Step+Step, P.10[i]+Step)
    }
  } else {
    if(is.na(P.10[i]) | is.na(P.05[i])) {
      P.09.08.07.06 <- c(P.09.08.07.06, "NA", "NA", "NA", "NA")
      P.13.12.11 <- c(P.13.12.11, "NA", "NA", "NA")
    } else {
      Step <- round((P.10[i]-P.05[i])/5,0)
      P.09.08.07.06 <- c(P.09.08.07.06, P.05[i]+Step+Step+Step+Step, P.05[i]+Step+Step+Step, P.05[i]+Step+Step, P.05[i]+Step)
      P.13.12.11 <- c(P.13.12.11, P.10[i]+Step+Step+Step, P.10[i]+Step+Step, P.10[i]+Step)
    }
  }
}
P.09.08.07.06 <- matrix(P.09.08.07.06,nrow = length(Mun.Pop.10$ID),ncol = 4, byrow=TRUE)
P.13.12.11 <- matrix(P.13.12.11,nrow = length(Mun.Pop.10$ID),ncol = 3, byrow=TRUE)
POP.INEGI.EST$P.13 <- P.13.12.11[,1]
POP.INEGI.EST$P.12 <- P.13.12.11[,2]
POP.INEGI.EST$P.11 <- P.13.12.11[,3]
POP.INEGI.EST$P.10 <- P.10
POP.INEGI.EST$P.09 <- P.09.08.07.06[,1]
POP.INEGI.EST$P.08 <- P.09.08.07.06[,2]
POP.INEGI.EST$P.07 <- P.09.08.07.06[,3]
POP.INEGI.EST$P.06 <- P.09.08.07.06[,4]
POP.INEGI.EST$P.05 <- P.05

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.00<-Mun.Pop.00$POBLACION[Mun.Pop.00$ID==Mun.Pop.10$ID[i]]
  } else {
    if(is.integer(Mun.Pop.00$POBLACION[Mun.Pop.00$ID==Mun.Pop.10$ID[i]]) 
       && length(Mun.Pop.00$POBLACION[Mun.Pop.00$ID==Mun.Pop.10$ID[i]]) == 0L) {
      P.00<-c(P.00, "NA")
    } else {
      P.00<-c(P.00, Mun.Pop.00$POBLACION[Mun.Pop.00$ID==Mun.Pop.10$ID[i]])
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
POP.INEGI.EST$P.04 <- P.04.03.02.01[,1]
POP.INEGI.EST$P.03 <- P.04.03.02.01[,2]
POP.INEGI.EST$P.02 <- P.04.03.02.01[,3]
POP.INEGI.EST$P.01 <- P.04.03.02.01[,4]
POP.INEGI.EST$P.00 <- P.00

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.95<-Mun.Pop.95$POBLACION[Mun.Pop.95$ID==Mun.Pop.10$ID[i]]
  } else {
    if(is.integer(Mun.Pop.95$POBLACION[Mun.Pop.95$ID==Mun.Pop.10$ID[i]]) 
       && length(Mun.Pop.95$POBLACION[Mun.Pop.95$ID==Mun.Pop.10$ID[i]]) == 0L) {
      P.95<-c(P.95, "NA")
    } else {
      P.95<-c(P.95, Mun.Pop.95$POBLACION[Mun.Pop.95$ID==Mun.Pop.10$ID[i]])
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
POP.INEGI.EST$P.99 <- P.99.98.97.96[,1]
POP.INEGI.EST$P.98 <- P.99.98.97.96[,2]
POP.INEGI.EST$P.97 <- P.99.98.97.96[,3]
POP.INEGI.EST$P.96 <- P.99.98.97.96[,4]
POP.INEGI.EST$P.95 <- P.95

for(i in 1:length(Mun.Pop.10$ID)) {
  if(i==1) {
    P.90<-Mun.Pop.90$POBLACION[Mun.Pop.90$ID==Mun.Pop.10$ID[i]]
  } else {
    if(is.integer(Mun.Pop.90$POBLACION[Mun.Pop.90$ID==Mun.Pop.10$ID[i]]) 
       && length(Mun.Pop.90$POBLACION[Mun.Pop.90$ID==Mun.Pop.10$ID[i]]) == 0L) {
      P.90<-c(P.90, "NA")
    } else {
      P.90<-c(P.90, Mun.Pop.90$POBLACION[Mun.Pop.90$ID==Mun.Pop.10$ID[i]])
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
POP.INEGI.EST$P.94 <- P.94.93.92.91[,1]
POP.INEGI.EST$P.93 <- P.94.93.92.91[,2]
POP.INEGI.EST$P.92 <- P.94.93.92.91[,3]
POP.INEGI.EST$P.91 <- P.94.93.92.91[,4]
POP.INEGI.EST$P.90 <- P.90


MAIN.POP<-POP.INEGI.EST

##Import Homicidios##############################################################################
Murders.tmp<-read.csv("./Primary_data/Homicidios-INEGI.csv", fileEncoding = "ISO-8859-1", header = FALSE, sep = ",")

Murders.tmp <- t(Murders.tmp)
colnames(Murders.tmp) = as.character(Murders.tmp[1, ])
Yr<-Murders.tmp[1,]
Murders.tmp<-Murders.tmp[-1,]
Murders.tmp <- data.frame(Murders.tmp)

Homicidios.Yr<-Murders.tmp[1,]
Homicidios.Yr<-Homicidios.Yr[,-c(1:3)]
Yr<-Yr[-c(1:3)]
Yr<-rev(Yr)
Homicidios.Yr<-rev(t(Homicidios.Yr))
Homicidios.Yr <- cbind(Yr, Homicidios.Yr)
colnames(Homicidios.Yr) = c("Yr", "Homicidios")
Homicidios.Yr <- as.data.frame(Homicidios.Yr)

Murders.est <- subset(Murders.tmp, !is.na(ENT) & MUN == 0 & MUN != "NE" & MUN != "")

Murders.tmp <- subset(Murders.tmp, !is.na(ENT) & MUN != 0 & MUN != "NE" & MUN != "")

for(i in 1:length(Murders.tmp$Año)) {
  if(i==1) {
    ID <- "01001"
  } else {
    if(as.numeric(as.character(Murders.tmp$ENT[i]))<10) {
      if(as.numeric(as.character(Murders.tmp$MUN[i]))<10) {
        ID <- c(ID, as.character(paste("0", Murders.tmp$ENT[i], "00", Murders.tmp$MUN[i], sep="")))
      } else if (as.numeric(as.character(Murders.tmp$MUN[i]))>9 & as.numeric(as.character(Murders.tmp$MUN[i]))<100){
        ID <- c(ID, as.character(paste("0", Murders.tmp$ENT[i], "0", Murders.tmp$MUN[i], sep="")))
      } else {
        ID <- c(ID, as.character(paste("0", Murders.tmp$ENT[i], Murders.tmp$MUN[i], sep="")))
      }
    } else {
      if(as.numeric(as.character(Murders.tmp$MUN[i]))<10) {
        ID <- c(ID, as.character(paste(Murders.tmp$ENT[i], "00", Murders.tmp$MUN[i], sep="")))
      } else if (as.numeric(as.character(Murders.tmp$MUN[i]))>9 & as.numeric(as.character(Murders.tmp$MUN[i]))<100){
        ID <- c(ID, as.character(paste(Murders.tmp$ENT[i], "0", Murders.tmp$MUN[i], sep="")))
      } else {
        ID <- c(ID, as.character(paste(Murders.tmp$ENT[i], Murders.tmp$MUN[i], sep="")))
      }
    } 
  }
}

Murders <- data.frame(ID = ID)
Murders$NOM <- Murders.tmp$Año
Murders$ENT <- Murders.tmp$ENT
Murders$MUN <- Murders.tmp$MUN

Murders$M.13 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,27]))))
Murders$M.12 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,26]))))
Murders$M.11 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,25]))))
Murders$M.10 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,24]))))
Murders$M.09 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,23]))))
Murders$M.08 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,22]))))
Murders$M.07 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,21]))))
Murders$M.06 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,20]))))
Murders$M.05 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,19]))))
Murders$M.04 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,18]))))
Murders$M.03 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,17]))))
Murders$M.02 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,16]))))
Murders$M.01 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,15]))))
Murders$M.00 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,14]))))
Murders$M.99 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,13]))))
Murders$M.98 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,12]))))
Murders$M.97 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,11]))))
Murders$M.96 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,10]))))
Murders$M.95 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,9]))))
Murders$M.94 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,8]))))
Murders$M.93 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,7]))))
Murders$M.92 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,6]))))
Murders$M.91 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,5]))))
Murders$M.90 <- as.numeric(as.character(gsub(",","",(Murders.tmp[,4]))))

write.csv(Murders, "Secondary_data/Murders.csv", row.names = TRUE)

Murders[is.na(Murders)]<-0

Murders <- subset(Murders, MUN != "MUN")
Murders <- subset(Murders, ENT != "33")
Murders <- subset(Murders, NOM != "No especificado")
#Murders <- data.frame(matrix(unlist(Murders), nrow=length(Murders$ID), byrow=F))
#Murders <- as.data.frame(Murders)
colnames(MAIN.POP)[1] = "CVEGEO"
for(i in 1:length(MAIN.POP$CVEGEO)) {
  if(i==1) {
    Murders.Fin<-Murders[i,]
  } else {
    if( identical(as.character(subset(Murders, Murders$ID == as.character(MAIN.POP$CVEGEO[i]))$ID), character(0))) {
      Murders.Fin<-rbind(Murders.Fin, rep("NA", length=ncol(Murders)))
    } else {
      Murders.Fin<-rbind(Murders.Fin, subset(Murders, Murders$ID == as.character(MAIN.POP$CVEGEO[i])))
    }
  }
}


##Calculate murder rate#################################################################################

Murder.Rate <- data.frame(MAIN.POP$CVEGEO)
Murder.Rate$R.13 <- round(((as.numeric(gsub(",","",Murders.Fin$M.13)))/(as.numeric(gsub(",","",MAIN.POP$P.13)))*100000),2)
Murder.Rate$R.12 <- round(((as.numeric(gsub(",","",Murders.Fin$M.12)))/(as.numeric(gsub(",","",MAIN.POP$P.12)))*100000),2)
Murder.Rate$R.11 <- round(((as.numeric(as.character(gsub(",","",Murders.Fin$M.11))))/(as.numeric(as.character(gsub(",","",MAIN.POP$P.11))))*100000),2)
Murder.Rate$R.10 <- round(((as.numeric(gsub(",","",Murders.Fin$M.10)))/(as.numeric(gsub(",","",MAIN.POP$P.10)))*100000),2)
Murder.Rate$R.09 <- round(((as.numeric(gsub(",","",Murders.Fin$M.09)))/(as.numeric(gsub(",","",MAIN.POP$P.09)))*100000),2)
Murder.Rate$R.08 <- round(((as.numeric(gsub(",","",Murders.Fin$M.08)))/(as.numeric(gsub(",","",MAIN.POP$P.08)))*100000),2)
Murder.Rate$R.07 <- round(((as.numeric(gsub(",","",Murders.Fin$M.07)))/(as.numeric(gsub(",","",MAIN.POP$P.07)))*100000),2)
Murder.Rate$R.06 <- round(((as.numeric(gsub(",","",Murders.Fin$M.06)))/(as.numeric(gsub(",","",MAIN.POP$P.06)))*100000),2)
Murder.Rate$R.05 <- round(((as.numeric(gsub(",","",Murders.Fin$M.05)))/(as.numeric(gsub(",","",MAIN.POP$P.05)))*100000),2)
Murder.Rate$R.04 <- round(((as.numeric(gsub(",","",Murders.Fin$M.04)))/(as.numeric(gsub(",","",MAIN.POP$P.04)))*100000),2)
Murder.Rate$R.03 <- round(((as.numeric(gsub(",","",Murders.Fin$M.03)))/(as.numeric(gsub(",","",MAIN.POP$P.03)))*100000),2)
Murder.Rate$R.02 <- round(((as.numeric(gsub(",","",Murders.Fin$M.02)))/(as.numeric(gsub(",","",MAIN.POP$P.02)))*100000),2)
Murder.Rate$R.01 <- round(((as.numeric(gsub(",","",Murders.Fin$M.01)))/(as.numeric(gsub(",","",MAIN.POP$P.01)))*100000),2)
Murder.Rate$R.00 <- round(((as.numeric(gsub(",","",Murders.Fin$M.00)))/(as.numeric(gsub(",","",MAIN.POP$P.00)))*100000),2)
Murder.Rate$R.99 <- round(((as.numeric(gsub(",","",Murders.Fin$M.99)))/(as.numeric(gsub(",","",MAIN.POP$P.99)))*100000),2)
Murder.Rate$R.98 <- round(((as.numeric(gsub(",","",Murders.Fin$M.98)))/(as.numeric(gsub(",","",MAIN.POP$P.98)))*100000),2)
Murder.Rate$R.97 <- round(((as.numeric(gsub(",","",Murders.Fin$M.97)))/(as.numeric(gsub(",","",MAIN.POP$P.97)))*100000),2)
Murder.Rate$R.96 <- round(((as.numeric(gsub(",","",Murders.Fin$M.96)))/(as.numeric(gsub(",","",MAIN.POP$P.96)))*100000),2)
Murder.Rate$R.95 <- round(((as.numeric(gsub(",","",Murders.Fin$M.95)))/(as.numeric(gsub(",","",MAIN.POP$P.95)))*100000),2)
Murder.Rate$R.94 <- round(((as.numeric(gsub(",","",Murders.Fin$M.94)))/(as.numeric(gsub(",","",MAIN.POP$P.94)))*100000),2)
Murder.Rate$R.93 <- round(((as.numeric(gsub(",","",Murders.Fin$M.93)))/(as.numeric(gsub(",","",MAIN.POP$P.93)))*100000),2)
Murder.Rate$R.92 <- round(((as.numeric(gsub(",","",Murders.Fin$M.92)))/(as.numeric(gsub(",","",MAIN.POP$P.92)))*100000),2)
Murder.Rate$R.91 <- round(((as.numeric(gsub(",","",Murders.Fin$M.91)))/(as.numeric(gsub(",","",MAIN.POP$P.91)))*100000),2)
Murder.Rate$R.90 <- round(((as.numeric(gsub(",","",Murders.Fin$M.90)))/(as.numeric(gsub(",","",MAIN.POP$P.90)))*100000),2)

colnames(Murder.Rate)[1]<-"ID"

##Align data with INEGI shapefile#########################################################################


mexico10<-readOGR(".//Shapefiles//INEGI//mgm2010v5_0a", 
                  layer="Municipios_2010_5A")
for(i in 1:length(mexico10$CVE_ENT)) {
  if(i==1) {
    CVEGEO10 <- "01001"
  } else {
    CVEGEO10 <- c(CVEGEO10, paste(mexico10$CVE_ENT[i], mexico10$CVE_MUN[i], sep=""))
  }
}
mexico10$CVEGEO <- CVEGEO10

for(i in 1:length(mexico10$CVEGEO)) {
  if(i==1) {
    R.13<-as.numeric(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.13)
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$ID)==0) {
      R.13<- c(R.13, "-0.001")
    } else {
      R.13<-c(R.13, as.numeric(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.13))
    }
  }
}

for(i in 1:length(mexico10$CVEGEO)) {
  if(i==1) {
    R.12<-as.numeric(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.12)
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$ID)==0) {
      R.12<- c(R.12, "-0.001")
    } else {
      R.12<-c(R.12, as.numeric(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.12))
    }
  }
}


for(i in 1:length(mexico10$CVEGEO)) {
  if(i==1) {
    R.11<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.11
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$ID)==0) {
      R.11<- c(R.11, "-0.001")
    } else {
      R.11<-c(R.11, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.11)
    }
  }
}

for(i in 1:length(mexico10$CVEGEO)) {
  if(i==1) {
    R.10<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.10
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$ID)==0) {
      R.10<- c(R.10, "-0.001")
    } else {
      R.10<-c(R.10, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico10$CVEGEO[i]))$R.10)
    }
  }
}


mexico00<-readOGR(".//Shapefiles//INEGI//mgm2000", 
                  layer="Municipios_2000")
#mexico05<-subset(mexico, !is.na(mexico05$NOMBRE))

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.09<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.09
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.09<- c(R.09, "-0.001")
    } else {
      R.09<-c(R.09, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.09)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.08<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.08
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.08<- c(R.08, "-0.001")
    } else {
      R.08<-c(R.08, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.08)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.07<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.07
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.07<- c(R.07, "-0.001")
    } else {
      R.07<-c(R.07, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.07)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.06<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.06
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.06<- c(R.06, "-0.001")
    } else {
      R.06<-c(R.06, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.06)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.05<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.05
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.05<- c(R.05, "-0.001")
    } else {
      R.05<-c(R.05, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.05)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.04<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.04
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.04<- c(R.04, "-0.001")
    } else {
      R.04<-c(R.04, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.04)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.03<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.03
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.03<- c(R.03, "-0.001")
    } else {
      R.03<-c(R.03, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.03)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.02<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.02
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.02<- c(R.02, "-0.001")
    } else {
      R.02<-c(R.02, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.02)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.01<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.01
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.01<- c(R.01, "-0.001")
    } else {
      R.01<-c(R.01, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.01)
    }
  }
}

for(i in 1:length(mexico00$CVEMUNI)) {
  if(i==1) {
    R.00<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.00
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$ID)==0) {
      R.00<- c(R.00, "-0.001")
    } else {
      R.00<-c(R.00, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico00$CVEMUNI[i]))$R.00)
    }
  }
}

mexico95<-readOGR(".//Shapefiles//INEGI//mgm1995", 
                  layer="Municipios_1995")
#mexico05<-subset(mexico, !is.na(mexico05$NOMBRE))
for(i in 1:length(mexico95$CVE_ENT)) {
  if(i==1) {
    CVEGEO95 <- "01001"
  } else {
    CVEGEO95 <- c(CVEGEO95, paste(mexico95$CVE_ENT[i], mexico95$CVE_MUN[i], sep=""))
  }
}
mexico95$CVEGEO <- CVEGEO95

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.99<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.99
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.99<- c(R.99, "-0.001")
    } else {
      R.99<-c(R.99, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.99)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.98<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.98
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.98<- c(R.98, "-0.001")
    } else {
      R.98<-c(R.98, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.98)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.97<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.97
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.97<- c(R.97, "-0.001")
    } else {
      R.97<-c(R.97, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.97)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.96<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.96
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.96<- c(R.96, "-0.001")
    } else {
      R.96<-c(R.96, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.96)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.95<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.95
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.95<- c(R.95, "-0.001")
    } else {
      R.95<-c(R.95, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.95)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.94<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.94
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.94<- c(R.94, "-0.001")
    } else {
      R.94<-c(R.94, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.94)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.93<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.93
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.93<- c(R.93, "-0.001")
    } else {
      R.93<-c(R.93, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.93)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.92<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.92
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.92<- c(R.92, "-0.001")
    } else {
      R.92<-c(R.92, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.92)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.91<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.91
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.91<- c(R.91, "-0.001")
    } else {
      R.91<-c(R.91, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.91)
    }
  }
}

for(i in 1:length(mexico95$CVEGEO)) {
  if(i==1) {
    R.90<-subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.90
  } else {
    if( length(subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$ID)==0) {
      R.90<- c(R.90, "-0.001")
    } else {
      R.90<-c(R.90, subset(Murder.Rate, Murder.Rate$ID == as.character(mexico95$CVEGEO[i]))$R.90)
    }
  }
}

MR10 <- data.frame(mexico10$CVEGEO)
MR10$MUNICIPIO <- mexico10$NOMBRE
MR10$NOM_ENT <- mexico10$NOM_ENT
MR10$MR13<-R.13
MR10$MR12<-R.12
MR10$MR11<-R.11
MR10$MR10<-R.10

MR00 <- data.frame(mexico00$CVEMUNI)
MR00$MUNICIPIO <- mexico00$NOM_MUN
MR00$MR09<-R.09
MR00$MR08<-R.08
MR00$MR07<-R.07
MR00$MR06<-R.06
MR00$MR05<-R.05
MR00$MR04<-R.04
MR00$MR03<-R.03
MR00$MR02<-R.02
MR00$MR01<-R.01
MR00$MR00<-R.00

MR95 <- data.frame(mexico95$CVEGEO)
MR95$MUNICIPIO <- mexico95$NOM_MUN
MR95$ENTIDAD <- mexico95$NOM_ENT
MR95$MR99<-R.99
MR95$MR98<-R.98
MR95$MR97<-R.97
MR95$MR96<-R.96
MR95$MR95<-R.95
MR95$MR94<-R.94
MR95$MR93<-R.93
MR95$MR92<-R.92
MR95$MR91<-R.91
MR95$MR90<-R.90


#save(MR, file="./Secondary_data/MR.RData")
#write.csv(MR, "Secondary_data/MR.csv", row.names = FALSE, na="-0.001")
write.csv(POP.INEGI.EST, "Secondary_data/POP.INEGI.EST.csv", row.names = FALSE)
#write.csv(POP.CONAPO, "Secondary_data/POP.CONAPO.csv", row.names = FALSE)
write.csv(MR00, "Secondary_data/MR00.csv", row.names = FALSE, na="-1")
write.csv(MR10, "Secondary_data/MR10.csv", row.names = FALSE, na="-1")
write.csv(MR95, "Secondary_data/MR95.csv", row.names = FALSE, na="-1")



##Generate Estate data for intercensus years and Consolidate in one data frame###################
tmp <- subset(ITER_NALTXT10, ENTIDAD >0 & MUN ==0 & LOC ==0)
EST.POP <- data.frame(ENT = tmp$ENT)
EST.POP$ENTIDAD <- tmp$NOM_ENT
EST.POP$P.10 <- tmp$POBTOT
tmp <- subset(ITER_NALTXT05, V1 >0 & V3 ==0 & V5 ==0)
EST.POP$P.05 <- tmp$V10
tmp <- subset(ITER_NALTXT00, V1 >0 & V3 ==0 & V5 ==0)
EST.POP$P.00 <- tmp$V10
tmp <- subset(ITER_NALTXT95, V1 >0 & V3 ==0 & V5 ==0)
EST.POP$P.95 <- tmp$V10
tmp <- subset(ITER_NALTXT90, entidad >0 & mun ==0 & loc ==0)
EST.POP$P.90 <- tmp$p_total

POP.INEGI.ESTATAL.ESTIMADA <- data.frame(ENT = EST.POP$ENT)
POP.INEGI.ESTATAL.ESTIMADA$ENTIDAD <- EST.POP$ENTIDAD
POP.INEGI.ESTATAL.ESTIMADA$P.13 <- EST.POP$P.10+(((EST.POP$P.10-EST.POP$P.05)/5)*3)
POP.INEGI.ESTATAL.ESTIMADA$P.12 <- EST.POP$P.10+(((EST.POP$P.10-EST.POP$P.05)/5)*2)
POP.INEGI.ESTATAL.ESTIMADA$P.11 <- EST.POP$P.10+(((EST.POP$P.10-EST.POP$P.05)/5)*1)
POP.INEGI.ESTATAL.ESTIMADA$P.10 <- EST.POP$P.10+(((EST.POP$P.10-EST.POP$P.05)/5)*0)
POP.INEGI.ESTATAL.ESTIMADA$P.09 <- EST.POP$P.05+(((EST.POP$P.10-EST.POP$P.05)/5)*4)
POP.INEGI.ESTATAL.ESTIMADA$P.08 <- EST.POP$P.05+(((EST.POP$P.10-EST.POP$P.05)/5)*3)
POP.INEGI.ESTATAL.ESTIMADA$P.07 <- EST.POP$P.05+(((EST.POP$P.10-EST.POP$P.05)/5)*2)
POP.INEGI.ESTATAL.ESTIMADA$P.06 <- EST.POP$P.05+(((EST.POP$P.10-EST.POP$P.05)/5)*1)
POP.INEGI.ESTATAL.ESTIMADA$P.05 <- EST.POP$P.05+(((EST.POP$P.10-EST.POP$P.05)/5)*0)
POP.INEGI.ESTATAL.ESTIMADA$P.04 <- EST.POP$P.00+(((EST.POP$P.05-EST.POP$P.00)/5)*4)
POP.INEGI.ESTATAL.ESTIMADA$P.03 <- EST.POP$P.00+(((EST.POP$P.05-EST.POP$P.00)/5)*3)
POP.INEGI.ESTATAL.ESTIMADA$P.02 <- EST.POP$P.00+(((EST.POP$P.05-EST.POP$P.00)/5)*2)
POP.INEGI.ESTATAL.ESTIMADA$P.01 <- EST.POP$P.00+(((EST.POP$P.05-EST.POP$P.00)/5)*1)
POP.INEGI.ESTATAL.ESTIMADA$P.00 <- EST.POP$P.00+(((EST.POP$P.05-EST.POP$P.00)/5)*0)
POP.INEGI.ESTATAL.ESTIMADA$P.99 <- EST.POP$P.95+(((EST.POP$P.00-EST.POP$P.95)/5)*4)
POP.INEGI.ESTATAL.ESTIMADA$P.98 <- EST.POP$P.95+(((EST.POP$P.00-EST.POP$P.95)/5)*3)
POP.INEGI.ESTATAL.ESTIMADA$P.97 <- EST.POP$P.95+(((EST.POP$P.00-EST.POP$P.95)/5)*2)
POP.INEGI.ESTATAL.ESTIMADA$P.96 <- EST.POP$P.95+(((EST.POP$P.00-EST.POP$P.95)/5)*1)
POP.INEGI.ESTATAL.ESTIMADA$P.95 <- EST.POP$P.95+(((EST.POP$P.00-EST.POP$P.95)/5)*0)
POP.INEGI.ESTATAL.ESTIMADA$P.94 <- EST.POP$P.90+(((EST.POP$P.95-EST.POP$P.90)/5)*4)
POP.INEGI.ESTATAL.ESTIMADA$P.93 <- EST.POP$P.90+(((EST.POP$P.95-EST.POP$P.90)/5)*3)
POP.INEGI.ESTATAL.ESTIMADA$P.92 <- EST.POP$P.90+(((EST.POP$P.95-EST.POP$P.90)/5)*2)
POP.INEGI.ESTATAL.ESTIMADA$P.91 <- EST.POP$P.90+(((EST.POP$P.95-EST.POP$P.90)/5)*1)
POP.INEGI.ESTATAL.ESTIMADA$P.90 <- EST.POP$P.90+(((EST.POP$P.95-EST.POP$P.90)/5)*0)

Murders.est <- subset(Murders.est, MUN != "MUN")
Murders.est <- subset(Murders.est, ENT != "33")

MEstados<- data.frame(Estado = Murders.est$Año)
MEstados$M.13<-Murders.est$X2013
MEstados$M.12<-Murders.est$X2012
MEstados$M.11<-Murders.est$X2011
MEstados$M.10<-Murders.est$X2010
MEstados$M.09<-Murders.est$X2009
MEstados$M.08<-Murders.est$X2008
MEstados$M.07<-Murders.est$X2007
MEstados$M.06<-Murders.est$X2006
MEstados$M.05<-Murders.est$X2005
MEstados$M.04<-Murders.est$X2004
MEstados$M.03<-Murders.est$X2003
MEstados$M.02<-Murders.est$X2002
MEstados$M.01<-Murders.est$X2001
MEstados$M.00<-Murders.est$X2000
MEstados$M.99<-Murders.est$X1999
MEstados$M.98<-Murders.est$X1998
MEstados$M.97<-Murders.est$X1997
MEstados$M.96<-Murders.est$X1996
MEstados$M.95<-Murders.est$X1995
MEstados$M.94<-Murders.est$X1994
MEstados$M.93<-Murders.est$X1993
MEstados$M.92<-Murders.est$X1992
MEstados$M.91<-Murders.est$X1991
MEstados$M.90<-Murders.est$X1990
#MEstados<-gsub(",","",MEstados)

MREstados<- data.frame(Estado = MEstados$Estado)
MREstados$ENT<-POP.INEGI.ESTATAL.ESTIMADA$ENT
MREstados$R.13<-(as.numeric(as.character(gsub(",","",MEstados$M.13)))/POP.INEGI.ESTATAL.ESTIMADA$P.13)*100000
MREstados$R.12<-(as.numeric(as.character(gsub(",","",MEstados$M.12)))/POP.INEGI.ESTATAL.ESTIMADA$P.12)*100000
MREstados$R.11<-(as.numeric(as.character(gsub(",","",MEstados$M.11)))/POP.INEGI.ESTATAL.ESTIMADA$P.11)*100000
MREstados$R.10<-(as.numeric(as.character(gsub(",","",MEstados$M.10)))/POP.INEGI.ESTATAL.ESTIMADA$P.10)*100000
MREstados$R.09<-(as.numeric(as.character(gsub(",","",MEstados$M.09)))/POP.INEGI.ESTATAL.ESTIMADA$P.09)*100000
MREstados$R.08<-(as.numeric(as.character(gsub(",","",MEstados$M.08)))/POP.INEGI.ESTATAL.ESTIMADA$P.08)*100000
MREstados$R.07<-(as.numeric(as.character(gsub(",","",MEstados$M.07)))/POP.INEGI.ESTATAL.ESTIMADA$P.07)*100000
MREstados$R.06<-(as.numeric(as.character(gsub(",","",MEstados$M.06)))/POP.INEGI.ESTATAL.ESTIMADA$P.06)*100000
MREstados$R.05<-(as.numeric(as.character(gsub(",","",MEstados$M.05)))/POP.INEGI.ESTATAL.ESTIMADA$P.05)*100000
MREstados$R.04<-(as.numeric(as.character(gsub(",","",MEstados$M.04)))/POP.INEGI.ESTATAL.ESTIMADA$P.04)*100000
MREstados$R.03<-(as.numeric(as.character(gsub(",","",MEstados$M.03)))/POP.INEGI.ESTATAL.ESTIMADA$P.03)*100000
MREstados$R.02<-(as.numeric(as.character(gsub(",","",MEstados$M.02)))/POP.INEGI.ESTATAL.ESTIMADA$P.02)*100000
MREstados$R.01<-(as.numeric(as.character(gsub(",","",MEstados$M.01)))/POP.INEGI.ESTATAL.ESTIMADA$P.01)*100000
MREstados$R.00<-(as.numeric(as.character(gsub(",","",MEstados$M.00)))/POP.INEGI.ESTATAL.ESTIMADA$P.00)*100000
MREstados$R.99<-(as.numeric(as.character(gsub(",","",MEstados$M.99)))/POP.INEGI.ESTATAL.ESTIMADA$P.99)*100000
MREstados$R.98<-(as.numeric(as.character(gsub(",","",MEstados$M.98)))/POP.INEGI.ESTATAL.ESTIMADA$P.98)*100000
MREstados$R.97<-(as.numeric(as.character(gsub(",","",MEstados$M.97)))/POP.INEGI.ESTATAL.ESTIMADA$P.97)*100000
MREstados$R.96<-(as.numeric(as.character(gsub(",","",MEstados$M.96)))/POP.INEGI.ESTATAL.ESTIMADA$P.96)*100000
MREstados$R.95<-(as.numeric(as.character(gsub(",","",MEstados$M.95)))/POP.INEGI.ESTATAL.ESTIMADA$P.95)*100000
MREstados$R.94<-(as.numeric(as.character(gsub(",","",MEstados$M.94)))/POP.INEGI.ESTATAL.ESTIMADA$P.94)*100000
MREstados$R.93<-(as.numeric(as.character(gsub(",","",MEstados$M.93)))/POP.INEGI.ESTATAL.ESTIMADA$P.93)*100000
MREstados$R.92<-(as.numeric(as.character(gsub(",","",MEstados$M.92)))/POP.INEGI.ESTATAL.ESTIMADA$P.92)*100000
MREstados$R.91<-(as.numeric(as.character(gsub(",","",MEstados$M.91)))/POP.INEGI.ESTATAL.ESTIMADA$P.91)*100000
MREstados$R.90<-(as.numeric(as.character(gsub(",","",MEstados$M.90)))/POP.INEGI.ESTATAL.ESTIMADA$P.90)*100000



##Align estate data with INEGI Shapefile###################################################################
mexico_estatal<-readOGR("./Shapefiles/INEGI/mge2010v5_0a", 
                        layer="Entidades_2010_5A")

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER13<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.13
  } else {
    MRER13<-c(MRER13, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.13)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER12<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.12
  } else {
    MRER12<-c(MRER12, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.12)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER11<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.11
  } else {
    MRER11<-c(MRER11, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.11)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER10b<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.10
  } else {
    MRER10b<-c(MRER10b, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.10)
  }
}

MRER10<- data.frame(mexico_estatal$CVE_ENT)
MRER10$R.13 <- MRER13
MRER10$R.12 <- MRER12
MRER10$R.11 <- MRER11
MRER10$R.10 <- MRER10b


mexico_estatal<-readOGR(".//Shapefiles//INEGI//mge2000", 
                        layer="Entidades_2000")

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER09<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.09
  } else {
    MRER09<-c(MRER09, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.09)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER08<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.08
  } else {
    MRER08<-c(MRER08, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.08)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER07<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.07
  } else {
    MRER07<-c(MRER07, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.07)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER06<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.06
  } else {
    MRER06<-c(MRER06, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.06)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER05<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.05
  } else {
    MRER05<-c(MRER05, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.05)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER04<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.04
  } else {
    MRER04<-c(MRER04, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.04)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER03<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.03
  } else {
    MRER03<-c(MRER03, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.03)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER02<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.02
  } else {
    MRER02<-c(MRER02, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.02)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER01<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.01
  } else {
    MRER01<-c(MRER01, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.01)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER00b<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.00
  } else {
    MRER00b<-c(MRER00b, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.00)
  }
}

MRER00<- data.frame(mexico_estatal$CVE_ENT)
MRER00$R.09 <- MRER09
MRER00$R.08 <- MRER08
MRER00$R.07 <- MRER07
MRER00$R.06 <- MRER06
MRER00$R.05 <- MRER05
MRER00$R.04 <- MRER04
MRER00$R.03 <- MRER03
MRER00$R.02 <- MRER02
MRER00$R.01 <- MRER01
MRER00$R.00 <- MRER00b

mexico_estatal<-readOGR(".//Shapefiles//INEGI//mge1995", 
                        layer="Entidades_1995")

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER99<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.99
  } else {
    MRER99<-c(MRER99, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.99)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER98<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.98
  } else {
    MRER98<-c(MRER98, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.98)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER97<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.97
  } else {
    MRER97<-c(MRER97, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.97)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER96<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.96
  } else {
    MRER96<-c(MRER96, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.96)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER95b<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.95
  } else {
    MRER95b<-c(MRER95b, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.95)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER94<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.94
  } else {
    MRER94<-c(MRER94, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.94)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER93<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.93
  } else {
    MRER93<-c(MRER93, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.93)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER92<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.92
  } else {
    MRER92<-c(MRER92, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.92)
  }
}

for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER91<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.91
  } else {
    MRER91<-c(MRER91, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.91)
  }
}


for(i in 1:length(mexico_estatal$CVE_ENT)) {
  if(i==1) {
    MRER90<-subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.90
  } else {
    MRER90<-c(MRER90, subset(MREstados, MREstados$ENT == as.numeric(mexico_estatal$CVE_ENT[i]))$R.90)
  }
}

MRER95<- data.frame(mexico_estatal$CVE_ENT)
MRER95$R.99 <- MRER99
MRER95$R.98 <- MRER98
MRER95$R.97 <- MRER97
MRER95$R.96 <- MRER96
MRER95$R.95 <- MRER95b
MRER95$R.94 <- MRER94
MRER95$R.93 <- MRER93
MRER95$R.92 <- MRER92
MRER95$R.91 <- MRER91
MRER95$R.90 <- MRER90

write.csv(MRER10, "Secondary_data/MREstados10.csv", row.names = TRUE)
write.csv(MRER00, "Secondary_data/MREstados00.csv", row.names = TRUE)
write.csv(MRER95, "Secondary_data/MREstados95.csv", row.names = TRUE)

POP <- c(sum(POP.INEGI.ESTATAL.ESTIMADA$P.13),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.12),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.11),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.10),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.09),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.08),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.07),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.06),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.05),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.04),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.03),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.02),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.01),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.00),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.99),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.98),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.97),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.96),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.95),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.94),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.93),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.92),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.91),
         sum(POP.INEGI.ESTATAL.ESTIMADA$P.90))

Homicidios.Yr$POP <- POP

RATE <- c(as.numeric(as.character(gsub(",","",Homicidios.Yr$Homicidios)))/Homicidios.Yr$POP)*100000

write.csv(Homicidios.Yr, "Secondary_data/MR_Anual.csv", row.names = TRUE)


Homicidios.Yr$RATE <- RATE
