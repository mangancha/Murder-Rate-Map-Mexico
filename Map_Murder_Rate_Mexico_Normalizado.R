##Required Libraries#################################################################################

library(sp) # For spplot
library(colorRamps)
library(rgdal) # To import shapefiles
library(RColorBrewer)
library(latticeExtra)
#library(gridExtra)
library(grid)
library(gridBase)


##Load Data#################################################################################
#load("./Secondary_data/MR.RData")
#MR<-read.csv("./Secondary_data/MR.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MR00<-read.csv("./Secondary_data/MR00.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MR10<-read.csv("./Secondary_data/MR10.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MR95<-read.csv("./Secondary_data/MR95.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")

MR00[MR00==100000]<--1
MR10[MR10==100000]<--1
MR95[MR95==100000]<--1
MR00[MR00==-0.001]<--1
MR10[MR10==-0.001]<--1
MR95[MR95==-0.001]<--1

mexico10<-readOGR("./Shapefiles/INEGI/mexico_municipal", 
                  layer="mexico_municipal_SELEC")
mexico10<-subset(mexico10, !is.na(mexico10$NOMBRE))
MREstados10<-read.csv("./Secondary_data/MREstados10.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MREstados00<-read.csv("./Secondary_data/MREstados00.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")

Yr<-read.csv("./Primary_data/141106-Homicidios-Yr.csv", fileEncoding = "windows-1252", header = FALSE, sep = ",")


H<-as.factor(c(t(as.numeric(MR10$MR13,2)), 
               t(as.numeric(MR10$MR12,2)), 
               t(as.numeric(MR10$MR11,2)),
               t(as.numeric(MR10$MR10,2)),
               t(as.numeric(MR00$MR09,2)),
               t(as.numeric(MR00$MR08,2)),
               t(as.numeric(MR00$MR07,2)),
               t(as.numeric(MR00$MR06,2)),
               t(as.numeric(MR00$MR05,2)),
               t(as.numeric(MR00$MR04,2)),
               t(as.numeric(MR00$MR03,2)),
               t(as.numeric(MR00$MR02,2)),
               t(as.numeric(MR00$MR01,2)),
               t(as.numeric(MR00$MR00,2)),
               t(as.numeric(MR95$MR99,2)),
               t(as.numeric(MR95$MR98,2)),
               t(as.numeric(MR95$MR97,2)),
               t(as.numeric(MR95$MR96,2)),
               t(as.numeric(MR95$MR95,2)),
               t(as.numeric(MR95$MR94,2)),
               t(as.numeric(MR95$MR93,2)),
               t(as.numeric(MR95$MR92,2)),
               t(as.numeric(MR95$MR91,2)),
               t(as.numeric(MR95$MR90,2))
))

HE<-as.factor(c(t(as.numeric(MREstados10$R.13,2)), 
                t(as.numeric(MREstados10$R.12,2)), 
                t(as.numeric(MREstados10$R.11,2)),
                t(as.numeric(MREstados10$R.10,2)),
                t(as.numeric(MREstados00$R.09,2)),
                t(as.numeric(MREstados00$R.08,2)),
                t(as.numeric(MREstados00$R.07,2)),
                t(as.numeric(MREstados00$R.06,2)),
                t(as.numeric(MREstados00$R.05,2)),
                t(as.numeric(MREstados00$R.04,2)),
                t(as.numeric(MREstados00$R.03,2)),
                t(as.numeric(MREstados00$R.02,2)),
                t(as.numeric(MREstados00$R.01,2)),
                t(as.numeric(MREstados00$R.00,2)),
                t(as.numeric(MREstados00$R.99,2)),
                t(as.numeric(MREstados00$R.98,2)),
                t(as.numeric(MREstados00$R.97,2)),
                t(as.numeric(MREstados00$R.96,2)),
                t(as.numeric(MREstados00$R.95,2)),
                t(as.numeric(MREstados00$R.94,2)),
                t(as.numeric(MREstados00$R.93,2)),
                t(as.numeric(MREstados00$R.92,2)),
                t(as.numeric(MREstados00$R.91,2)),
                t(as.numeric(MREstados00$R.90,2))
))


nclr <- 3
plotclr <- brewer.pal(nclr,"Reds") 
fillRed <- colorRampPalette(c("white", plotclr))
H.Mod <- as.numeric(as.character(H))^(1/1.5)
H.Mod[is.na(H.Mod)]<--1

#Col.Mod<- fillRed(length(unique(H.Mod)))
Col.Mod = blue2red(length(unique(H.Mod)))
#Col.Mod = rev(heat.colors(length(unique(H.Mod))))
Col.Mod<-Col.Mod[-1]
Col.Mod <- c("gray", Col.Mod)

##2013#################################################################################
mexico_estatal<-readOGR("./Shapefiles/INEGI/mexico_estatal", 
                        layer="mexico_estatal")

MR13.Mod <- as.numeric(as.character(MR13))^(1/1.5)
MR13.Mod[is.na(MR13.Mod)]<--1
mexico10$MR13.Mod <- MR13.Mod

top.label<-as.character(sort(unique(mexico10$MR13.Mod)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico10$MR13.Mod)
Q3.pos<-sort(unique(mexico10$MR13.Mod))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico10$MR13.Mod))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico10$MR13.Mod))[round((top.pos/4)*1,0)]
Q3.label<-as.numeric(as.character(Q3.pos))
Q2.label<-as.numeric(as.character(Q2.pos))
Q1.label<-as.numeric(as.character(Q1.pos))
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))
state.borders = list("sp.polygons", mexico_estatal, col="gray")


num.col.mod=length(unique(mexico10$MR13.Mod))
for ( i in 1:num.col.mod) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR13.Mod)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H.Mod)))
    col.Mod=Col.Mod[color]
    a<-i
  } else {
    exp<-as.character(sort(unique(MR13.Mod)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H.Mod)))
    col.Mod=c(col.Mod, Col.Mod[color])
    a<-c(a, i)
  }
}

spplot(mexico10, "MR13.Mod", 
             col.regions=col.Mod, 
             main = "2013 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 





##2011#################################################################################
MR11.Mod <- as.numeric(as.character(MR11))^(1/1.5)
MR11.Mod[is.na(MR11.Mod)]<--1
mexico10$MR11.Mod <- MR11.Mod

top.label<-as.character(sort(unique(mexico10$MR11.Mod)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico10$MR11.Mod)
Q3.pos<-sort(unique(mexico10$MR11.Mod))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico10$MR11.Mod))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico10$MR11.Mod))[round((top.pos/4)*1,0)]
Q3.label<-as.numeric(as.character(Q3.pos))
Q2.label<-as.numeric(as.character(Q2.pos))
Q1.label<-as.numeric(as.character(Q1.pos))
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))
state.borders = list("sp.polygons", mexico_estatal, col="gray")


num.col.mod=length(unique(mexico10$MR13.Mod))
for ( i in 1:num.col.mod) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MR13.Mod)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H.Mod)))
    col.Mod=Col.Mod[color]
    a<-i
  } else {
    exp<-as.character(sort(unique(MR13.Mod)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H.Mod)))
    col.Mod=c(col.Mod, Col.Mod[color])
    a<-c(a, i)
  }
}

spplot(mexico10, "MR13.Mod", 
       col.regions=col.Mod, 
       main = "2013 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
       par.settings = list(axis.line = list(col = 0)),
       colorkey=list(
         labels=list(
           at = labelat,
           labels = labeltext
         )
       ), 
       axes=F, col="transparent", 
       sp.layout = list(state.borders)) 