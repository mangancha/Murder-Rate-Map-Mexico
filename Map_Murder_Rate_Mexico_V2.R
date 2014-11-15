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
MR00<-read.csv("./Secondary_data/MR00.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MR10<-read.csv("./Secondary_data/MR10.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MR95<-read.csv("./Secondary_data/MR95.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")

MR00[MR00==100000]<--1
MR10[MR10==100000]<--1
MR95[MR95==100000]<--1
MR00[MR00==-0.001]<--1
MR10[MR10==-0.001]<--1
MR95[MR95==-0.001]<--1

mexico10<-readOGR("./Shapefiles/INEGI/mgm2010v5_0a", 
                layer="Municipios_2010_5A")
#mexico10<-subset(mexico10, !is.na(mexico10$NOMBRE))
for(i in 1:length(mexico10$CVE_ENT)) {
  if(i==1) {
    CVEGEO10 <- "01001"
  } else {
    CVEGEO10 <- c(CVEGEO10, paste(mexico10$CVE_ENT[i], mexico10$CVE_MUN[i], sep=""))
  }
}
mexico10$CVEGEO <- CVEGEO10

MREstados10<-read.csv("./Secondary_data/MREstados10.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MREstados00<-read.csv("./Secondary_data/MREstados00.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")
MREstados95<-read.csv("./Secondary_data/MREstados95.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")


Yr<-read.csv("./Secondary_data/MR_Anual.csv", fileEncoding = "windows-1252", header = TRUE, sep = ",")


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
    t(as.numeric(MREstados95$R.99,2)),
    t(as.numeric(MREstados95$R.98,2)),
    t(as.numeric(MREstados95$R.97,2)),
    t(as.numeric(MREstados95$R.96,2)),
    t(as.numeric(MREstados95$R.95,2)),
    t(as.numeric(MREstados95$R.94,2)),
    t(as.numeric(MREstados95$R.93,2)),
    t(as.numeric(MREstados95$R.92,2)),
    t(as.numeric(MREstados95$R.91,2)),
    t(as.numeric(MREstados95$R.90,2))
))


nclr <- 9
plotclr <- brewer.pal(nclr,"Reds") 
fillRed <- colorRampPalette( plotclr)
Col<- fillRed(length(levels(unique(H))))
ColE<- fillRed(length(levels(unique(HE))))


##2013#################################################################################
MR13<-as.factor(t(as.numeric(MR10$MR13,2)))
mexico10$MR13 <- MR13
num.col=length(levels(mexico10$MR13))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR13)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
    a<-i
  } else {
    exp<-as.character(sort(unique(MR13)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=c(col, Col[color])
    if(length(color>1)) {color<-color[1]}
    a<-c(a, i)
  }
}
col<-c("gray", col)
mexico_estatal<-readOGR("./Shapefiles/INEGI/mge2010v5_0a", 
                        layer="Entidades_2010_5A")
top.label<-as.character(sort(unique(mexico10$MR13)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico10$MR13)
Q3.pos<-sort(unique(mexico10$MR13))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico10$MR13))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico10$MR13))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))
state.borders = list("sp.polygons", mexico_estatal, col="gray")

MISSING_Murder_Data_13<-data.frame(Municipio=subset(mexico10, mexico10$MR13==-1)$NOM_MUN)
MISSING_Murder_Data_13$ENTIDAD<-subset(mexico10, mexico10$MR13==-1)$CVEGEO

MX13<-spplot(mexico10, "MR13", 
             col.regions=col, 
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

MRE13<-as.numeric(as.character(MREstados10$R.13))
mexico_estatal$MRE13 <- MRE13
num.cole=length(unique(mexico_estatal$MRE13))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE13)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE13)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE13)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE13)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE13)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE13)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col=c(rgb(red=1,green=0.1,blue=0,alpha=seq((0.001), ((max(MRE12)/max(H.2))), length.out = max(MRE12))))
#col<- fillRed(max(MRE13))

MXE13<-spplot(mexico_estatal, "MRE13", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2013], Yr$RATE[Yr$X==2013], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE13, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX13, newpage = FALSE) 

##2012#################################################################################
MR12<-as.factor(t(as.numeric(MR10$MR12,2)))
mexico10$MR12 <- MR12
num.col=length(levels(mexico10$MR12))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR12)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
    a<-1
  } else {
    exp<-as.character(sort(unique(MR12)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
    a<-a+1
  }
}
col<-c("gray", col)

top.label<-as.character(sort(unique(mexico10$MR12)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico10$MR12)
Q3.pos<-sort(unique(mexico10$MR12))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico10$MR12))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico10$MR12))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX12<-spplot(mexico10, "MR12", 
       col.regions=col, 
       main = "2012 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
       par.settings = list(axis.line = list(col = 0)),
       colorkey=list(
         labels=list(
           at = labelat,
           labels = labeltext
           )
         ), 
       axes=F, col="transparent", 
       sp.layout = list(state.borders)) 


MRE12<-as.numeric(as.character(MREstados10$R.12))
mexico_estatal$MRE12 <- MRE12
num.cole=length(unique(mexico_estatal$MRE12))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE12)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE12)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE12)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE12)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE12)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE12)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col=c(rgb(red=1,green=0.1,blue=0,alpha=seq((0.001), ((max(MRE12)/max(H.2))), length.out = max(MRE12))))
#col<- fillRed(max(MRE12))

MXE12<-spplot(mexico_estatal, "MRE12", 
             col.regions=cole, cex.main = 0.3,
             main = "State Means",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               space = "left",
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2012], Yr$RATE[Yr$X==2012], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE12, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX12, newpage = FALSE) 


##2011#################################################################################
MR11<-as.factor(t(as.numeric(MR10$MR11,2)))
mexico10$MR11 <- MR11
num.col=length(levels(mexico10$MR11))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR11)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR11)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico10$MR11)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico10$MR11)
Q3.pos<-sort(unique(mexico10$MR11))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico10$MR11))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico10$MR11))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX11<-spplot(mexico10, "MR11", 
             col.regions=col, 
             main = "2011 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE11<-as.numeric(as.character(MREstados10$R.11))
mexico_estatal$MRE11 <- MRE11
num.cole=length(unique(mexico_estatal$MRE11))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE11)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE11)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE11)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE11)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE11)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE11)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE11))

MXE11<-spplot(mexico_estatal, "MRE11", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2011], Yr$RATE[Yr$X==2011], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE11, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX11, newpage = FALSE) 

##2010#################################################################################
MR10b<-as.factor(t(as.numeric(MR10$MR10,2)))
mexico10$MR10 <- MR10b
num.col=length(levels(mexico10$MR10))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR10b)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR10b)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico10$MR10)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico10$MR10)
Q3.pos<-sort(unique(mexico10$MR10))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico10$MR10))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico10$MR10))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX10<-spplot(mexico10, "MR10", 
             col.regions=col, 
             main = "2010 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE10<-as.numeric(as.character(MREstados10$R.10))
mexico_estatal$MRE10 <- MRE10
num.cole=length(unique(mexico_estatal$MRE10))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE10)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE10)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE10)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE10)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE10)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE10)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE10))

MXE10<-spplot(mexico_estatal, "MRE10", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2010], Yr$RATE[Yr$X==2010], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE10, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX10, newpage = FALSE) 


##2009#################################################################################
mexico00<-readOGR(".//Shapefiles//INEGI//mgm2000", 
                  layer="Municipios_2000")
mexico_estatal<-readOGR(".//Shapefiles//INEGI//mge2000", 
                  layer="Entidades_2000")

MR09<-as.factor(t(as.numeric(MR00$MR09,2)))
mexico00$MR09 <- MR09
num.col=length(levels(mexico00$MR09))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR09)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR09)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR09)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR09)
Q3.pos<-sort(unique(mexico00$MR09))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR09))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR09))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX09<-spplot(mexico00, "MR09", 
             col.regions=col, 
             main = "2009 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE09<-as.numeric(as.character(MREstados00$R.09))
mexico_estatal$MRE09 <- MRE09
num.cole=length(unique(mexico_estatal$MRE09))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE09)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE09)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE09)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE09)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE09)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE09)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))

#col<- fillRed(max(MRE09))

MXE09<-spplot(mexico_estatal, "MRE09", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2009], Yr$RATE[Yr$X==2009], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE09, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX09, newpage = FALSE) 

##2008#################################################################################
MR08<-as.factor(t(as.numeric(MR00$MR08,2)))
mexico00$MR08 <- MR08
num.col=length(levels(mexico00$MR08))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR08)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR08)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR08)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR08)
Q3.pos<-sort(unique(mexico00$MR08))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR08))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR08))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX08<-spplot(mexico00, "MR08", 
             col.regions=col, 
             main = "2008 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE08<-as.numeric(as.character(MREstados00$R.08))
mexico_estatal$MRE08 <- MRE08
num.cole=length(unique(mexico_estatal$MRE08))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE08)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE08)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE08)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE08)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE08)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE08)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE08))

MXE08<-spplot(mexico_estatal, "MRE08", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2008], Yr$RATE[Yr$X==2008], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE08, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX08, newpage = FALSE) 

##2007#################################################################################
MR07<-as.factor(t(as.numeric(MR00$MR07,2)))
mexico00$MR07 <- MR07
num.col=length(levels(mexico00$MR07))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR07)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR07)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR07)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR07)
Q3.pos<-sort(unique(mexico00$MR07))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR07))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR07))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX07<-spplot(mexico00, "MR07", 
             col.regions=col, 
             main = "2007 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE07<-as.numeric(as.character(MREstados00$R.07))
mexico_estatal$MRE07 <- MRE07
num.cole=length(unique(mexico_estatal$MRE07))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE07)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE07)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE07)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE07)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE07)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE07)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE07))

MXE07<-spplot(mexico_estatal, "MRE07", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2007], Yr$RATE[Yr$X==2007], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE07, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX07, newpage = FALSE) 

##2006#################################################################################
MR06<-as.factor(t(as.numeric(MR00$MR06,2)))
mexico00$MR06 <- MR06
num.col=length(levels(mexico00$MR06))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR06)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR06)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR06)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR06)
Q3.pos<-sort(unique(mexico00$MR06))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR06))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR06))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX06<-spplot(mexico00, "MR06", 
             col.regions=col, 
             main = "2006 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE06<-as.numeric(as.character(MREstados00$R.06))
mexico_estatal$MRE06 <- MRE06
num.cole=length(unique(mexico_estatal$MRE06))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE06)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE06)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE06)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE06)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE06)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE06)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE06<-spplot(mexico_estatal, "MRE06", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2006], Yr$RATE[Yr$X==2006], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE06, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX06, newpage = FALSE) 

##2005#################################################################################
MR05<-as.factor(t(as.numeric(MR00$MR05,2)))
mexico00$MR05 <- MR05
num.col=length(levels(mexico00$MR05))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR05)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR05)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR05)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR05)
Q3.pos<-sort(unique(mexico00$MR05))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR05))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR05))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX05<-spplot(mexico00, "MR05", 
             col.regions=col, 
             main = "2005 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE05<-as.numeric(as.character(MREstados00$R.05))
mexico_estatal$MRE05 <- MRE05
num.cole=length(unique(mexico_estatal$MRE05))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE05)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE05)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE05)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE05)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE05)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE05)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE05<-spplot(mexico_estatal, "MRE05", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2005], Yr$RATE[Yr$X==2005], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE05, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX05, newpage = FALSE) 

##2004#################################################################################
MR04<-as.factor(t(as.numeric(MR00$MR04,2)))
mexico00$MR04 <- MR04
num.col=length(levels(mexico00$MR04))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR04)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR04)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR04)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR04)
Q3.pos<-sort(unique(mexico00$MR04))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR04))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR04))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0)
Q2.label<-round(as.numeric(as.character(Q2.pos)),0)
Q1.label<-round(as.numeric(as.character(Q1.pos)),0)
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX04<-spplot(mexico00, "MR04", 
             col.regions=col, 
             main = "2004 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE04<-as.numeric(as.character(MREstados00$R.04))
mexico_estatal$MRE04 <- MRE04
num.cole=length(unique(mexico_estatal$MRE04))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE04)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE04)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE04)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE04)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE04)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE04)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE04<-spplot(mexico_estatal, "MRE04", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2004], Yr$RATE[Yr$X==2004], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE04, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX04, newpage = FALSE) 

##2003#################################################################################
MR03<-as.factor(t(as.numeric(MR00$MR03,2)))
mexico00$MR03 <- MR03
num.col=length(levels(mexico00$MR03))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR03)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR03)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR03)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR03)
Q3.pos<-sort(unique(mexico00$MR03))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR03))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR03))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX03<-spplot(mexico00, "MR03", 
             col.regions=col, 
             main = "2003 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE03<-as.numeric(as.character(MREstados00$R.03))
mexico_estatal$MRE03 <- MRE03
num.cole=length(unique(mexico_estatal$MRE03))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE03)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE03)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE03)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE03)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE03)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE03)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE03<-spplot(mexico_estatal, "MRE03", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2003], Yr$RATE[Yr$X==2003], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE03, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX03, newpage = FALSE) 

##2002#################################################################################
MR02<-as.factor(t(as.numeric(MR00$MR02,2)))
mexico00$MR02 <- MR02
num.col=length(levels(mexico00$MR02))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR02)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR02)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR02)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR02)
Q3.pos<-sort(unique(mexico00$MR02))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR02))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR02))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX02<-spplot(mexico00, "MR02", 
             col.regions=col, 
             main = "2002 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE02<-as.numeric(as.character(MREstados00$R.02))
mexico_estatal$MRE02 <- MRE02
num.cole=length(unique(mexico_estatal$MRE02))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE02)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE02)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE02)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE02)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE02)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE02)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE02<-spplot(mexico_estatal, "MRE02", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2002], Yr$RATE[Yr$X==2002], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE02, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX02, newpage = FALSE) 

##2001#################################################################################
MR01<-as.factor(t(as.numeric(MR00$MR01,2)))
mexico00$MR01 <- MR01
num.col=length(levels(mexico00$MR01))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR01)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR01)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR01)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR01)
Q3.pos<-sort(unique(mexico00$MR01))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR01))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR01))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX01<-spplot(mexico00, "MR01", 
             col.regions=col, 
             main = "2001 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE01<-as.numeric(as.character(MREstados00$R.01))
mexico_estatal$MRE01 <- MRE01
num.cole=length(unique(mexico_estatal$MRE01))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE01)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE01)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE01)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE01)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE01)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE01)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE01<-spplot(mexico_estatal, "MRE01", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2001], Yr$RATE[Yr$X==2001], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE01, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX01, newpage = FALSE) 

##2000#################################################################################
MR00b<-as.factor(t(as.numeric(MR00$MR00,2)))
mexico00$MR00 <- MR00b
num.col=length(levels(mexico00$MR00))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR00b)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR00b)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico00$MR00)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico00$MR00)
Q3.pos<-sort(unique(mexico00$MR00))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico00$MR00))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico00$MR00))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX00<-spplot(mexico00, "MR00", 
             col.regions=col, 
             main = "2000 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE00<-as.numeric(as.character(MREstados00$R.00))
mexico_estatal$MRE00 <- MRE00
num.cole=length(unique(mexico_estatal$MRE00))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE00)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE00)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE00)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE00)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE00)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE00)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE00<-spplot(mexico_estatal, "MRE00", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==2000], Yr$RATE[Yr$X==2000], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE00, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX00, newpage = FALSE) 

##1999#################################################################################
mexico95<-readOGR(".//Shapefiles//INEGI//mgm1995", 
                  layer="Municipios_1995")

mexico_estatal<-readOGR(".//Shapefiles//INEGI//mge1995", 
                        layer="Entidades_1995")

MR99<-as.factor(t(as.numeric(MR95$MR99,2)))
mexico95$MR99 <- MR99
num.col=length(levels(mexico95$MR99))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR99)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR99)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR99)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR99)
Q3.pos<-sort(unique(mexico95$MR99))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR99))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR99))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX99<-spplot(mexico95, "MR99", 
             col.regions=col, 
             main = "1999 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE99<-as.numeric(as.character(MREstados95$R.99))
mexico_estatal$MRE99 <- MRE99
num.cole=length(unique(mexico_estatal$MRE99))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE99)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE99)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE99)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE99)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE99)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE99)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE99<-spplot(mexico_estatal, "MRE99", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1999], Yr$RATE[Yr$X==1999], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE99, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX99, newpage = FALSE) 

##1998#################################################################################
MR98<-as.factor(t(as.numeric(MR95$MR98,2)))
mexico95$MR98 <- MR98
num.col=length(levels(mexico95$MR98))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR98)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR98)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR98)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR98)
Q3.pos<-sort(unique(mexico95$MR98))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR98))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR98))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX98<-spplot(mexico95, "MR98", 
             col.regions=col, 
             main = "1998 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE98<-as.numeric(as.character(MREstados95$R.98))
mexico_estatal$MRE98 <- MRE98
num.cole=length(unique(mexico_estatal$MRE98))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE98)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE98)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE98)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE98)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE98)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE98)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE98<-spplot(mexico_estatal, "MRE98", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1998], Yr$RATE[Yr$X==1998], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE98, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX98, newpage = FALSE) 

##1997#################################################################################
MR97<-as.factor(t(as.numeric(MR95$MR97,2)))
mexico95$MR97 <- MR97
num.col=length(levels(mexico95$MR97))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR97)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR97)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR97)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR97)
Q3.pos<-sort(unique(mexico95$MR97))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR97))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR97))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX97<-spplot(mexico95, "MR97", 
             col.regions=col, 
             main = "1997 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE97<-as.numeric(as.character(MREstados95$R.97))
mexico_estatal$MRE97 <- MRE97
num.cole=length(unique(mexico_estatal$MRE97))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE97)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE97)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE97)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE97)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE97)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE97)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE97<-spplot(mexico_estatal, "MRE97", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1997], Yr$RATE[Yr$X==1997], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE97, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX97, newpage = FALSE) 

##1996#################################################################################
MR96<-as.factor(t(as.numeric(MR95$MR96,2)))
mexico95$MR96 <- MR96
num.col=length(levels(mexico95$MR96))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR96)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR96)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR96)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR96)
Q3.pos<-sort(unique(mexico95$MR96))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR96))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR96))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX96<-spplot(mexico95, "MR96", 
             col.regions=col, 
             main = "1996 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE96<-as.numeric(as.character(MREstados95$R.96))
mexico_estatal$MRE96 <- MRE96
num.cole=length(unique(mexico_estatal$MRE96))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE96)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE96)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE96)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE96)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE96)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE96)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE96<-spplot(mexico_estatal, "MRE96", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1996], Yr$RATE[Yr$X==1996], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE96, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX96, newpage = FALSE) 

##1995#################################################################################
MR95b<-as.factor(t(as.numeric(MR95$MR95,2)))
mexico95$MR95 <- MR95b
num.col=length(levels(mexico95$MR95))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR95b)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR95b)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR95)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR95)
Q3.pos<-sort(unique(mexico95$MR95))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR95))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR95))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX95<-spplot(mexico95, "MR95b", 
             col.regions=col, 
             main = "1995 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE95<-as.numeric(as.character(MREstados95$R.95))
mexico_estatal$MRE95 <- MRE95
num.cole=length(unique(mexico_estatal$MRE95))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE95)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE95)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE95)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE95)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE95)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE95)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE95<-spplot(mexico_estatal, "MRE95", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1995], Yr$RATE[Yr$X==1995], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE95, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX95, newpage = FALSE) 

##1994#################################################################################
MR94<-as.factor(t(as.numeric(MR95$MR94,2)))
mexico95$MR94 <- MR94
num.col=length(levels(mexico95$MR94))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR94)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR94)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR94)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR94)
Q3.pos<-sort(unique(mexico95$MR94))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR94))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR94))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX94<-spplot(mexico95, "MR94", 
             col.regions=col, 
             main = "1994 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE94<-as.numeric(as.character(MREstados95$R.94))
mexico_estatal$MRE94 <- MRE94
num.cole=length(unique(mexico_estatal$MRE94))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE94)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE94)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE94)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE94)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE94)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE94)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE94<-spplot(mexico_estatal, "MRE94", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1994], Yr$RATE[Yr$X==1994], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE94, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX94, newpage = FALSE) 

##1993#################################################################################
MR93<-as.factor(t(as.numeric(MR95$MR93,2)))
mexico95$MR93 <- MR93
num.col=length(levels(mexico95$MR93))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR93)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR93)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR93)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR93)
Q3.pos<-sort(unique(mexico95$MR93))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR93))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR93))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX93<-spplot(mexico95, "MR93", 
             col.regions=col, 
             main = "1993 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE93<-as.numeric(as.character(MREstados95$R.93))
mexico_estatal$MRE93 <- MRE93
num.cole=length(unique(mexico_estatal$MRE93))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE93)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE93)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE93)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE93)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE93)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE93)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE93<-spplot(mexico_estatal, "MRE93", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1993], Yr$RATE[Yr$X==1993], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE93, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX93, newpage = FALSE) 

##1992#################################################################################
MR92<-as.factor(t(as.numeric(MR95$MR92,2)))
mexico95$MR92 <- MR92
num.col=length(levels(mexico95$MR92))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR92)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR92)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR92)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR92)
Q3.pos<-sort(unique(mexico95$MR92))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR92))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR92))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX92<-spplot(mexico95, "MR92", 
             col.regions=col, 
             main = "1992 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE92<-as.numeric(as.character(MREstados95$R.92))
mexico_estatal$MRE92 <- MRE92
num.cole=length(unique(mexico_estatal$MRE92))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE92)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE92)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE92)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE92)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE92)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE92)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE92<-spplot(mexico_estatal, "MRE92", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1992], Yr$RATE[Yr$X==1992], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE92, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX92, newpage = FALSE) 

##1991#################################################################################
MR91<-as.factor(t(as.numeric(MR95$MR91,2)))
mexico95$MR91 <- MR91
num.col=length(levels(mexico95$MR91))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR91)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR91)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR91)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR91)
Q3.pos<-sort(unique(mexico95$MR91))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR91))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR91))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX91<-spplot(mexico95, "MR91", 
             col.regions=col, 
             main = "1991 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE91<-as.numeric(as.character(MREstados95$R.91))
mexico_estatal$MRE91 <- MRE91
num.cole=length(unique(mexico_estatal$MRE91))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE91)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE91)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE91)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE91)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE91)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE91)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE91<-spplot(mexico_estatal, "MRE91", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1991], Yr$RATE[Yr$X==1991], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE91, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX91, newpage = FALSE) 

##1990#################################################################################
MR90<-as.factor(t(as.numeric(MR95$MR90,2)))
mexico95$MR90 <- MR90
num.col=length(levels(mexico95$MR90))
for ( i in 2:num.col) {
  if ( i == 2 ) {
    exp<-as.character(sort(unique(MR90)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    col=Col[color]
  } else {
    exp<-as.character(sort(unique(MR90)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(H)))
    if(length(color>1)) {color<-color[1]}
    col=c(col, Col[color])
  }
}
col<-c("gray", col)
top.label<-as.character(sort(unique(mexico95$MR90)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-nlevels(mexico95$MR90)
Q3.pos<-sort(unique(mexico95$MR90))[round((top.pos/4)*3,0)]
Q2.pos<-sort(unique(mexico95$MR90))[round((top.pos/4)*2,0)]
Q1.pos<-sort(unique(mexico95$MR90))[round((top.pos/4)*1,0)]
Q3.label<-round(as.numeric(as.character(Q3.pos)),0) 
Q2.label<-round(as.numeric(as.character(Q2.pos)),0) 
Q1.label<-round(as.numeric(as.character(Q1.pos)),0) 
labelat = c(as.numeric(Q1.pos), as.numeric(Q2.pos), as.numeric(Q3.pos), top.pos)
labeltext =  as.character(c(Q1.label, Q2.label, Q3.label, top.label))

state.borders = list("sp.polygons", mexico_estatal, col="gray")
MX90<-spplot(mexico95, "MR90", 
             col.regions=col, 
             main = "1990 - Murder Rate per 100,000 inhabitants \n(Municipalty Level)",
             par.settings = list(axis.line = list(col = 0)),
             colorkey=list(
               labels=list(
                 at = labelat,
                 labels = labeltext
               )
             ), 
             axes=F, col="transparent", 
             sp.layout = list(state.borders)) 

MRE90<-as.numeric(as.character(MREstados95$R.90))
mexico_estatal$MRE90 <- MRE90
num.cole=length(unique(mexico_estatal$MRE90))
for ( i in 1:num.cole) {
  if ( i == 1 ) {
    exp<-as.character(sort(unique(MRE90)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=ColE[color]
  } else {
    exp<-as.character(sort(unique(MRE90)))[i]
    exp<-paste("^", exp, "$", sep="")
    color=grep(exp,sort(unique(HE)))
    cole=c(cole, ColE[color])
  }
}

top.label<-as.character(sort(unique(mexico_estatal$MRE90)))
top.label<-round(max(as.numeric(top.label)),0)
top.pos<-top.label
Q3.pos<- (top.label/4)*3
Q2.pos<- (top.label/4)*2
Q1.pos<- (top.label/4)*1
Q3.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE90)))[round((32/4)*3,0)]))
Q2.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE90)))[round((32/4)*2,0)]))
Q1.label<-as.numeric(as.character(as.factor(sort(unique(mexico_estatal$MRE90)))[round((32/4)*1,0)]))
labelat = c(round(as.numeric(as.character(Q1.pos)),0),
            round(as.numeric(as.character(Q2.pos)),0),
            round(as.numeric(as.character(Q3.pos)),0),
            top.pos)
labeltext =  as.character(c(round(Q1.label,0), round(Q2.label,0), round(Q3.label,0), top.pos))


#col<- fillRed(max(MRE06))

MXE90<-spplot(mexico_estatal, "MRE90", 
              col.regions=cole, cex.main = 0.3,
              main = "State Means",
              par.settings = list(axis.line = list(col = 0)),
              colorkey=list(
                space = "left",
                labels=list(
                  at = labelat,
                  labels = labeltext
                )
              ), 
              axes=F, col="transparent") 

grid.newpage()
plot.new()
vp1 <- viewport(x = 1, y = 0, 
                height = 0.3, width = 0.5,
                just = c("right", "bottom"),
                name = "lower right")
vp2 <- viewport(x = 0, y = 0, 
                height = 0.3, width = 0.5,
                just = c("left", "bottom"),
                name = "lower left")
vp3 <- viewport(x = 0, y = 0.1, 
                height = 1, width = 1,
                just = c("left", "bottom"),
                name = "centre")
pushViewport(vp1)
par(new=TRUE, mai=c(0.5,0.5,0.5,0.5), fig=gridFIG())
plot(Yr$X, Yr$RATE, type = "l", xaxt = 'n', yaxt = 'n', pch=20, col="black", frame.plot=F, cex.main = 0.8, font.main = 2, main = "National Mean")
points(Yr$X[Yr$X==1990], Yr$RATE[Yr$X==1990], pch=20, col="red", cex=1)
axis(side = "1", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("1985", "1990", "1995", "2000", "2005", "2010", "2015"), at = c(1985, 1990, 1995, 2000, 2005, 2010, 2015))
axis(side = "2", cex.axis = 0.5, hadj= 0, tck=0, las = 2, labels = c("5", "10", "15", "20", "25"), at = c(5, 10, 15, 20, 25))
upViewport()
pushViewport(vp2)
par(new=TRUE, fig=gridFIG())
print(MXE90, newpage = FALSE) 
upViewport()
pushViewport(vp3)
par(new=TRUE)
print(MX90, newpage = FALSE) 