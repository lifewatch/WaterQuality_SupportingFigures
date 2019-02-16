######################################
###       Seasonality              ### 
###     Codes by Olivier Beauchard ###
######################################
#### Libraries ####
library(ggplot2)

#### Setup ####
# Set working directory 
# setwd("~/R/FinalScripts_DatapaperWaterquality/Seasonality")

#clean all
rm(list=ls(all=TRUE))

#libraries
require(raster)
require(gplots)
require(RColorBrewer)
require(ade4)
require(maps)
require(mapdata)
require(prettymapr)

### Data importation and cleaning
t=read.csv("data/Fig1-3_Mapping.csv",h=T,quote="")
t=t[-nrow(t),]
colnames(t)
t=t[,-ncol(t)]
t$Code=as.character(t$Code)
t$Code=sub(' +$','',t$Code)
t$Code

### Making year and season factors
t$StartDate=as.POSIXct(strptime(t$StartDate,format="%d-%b-%y",tz="GMT"))
t$year=factor(substr(t$StartDate,1,4))
t$month=factor(substr(t$StartDate,6,7))
t$sea=t$month
levels(t$sea)=rep(1:4,rep(3,4))

### Visualization
xy=read.csv("data/Fig1-3_xy.csv",h=T,row.names=1)
xy=xy[rownames(xy)%in%t$Code,]
xy=xy[order(rownames(xy)),]
t=t[order(t$Code),]
t=t[t$Code%in%rownames(xy),]
t$x=rep(xy$x,table(t$Code))
t$y=rep(xy$y,table(t$Code))
plot(t$x,t$y)
colnames(t)

tab=t[,c(4:9,35)]#[t$year!=2017,]
w1=matrix(0,nrow(xy),4)
for(i in 1:4){
  for(j in 1:length(rownames(xy))){
    w=tab[t$Code==rownames(xy)[j] & t$sea==i & t$year%in%c(2000:2019),]
    data=w[is.na(w)==F]
    w1[j,][i]=length(data)
  }
}

tab=t[,c(10:34)]#[t$year!=2017,]
w2=matrix(0,nrow(xy),4)
for(i in 1:4){
  for(j in 1:length(rownames(xy))){
    w=tab[t$Code==rownames(xy)[j] & t$sea==i & t$year%in%c(2000:2019),]
    data=w[is.na(w)==F]
    w2[j,][i]=length(data)
  }
}

w1=w1/max(w1)
w2=w2/max(w2)

city=c("Duinkerke","Nieuwpoort","Oostende","Zeebrugge","Vlissingen")
xcity=c(2.48,2.75,2.92,3.24,3.58)
ycity=c(51.05,51.15,51.21,51.33,51.46)
p=read.csv("data/Fig1-3_PolyBelgium.csv",h=T)
p=SpatialPoints(p)
p=Polygon(p)
p=Polygons(list(p),ID="1")
p=SpatialPolygons(list(p))
r=raster("data/bat")
values(r)[values(r)>0]=0
values(r)[!is.na(values(r))]=-values(r)[!is.na(values(r))]

pdf("fig/1_Map of the sampling area.pdf",width=15,height=10)
par(mar=c(3,4.5,1,8),xaxs="i",yaxs="i",cex.axis=1.5)
rx=c(1.8,3.8)
ry=c(51.0,52.0)
plot(xy[,1:2],type="n",xlim=rx,ylim=ry,xaxt="n",yaxt="n",xlab="",ylab="")
plot(r,col=colorRampPalette(brewer.pal(9,"Blues"))(100),interpolation=T,
     legend.width=2,legend.shrink=0.6,axis.args=list(cex.axis=1.5),add=T)
#contour(r,levels=c(-25,-60),labcex=1,col="dodgerblue3",add=T)
plot(p,add=T)
map("worldHires",fill=T,col="grey90",lwd=1,resolution=0.5,boundary=F,add=T)
map("worldHires",fill=F,interior=F,col=1,lwd=1,resolution=0.5,add=T)
map.scale(3.08,51.1,0.25,ratio=F)
addnortharrow("topleft",padin=c(0.2,0.2))
axis(side=1,at=seq(from=rx[1]+0.2,to=rx[2]-0.2,by=0.2),tick=F)
axis(side=2,at=seq(from=ry[1]+0.2,to=ry[2]-0.2,by=0.2),tick=F,las=1)
points(xy[,1:2][xy$cat=="ns",],pch=20,col=1,cex=2.8)
points(xy[,1:2][xy$cat=="os",],pch=15,col=1,cex=2.1)
text(xy[,1],xy[,2]+0.01,rownames(xy),pos=3,cex=1.5)
points(xcity,ycity,cex=2.3)
text(xcity,ycity-0.01,city,pos=1,cex=1.5,font=3)
abline(h=seq(from=ry[1]+0.2,to=ry[2]-0.2,by=0.2),lty=1,lwd=0.7,col="gray70")
abline(v=seq(from=rx[1]+0.2,to=rx[2]-0.2,by=0.2),lty=1,lwd=0.7,col="gray70")
box()
mapns=function(x){
  par(bty="n")
  plot(c(-1,7),c(49.8,54),type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  map(x,resolution=0,col=1,mar=par("mar"),add=T)
  rect(rx[1],ry[1],rx[2],ry[2],lty=5,lwd=3)
}
add.scatter(mapns("worldHires"),ratio=0.3,
            inset=c(0.03,0.03),pos="topright",bg.col=NULL)
dev.off()






pdf("fig/3_Seasonal mapping.pdf",width=15,height=6)
m=matrix(1:15,nr=3,nc=5,byrow=T)
layout(m,width=c(0.1,1,1,1,1),height=c(0.1,1,1))
par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
plot.new()
for(i in 1:4){
  plot.new()
  mtext(side=1,line=-1,c("Winter","Spring","Summer","Autumn")[i],cex=1.3)
}
plot.new()
mtext(side=2,line=-2,"Abiotic descriptors",cex=1.3,at=0.55)
par(mar=c(3,3,1,1))
for(i in 1:4){
  plot(xy[,1:2],type="n",xlim=c(2.1,3.3),ylim=c(51.0,51.9),xaxt="n",yaxt="n")
  image(r,legend=F,col=rev(colorRampPalette(rev(brewer.pal(9,"Blues")))(100)),
        add=T)
  map("worldHires",fill=T,col="grey90",lwd=1,resolution=1,boundary=F,add=T)
  map("worldHires",fill=F,interior=F,col=1,lwd=1,resolution=1,add=T)
  mtext(side=1,text=c(seq(from=2.2,to=2.8,by=0.2),"3.0",3.2),
        cex=0.8,at=seq(from=2.2,to=3.2,by=0.2),line=1.2)
  points(xy,pch=20,col=1,cex=15*w1[,i])
  mtext(side=2,text=seq(from=51.2,to=51.8,by=0.2),
        cex=0.8,at=seq(from=51.2,to=51.8,by=0.2),line=0.5,las=1)
  abline(h=seq(from=51.2,to=51.8,by=0.2),lty=1,lwd=0.3,col="gray70")
  abline(v=seq(from=2.2,to=3.2,by=0.2),lty=1,lwd=0.3,col="gray70")
  box()
}
par(mar=c(0,0,0,0))
plot.new()
mtext(side=2,line=-2,"Pigments",cex=1.3,at=0.55)
par(mar=c(3,3,1,1),xaxs="i",yaxs="i")

for(i in 1:4){
  plot(xy[,1:2],type="n",xlim=c(2.1,3.3),ylim=c(51.0,51.9),xaxt="n",yaxt="n")
  image(r,legend=F,col=rev(colorRampPalette(rev(brewer.pal(9,"Blues")))(100)),
        add=T)
  map("worldHires",fill=T,col="grey90",lwd=1,resolution=1,boundary=F,add=T)
  map("worldHires",fill=F,interior=F,col=1,lwd=1,resolution=1,add=T)
  mtext(side=1,text=c(seq(from=2.2,to=2.8,by=0.2),"3.0",3.2),
        cex=0.8,at=seq(from=2.2,to=3.2,by=0.2),line=1.2)
  points(xy,pch=20,col=1,bg=1,cex=15*w2[,i])
  mtext(side=2,text=seq(from=51.2,to=51.8,by=0.2),
        cex=0.8,at=seq(from=51.2,to=51.8,by=0.2),line=0.5,las=1)
  abline(h=seq(from=51.2,to=51.8,by=0.2),lty=1,lwd=0.3,col="gray70")
  abline(v=seq(from=2.2,to=3.2,by=0.2),lty=1,lwd=0.3,col="gray70")
  box()
}
dev.off()

