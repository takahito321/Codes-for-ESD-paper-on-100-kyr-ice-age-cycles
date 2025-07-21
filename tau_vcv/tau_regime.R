library(palinsol)
library(viridis)
#library(Hmisc)
#library(fields)
library(RColorBrewer)

# LR04
dt<-0.05
t<-seq(-1200,0,dt)
n<-length(t)
tab<-read.table("../LR04stack.txt")
z<-approx(-tab[,1],tab[,2],t)$y
z<- 26*z^2-135*z+163

# insolation
tts<-1000*t-50   
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
ins<-ins-495.7
ins<-ins/24

th<-1e-4

# Heun dt above
func<-function(a,b){
    para<-c(0.065,0.005,0.042,2,2,0.21,0.3,12,0.11)
    para[9]<-b*para[9]
    x<-rep(5,n) # S (10^6 km2)
    y<-rep(0,n) # theta (deg C)
    z<-rep(2,n) # omega
    
    for(i in 1:(n-1)){
        fx<-0.8*x[i]^0.75*(para[1]-para[9]*ins[i]-para[2]*z[i]-para[3]*y[i])
        fy<-(para[1]-para[9]*ins[i]-para[2]*z[i])*(para[4]*z[i]+para[5]*(x[i]-para[8])-y[i])/x[i]^0.25
        fz<--para[6]*(x[i]-para[8])-para[7]*z[i]
        xa<-x[i]+fx*dt/a
        ya<-y[i]+fy*dt/a
        za<-z[i]+fz*dt/a
        if(xa<th) xa<-th
        x[i+1]<-x[i]+0.5*(fx+0.8*xa^0.75*(para[1]-para[9]*ins[i+1]-para[2]*za-para[3]*ya))*dt/a
        y[i+1]<-y[i]+0.5*(fy+(para[1]-para[9]*ins[i+1]-para[2]*za)*(para[4]*za+para[5]*(xa-para[8])-ya)/xa^0.25)*dt/a
        z[i+1]<-z[i]+0.5*(fz-para[6]*(xa-para[8])-para[7]*za)*dt/a
        if(x[i+1]<th) x[i+1]<-th
    }  
    return( (10^(-1.5))*( (x*10^6)^1.25 )*(10^9)/4/(10^14) )
}


plot(t,z,type="l",ylim=c(160,0))
lines(t,func(1,1),col=4)

ix<-which(t>=(-1000))
pad<-9

# spec data
zt<-ts(z[ix],deltat=dt)
specz<-spec.pgram(zt,pad=pad,plot=FALSE)
specz$spec<-2*specz$spec  # non normalized

cx<-1.7
cx2<-1.4
cx3<-1.1
cx4<-1.5
cl<-rev(viridis(53))
cl<-brewer.pal(5, "Set1")
yl<-c(160,-20)
xl<-c(min(t[ix]),max(t[ix]))
#postscript(file="tau_vcv_traj.eps", onefile=FALSE, horizontal=TRUE,encoding="WinAnsi.enc")
layout(matrix(c(1,2,
                3,4,
                5,6,
                7,8,
                9,10), 5, 2, byrow = FALSE), widths=c(1,1))
par(oma=c(2.5,0.1,0.3,0.1))
par(mai = c(0.2, 0.7, 0.12, 0.1))

for(i in 0:4){
    if(i==0) xt<-func(1,0.0)
    if(i==1) xt<-func(1,0.5)
    if(i==2) xt<-func(1,1.0)
    if(i==3) xt<-func(1,1.5)
    if(i==4) xt<-func(1,2.0)
    plot(t,z,xlim=xl,ylim=yl,col=1,type="l",ylab="Sea level (m)",xlab="", cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F,lwd=0.8)
    lines(t,xt,col=cl[i+1],lwd=cx)
    axis(side=2, cex.axis=cx)
    axis(side=1, cex.axis=cx)
    if(i==0) text(-860,-13,"(a-1) A=0",cex=cx4)
    if(i==1) text(-860,-13,"(b-1) A=0.5",cex=cx4)
    if(i==2) text(-860,-13,"(c-1) A=1.0",cex=cx4)
    if(i==3) text(-860,-13,"(d-1) A=1.5",cex=cx4)
    if(i==4) text(-860,-13,"(e-1) A=2.0",cex=cx4)
}
mtext(expression(paste("Age (kyr BP)")), side = 1, line = 2.8, col=1, cex=cx3)
 
for(i in 0:4){
    if(i==0) xt<-ts(func(1,0)[ix],deltat=dt)
    if(i==1) xt<-ts(func(1,0.5)[ix],deltat=dt)
    if(i==2) xt<-ts(func(1,1.0)[ix],deltat=dt)
    if(i==3) xt<-ts(func(1,1.5)[ix],deltat=dt)
    if(i==4) xt<-ts(func(1,2.0)[ix],deltat=dt)
    spec<-spec.pgram(xt,pad=pad,plot=FALSE)
    spec$spec<-2*spec$spec
    plot(specz$freq[specz$freq<0.055],specz$spec[specz$freq<0.055],col=1,xlim=c(0,0.055),ylim=c(0,max(specz$spec)*1.4),type="l",xlab="",ylab="",cex=cx,cex.axis=cx,cex.lab=cx,bty="n",lwd=0.8,lty=1,xaxs="i")
    lines(spec$freq[spec$freq<0.055],spec$spec[spec$freq<0.055],col=cl[i+1],xlab="",ylab="",lwd=2,lty=1)
    abline(v=1/18.9,lty=2)
    abline(v=1/22.4,lty=2)
    abline(v=1/23.7,lty=2)
    abline(v=1/41,lty=2)
    abline(v=1/82,lty=2)
    abline(v=1/95,lty=2)
    abline(v=1/124,lty=2)
    abline(v=1/405,lty=2)
    text(1/19,max(specz$spec)*1.1,expression(paste(italic("19"))),col=1,cex=cx4,bty="n")
    text(1/22.4,max(specz$spec)*0.95,expression(paste(italic("22.4"))),col=1,cex=cx4,bty="n")
    text(1/23.7,max(specz$spec)*1.1,expression(paste(italic("23.7"))),col=1,cex=cx4,bty="n")
    text(1/41,max(specz$spec)*1.1,expression(paste(italic("41"))),col=1,cex=cx4,bty="n")
    text(1/76,max(specz$spec)*0.95,expression(paste(italic("82"))),col=1,cex=cx4,bty="n")
    text(1/95,max(specz$spec)*1.1,expression(paste(italic("95"))),col=1,cex=cx4,bty="n")
    text(1/124-0.0005,max(specz$spec)*0.95,expression(paste(italic("124"))),col=1,cex=cx4,bty="n")
    text(1/405,max(specz$spec)*1.1,expression(paste(italic("405"))),col=1,cex=cx4,bty="n")
    mtext(expression(paste("PSD (m"^2, "kyr)")), side = 2, line = 2.5, col=1, cex=cx3)
    if(i==0) text(0.033,max(specz$spec)*0.5,"(a-2) A=0",cex=cx4)
    if(i==1) text(0.033,max(specz$spec)*0.5,"(b-2) A=0.5",cex=cx4)
    if(i==2) text(0.033,max(specz$spec)*0.5,"(c-2) A=1.0",cex=cx4)
    if(i==3) text(0.033,max(specz$spec)*0.5,"(d-2) A=1.5",cex=cx4)
    if(i==4) text(0.033,max(specz$spec)*0.5,"(e-2) A=2.0",cex=cx4)
}
mtext(expression(paste("Frequency (kyr"^{-1},")")), side = 1, line = 2.8, col=1, cex=cx3)
#dev.off()



### Sensitivity experiments ##########################################################################
tau<-0.04*(5:50)
w<-0.04*(0:50)
ii<-1:length(tau)
jj<-1:length(w)
xt<-ts(func(1,1)[ix],deltat=dt)
fr<-spec.pgram(xt,pad=pad,plot=FALSE)$freq
px<-expand.grid(tau,w)
pj<-expand.grid(ii,jj)
#er<-0.028
er<-0.04
regime<-numeric(nrow(px))
periods<-c(405,124,95,82,41,23.7,22.4,18.9)
for(i in 1:nrow(px)){
    xt<-ts(func(px[i,1],px[i,2])[ix],deltat=dt)
    sp<-2*spec.pgram(xt,pad=pad,plot=FALSE)$spec
    period<-1/fr[which.max(sp)]
    m<-which.min(abs(period-periods))
    if(abs(period-periods[m])<er*periods[m]) regime[i]<-m
}

tau0<-95
#postscript(file="tau_regime_vcv_er0028.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
postscript(file="tau_regime_vcv.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))
cl<-viridis(4)
cx<-1.5
plot(px[regime==1,1],px[regime==1,2],type="p",pch=1,col=cl[3],lwd=2,xlim=c(min(tau),max(tau)),ylim=c(min(w),2),ylab="Forcing amplitude A",xlab="",cex.lab=cx,cex.axis=cx,cex=cx,xaxs="i",yaxs="i")
lines(px[regime==2,1],px[regime==2,2],type="p",pch=2,col=cl[1],cex=cx,lwd=2)
lines(px[regime==3,1],px[regime==3,2],type="p",pch=3,col=rgb(1,0,0),cex=cx,lwd=2)
lines(px[regime==4,1],px[regime==4,2],type="p",pch=4,col="darkorange",cex=cx,lwd=2)
lines(px[regime==5,1],px[regime==5,2],type="p",pch=5,col="dodgerblue",cex=cx,lwd=2)
lines(px[regime==6,1],px[regime==6,2],type="p",pch=6,col="violet",cex=cx,lwd=2)
lines(px[regime==7,1],px[regime==7,2],type="p",pch=7,col="yellow",cex=cx,lwd=2)
lines(px[regime==8,1],px[regime==8,2],type="p",pch=8,col=8,cex=cx,lwd=2)
#legend("bottomright",c("124 kyr","95 kyr","82 kyr","41 kyr","23.7 kyr"),col=c(cl[1],2,"violet","dodgerblue","darkorange"),cex=cx,pch=c(2,3,4,5,6),bg="white")
#legend("topleft",c("405 kyr","124 kyr","95 kyr","82 kyr","41 kyr","23.7 kyr"),col=c(cl[3],cl[1],2,"violet","dodgerblue","darkorange"),cex=cx,pch=c(1,2,3,4,5,6),bg="white")
legend("bottomright",c("82 kyr","41 kyr","23.7 kyr"),col=c("darkorange","dodgerblue","violet"),cex=cx,pch=c(4,5,6),bg="white")
legend(1.18,0.5,c("405 kyr","124 kyr","95 kyr"),col=c(cl[3],cl[1],rgb(1,0,0)),cex=cx,pch=c(1,2,3),bg="white")
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.5, col=1, cex=cx)
title("(b) VCV18 model",line=0.6,cex.main=cx)
points(1,1,pch=18,cex=2)
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Natural period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
abline(h=0)
dev.off()
