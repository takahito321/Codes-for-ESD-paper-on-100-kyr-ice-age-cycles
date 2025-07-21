library(palinsol)
library(viridis)
#library(Hmisc)
#library(fields)
library(RColorBrewer)

# LR04
dt<-0.05
t<-seq(-1100,0,dt)
n<-length(t)
tab<-read.table("../LR04stack.txt")
z<-approx(-tab[,1],tab[,2],t)$y
z<-z-4 

# insolation
tts<-1000*t-50   
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
ins<-ins-495.7
ins<-ins/24

# Heun dt above
func<-function(a,b){
    #para<-c(1.0357736667, 2.8729822923, 0.0354486875, 0.0001007094, 0.0185370520, 0.0523631525, 0.0179049705, 0.0049252340, 0.1827216363) # 0.04159999 dt=0.1
    #para<-c(1.057188e+00, 2.955397e+00, 3.561500e-02, 9.968853e-05, 1.810488e-02, 5.106517e-02, 1.889141e-02, 5.002737e-03, 1.785975e-01)
    para<-c(1.0536394044, 2.9662458029, 0.0356079021, 0.0001000922, 0.0180996836, 0.0514402004, 0.0189082535, 0.0049923333, 0.1801349684) # 0.04181133 91.7 0.8790002

    para[6:8]<-c(b,b,b^2)*para[6:8] 
    y<-rep(z[1],n)
    x<-rep(0,n)
    for(i in 1:(n-1)){
        fx<-y[i]+para[1]*x[i]-para[2]*x[i]^3/3.0
        fy<--para[3]*x[i]-para[4]*x[i]^3-para[5]-para[6]*ins[i]-para[7]*x[i]*ins[i]-para[8]*ins[i]^2
        xa<-x[i]+fx*dt/a
        ya<-y[i]+fy*dt/a
        x[i+1]<-x[i]+0.5*(fx+ya+para[1]*xa-para[2]*xa^3/3.0)*dt/a
        y[i+1]<-y[i]+0.5*(fy-para[3]*xa-para[4]*xa^3-para[5]-para[6]*ins[i+1]-para[7]*xa*ins[i+1]-para[8]*ins[i+1]^2)*dt/a
    }  
    return(y+para[9])
}

plot(t,z,type="l",ylim=c(1,-1))
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
yl<-c(1,-1)
xl<-c(min(t[ix]),max(t[ix]))
#postscript(file="tau_traj.eps", onefile=FALSE, horizontal=TRUE,encoding="WinAnsi.enc")
layout(matrix(c(1,2,
                3,4,
                5,6,
                7,8,
                9,10), 5, 2, byrow = FALSE), widths=c(1,1))
par(oma=c(2.5,0.1,0.3,0.1))
par(mai = c(0.2, 0.7, 0.12, 0.1))

for(i in 0:4){
    if(i==0) xt<-func(1,0)
    if(i==1) xt<-func(1,0.5)
    if(i==2) xt<-func(1,1.0)
    if(i==3) xt<-func(1,1.5)
    if(i==4) xt<-func(1,2)
    plot(t,z,xlim=xl,ylim=yl,col=1,type="l",ylab=expression(paste(delta^{18},"O-4 (\u2030)")),xlab="", cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F,lwd=0.8)
    lines(t,xt,col=cl[i+1],lwd=cx)
    axis(side=2, cex.axis=cx)
    axis(side=1, cex.axis=cx)
    if(i==0) text(-860,-0.9,"(a-1) A=0",cex=cx4)
    if(i==1) text(-860,-0.9,"(b-1) A=0.5",cex=cx4)
    if(i==2) text(-860,-0.9,"(c-1) A=1.0",cex=cx4)
    if(i==3) text(-860,-0.9,"(d-1) A=1.5",cex=cx4)
    if(i==4) text(-860,-0.9,"(e-1) A=2.0",cex=cx4)
}
mtext(expression(paste("Age (kyr BP)")), side = 1, line = 2.8, col=1, cex=cx3)
 
for(i in 0:4){
    if(i==0) xt<-ts(func(1,0)[ix],deltat=dt)
    if(i==1) xt<-ts(func(1,0.5)[ix],deltat=dt)
    if(i==2) xt<-ts(func(1,1.0)[ix],deltat=dt)
    if(i==3) xt<-ts(func(1,1.5)[ix],deltat=dt)
    if(i==4) xt<-ts(func(1,2)[ix],deltat=dt)
    spec<-spec.pgram(xt,pad=pad,plot=FALSE,spans=c(3,3))
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
    text(1/74,max(specz$spec)*0.95,expression(paste(italic("82"))),col=1,cex=cx4,bty="n")
    text(1/95,max(specz$spec)*1.1,expression(paste(italic("95"))),col=1,cex=cx4,bty="n")
    text(1/124-0.0005,max(specz$spec)*0.95,expression(paste(italic("124"))),col=1,cex=cx4,bty="n")
    text(1/405,max(specz$spec)*1.1,expression(paste(italic("405"))),col=1,cex=cx4,bty="n")
    mtext(expression(paste("PSD (\u2030"^2, "kyr)")), side = 2, line = 2.5, col=1, cex=cx3)
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
px<-expand.grid(w,tau)
fr<-spec.pgram(xt,pad=pad,plot=FALSE)$freq
px<-expand.grid(tau,w)
pj<-expand.grid(ii,jj)
er<-0.028
#er<-0.04
#> (23.7-22.4)/(23.7+22.4)
#[1] 0.02819957
#> (95-82)/(95+82)
#[1] 0.07344633
regime<-numeric(nrow(px))
periods<-c(405,124,95,82,41,23.7,22.4,18.9)
for(i in 1:nrow(px)){
    xt<-ts(func(px[i,1],px[i,2])[ix],deltat=dt)
    sp<-2*spec.pgram(xt,pad=pad,plot=FALSE)$spec
    period<-1/fr[which.max(sp)]
    m<-which.min(abs(period-periods))
    if(abs(period-periods[m])<er*periods[m]) regime[i]<-m
}

tau0<-91.7
postscript(file="tau_regime.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
#postscript(file="tau_regime_er004.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
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
legend("bottomright",c("405 kyr","124 kyr","95 kyr","82 kyr","41 kyr","23.7 kyr","22.4 kyr","18.9 kyr"),col=c(cl[3],cl[1],rgb(1,0,0),"darkorange","dodgerblue","violet","yellow",8),cex=cx,pch=c(1,2,3,4,5,6,7,8),bg="white")
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.5, col=1, cex=cx)
title("(a) SO model",line=0.6,cex.main=cx)
points(1,1,pch=18,cex=2)
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Internal period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
dev.off()

