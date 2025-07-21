library(palinsol)
library(viridis)
library(Hmisc)
library(fields)
library(RColorBrewer)

# LR04
dt<-0.05
t<-seq(-1100,0,dt)
n<-length(t)
tab<-read.table("../LR04stack.txt")
z<-approx(-tab[,1],tab[,2],t)$y
z<-z-z[n]

# insolation
tts<-1000*t-50   
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
ins<-ins-mean(ins)


# Heun dt above
t1<-30  # kyr
t2<-10  # kyr
f1<--16 # Wm-2
f2<-16  # Wm-2
Vc<-1.4
func<-function(a,b){
    y<-rep(z[1],n)
    f<-b*ins
    k<-1
    for(i in 1:(n-1)){
        if(f[i]<f1){
            Ve<-1+sqrt((f2-f[i])/(f2-f1))
        } else if(f[i]>f2){
            Ve<-0
        } else{ 
            Vu<-1-sqrt((f2-f[i])/(f2-f1))   
            if(y[i]>Vu){
                Ve<-1+sqrt((f2-f[i])/(f2-f1))
            } else{
                Ve<-0
            }
        }
        if(k==1 && (f[i+1]-f[i])>0 && f[i]>0 && y[i]>Vc) k<-2
        if(k==2 && f[i]<f1) k<-1
        if(k==1) y[i+1]<-y[i]+(Ve-y[i])/t1*dt/a
        if(k==2) y[i+1]<-y[i]-Vc/t2*dt/a
        if(y[i+1]<0) y[i+1]<-0
    }  
    return(y)
}


plot(t,z,type="l",ylim=c(1.6,0))
lines(t,func(1,1),col=4)

ix<-which(t>=(-1000))

# spec data
zt<-ts(z[ix],deltat=dt)
specz<-spec.pgram(zt,pad=15.3,plot=FALSE)
specz$spec<-2*specz$spec  # non normalized

cx<-1.7
cx2<-1.4
cx3<-1.1
cx4<-1.5
cl<-rev(viridis(53))
cl<-brewer.pal(5, "Set1")
yl<-c(1.7,-0.2)
xl<-c(min(t[ix]),max(t[ix]))
#postscript(file="tau_ag_traj.eps", onefile=FALSE, horizontal=TRUE,encoding="WinAnsi.enc")
layout(matrix(c(1,2,
                3,4,
                5,6,
                7,8,
                9,10), 5, 2, byrow = FALSE), widths=c(1,1))
#par(mfrow=c(6,1))
#par(mar=c(0,0,0,0))
par(oma=c(2.5,0.1,0.3,0.1))
par(mai = c(0.2, 0.7, 0.12, 0.1))

for(i in 0:4){
    if(i==0) xt<-func(1,0.0)
    if(i==1) xt<-func(1,0.5)
    if(i==2) xt<-func(1,1.0)
    if(i==3) xt<-func(1,1.5)
    if(i==4) xt<-func(1,2.0)
    plot(t,z,xlim=xl,ylim=yl,col=1,type="l",ylab="Sea level (a.u.)",xlab="", cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F,lwd=0.8)
    lines(t,xt,col=cl[i+1],lwd=cx)
    axis(side=2, cex.axis=cx)
    axis(side=1, cex.axis=cx)
    if(i==0) text(-860,-0.1,"(a-1) A=0",cex=cx4)
    if(i==1) text(-860,-0.1,"(b-1) A=0.5",cex=cx4)
    if(i==2) text(-860,-0.1,"(c-1) A=1.0",cex=cx4)
    if(i==3) text(-860,-0.1,"(d-1) A=1.5",cex=cx4)
    if(i==4) text(-860,-0.1,"(e-1) A=2.0",cex=cx4)
}
mtext(expression(paste("Age (kyr BP)")), side = 1, line = 2.8, col=1, cex=cx3)
 
for(i in 0:4){
    if(i==0) xt<-ts(func(1,0.0)[ix],deltat=dt)
    if(i==1) xt<-ts(func(1,0.5)[ix],deltat=dt)
    if(i==2) xt<-ts(func(1,1.0)[ix],deltat=dt)
    if(i==3) xt<-ts(func(1,1.5)[ix],deltat=dt)
    if(i==4) xt<-ts(func(1,2.0)[ix],deltat=dt)
    spec<-spec.pgram(xt,pad=15.3,plot=FALSE)
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
    mtext(expression(paste("PSD (kyr)")), side = 2, line = 2.5, col=1, cex=cx3)
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
fr<-spec.pgram(xt,pad=15.3,plot=FALSE)$freq
px<-expand.grid(tau,w)
pj<-expand.grid(ii,jj)
er<-0.04
regime<-numeric(nrow(px))
amplitude<-matrix(NA,length(tau),length(w))
for(i in 1:nrow(px)){
    xt<-ts(func(px[i,1],px[i,2])[ix],deltat=dt)
    amplitude[pj[i,1],pj[i,2]]<-sd(xt)
    sp<-2*spec.pgram(xt,pad=15.3,plot=FALSE)$spec
    period<-1/fr[which.max(sp)]
    if(abs(period-18.9)<er*18.9) regime[i]<-8
    if(abs(period-22.4)<er*22.4) regime[i]<-7
    if(abs(period-23.7)<er*23.7) regime[i]<-6
    if(abs(period-41)<er*41)     regime[i]<-5
    if(abs(period-82)<er*82)     regime[i]<-4
    if(abs(period-95)<er*95)     regime[i]<-3
    if(abs(period-124)<er*124)   regime[i]<-2
    if(abs(period-405)<er*405)   regime[i]<-1
    if(regime[i]==7){
        if(period>(22.4+23.7)/2) regime[i]<-6
    }
}

#amplitude2<-amplitude

tau0<-78.3
postscript(file="tau_ag.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))
cl<-viridis(4)
cx<-1.4
image.plot(tau,w,amplitude,col=brewer.pal(9,"Purples")[1:9],ylim=c(min(w),2),zlim=c(0,max(amplitude)),ylab="Forcing amplitude A",xlab="",cex.lab=cx,cex.axis=cx,cex=cx)
lines(px[regime==1,1],px[regime==1,2],type="p",pch=1,col=cl[3],lwd=2)
lines(px[regime==2,1],px[regime==2,2],type="p",pch=2,col=cl[1],cex=cx,lwd=2)
lines(px[regime==3,1],px[regime==3,2],type="p",pch=3,col=2,cex=cx,lwd=2)
lines(px[regime==4,1],px[regime==4,2],type="p",pch=4,col="pink",cex=cx,lwd=2)
lines(px[regime==5,1],px[regime==5,2],type="p",pch=5,col="dodgerblue",cex=cx,lwd=2)
lines(px[regime==6,1],px[regime==6,2],type="p",pch=6,col="darkorange",cex=cx,lwd=2)
lines(px[regime==7,1],px[regime==7,2],type="p",pch=7,col=7,cex=cx,lwd=2)
lines(px[regime==8,1],px[regime==8,2],type="p",pch=8,col=8,cex=cx,lwd=2)
legend("topright",c("405 kyr","124 kyr","95 kyr","82 kyr","41 kyr","23.7 kyr","22.4 kyr"),col=c(cl[3],cl[1],2,"pink","dodgerblue","darkorange",7),cex=cx,pch=c(1,2,3,4,5,6,7),bg="white")
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.5, col=1, cex=cx)
title("(c) G24 model 3",line=0.6,cex=cx)
points(1,1,pch=18,cex=2)
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Intrinsic timescale rT"[0]," (kyr)")),1,line=7,at=0.95,col=1,cex=cx)
abline(h=0)
dev.off()



postscript(file="tau_ag_amplitude.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))
plot(tau,amplitude[,21],type="b",col=1,ylab="Standard deviation (m)",xlab="",cex.lab=cx,cex.axis=cx,cex=cx) # w==0.8
lines(tau,amplitude[,26],type="b",col=2,cex=cx,pch=2) # w==1
lines(tau,amplitude[,31],type="b",col=3,cex=cx,pch=3) # w==1.2
lines(tau,amplitude[,36],type="b",col=4,cex=cx,pch=4) # w==1.4
legend("topright",c("A=1.4","A=1.2","A=1.0","A=0.8"),col=4:1,cex=cx,pch=4:1,bg="white")
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.5, col=1, cex=cx)
title("(c) G24 model 3",line=0.6,cex=cx)
points(1,1,pch=18,cex=2)
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Natural periodicity ",r,"T"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
dev.off()
