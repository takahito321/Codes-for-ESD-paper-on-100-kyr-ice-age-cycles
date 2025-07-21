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


### Sensitivity experiments ##########################################################################
tau<-0.04*(5:50)
xt<-ts(func(1,1)[ix],deltat=dt)
fr<-spec.pgram(xt,pad=1,plot=FALSE)$freq
pw41<-numeric(length(tau))
pw95<-numeric(length(tau))
pw124<-numeric(length(tau))

Df<-(1/82-1/95)/3

for(i in 1:length(tau)){
    xt<-ts(func(tau[i],1)[ix],deltat=dt)
    sp<-2*spec.pgram(xt,pad=1,plot=FALSE)$spec
    pw41[i]<-sum(sp[fr>(1/41-Df) & fr<(1/41+Df)])/sum(sp)
    pw95[i]<-sum(sp[fr>(1/95-Df) & fr<(1/95+Df)])/sum(sp)
    pw124[i]<-sum(sp[fr>(1/124-Df) & fr<(1/124+Df)])/sum(sp)
}

tau0<-95
cl<-viridis(4)
cx<-1.4
postscript(file="tau_power_vcv_normalized.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,pw41,ylim=c(0,0.3),type="b",pch=5,col="dodgerblue",ylab="Relative band power strength",xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,pw95,type="b",col="red",cex=cx,pch=3) 
lines(tau,pw124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","41 kyr"),col=c(cl[1],2,"dodgerblue"),cex=cx,pch=c(2,3,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
mtext(expression(paste("Natural period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
title("(b) VCV18 model",line=0.6,cex=cx)
dev.off()


