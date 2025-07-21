library(palinsol)
library(viridis)
library(Hmisc)
library(fields)
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
func<-function(b){
    para<-c(0.065,0.005,0.042,2,2,0.21,0.3,12,0.11)
    para[9]<-b*para[9]
    x<-rep(5,n) # S (10^6 km2)
    y<-rep(0,n) # theta (deg C)
    z<-rep(2,n) # omega
    
    for(i in 1:(n-1)){
        fx<-0.8*x[i]^0.75*(para[1]-para[9]*ins[i]-para[2]*z[i]-para[3]*y[i])
        fy<-(para[1]-para[9]*ins[i]-para[2]*z[i])*(para[4]*z[i]+para[5]*(x[i]-para[8])-y[i])/x[i]^0.25
        fz<--para[6]*(x[i]-para[8])-para[7]*z[i]
        xa<-x[i]+fx*dt
        ya<-y[i]+fy*dt
        za<-z[i]+fz*dt
        if(xa<th) xa<-th
        x[i+1]<-x[i]+0.5*(fx+0.8*xa^0.75*(para[1]-para[9]*ins[i+1]-para[2]*za-para[3]*ya))*dt
        y[i+1]<-y[i]+0.5*(fy+(para[1]-para[9]*ins[i+1]-para[2]*za)*(para[4]*za+para[5]*(xa-para[8])-ya)/xa^0.25)*dt
        z[i+1]<-z[i]+0.5*(fz-para[6]*(xa-para[8])-para[7]*za)*dt
        if(x[i+1]<th) x[i+1]<-th
    }  
    return( (10^(-1.5))*( (x*10^6)^1.25 )*(10^9)/4/(10^14) )
}


plot(t,z,type="l",ylim=c(160,0))
lines(t,func(1),col=4)

ix<-which(t>=(-1000))

w<-0.02*(0:125)
xt<-ts(func(w[1])[ix],deltat=dt)
fr<-spec.pgram(xt,pad=15.3,plot=FALSE)$freq
sp<-matrix(0,length(w),length(fr))
th<-0.7
for(i in 1:length(w)){
    xt<-ts(func(w[i])[ix],deltat=dt)
    sp[i,]<-2*spec.pgram(xt,pad=15.3,plot=FALSE)$spec
}

df<-(1/82-1/95)/2
postscript(file="frequency_vcv.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
cx<-1.5
cx2<-1.5
#cl3<-viridis(100)
#cl3<-rev(mako(100))
cl3<-rev(magma(100))
#cl3<-rev(rocket(100))
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(0.2,0,0,2.5))
par(mai = c(0.9, 1.1, 0.5, 0.2))
image.plot(w,fr[fr<0.028],sp[,fr<0.028],xlim=c(-0.2,2),xlab="Forcing amplitude A",ylab=expression(paste("Frequency (kyr"^{-1},")")), cex=cx2,cex.lab=cx,cex.axis=cx,xaxs="i",col=cl3,axis.args=list(cex.axis=cx))
#contour(w,fr[fr<0.028],sp[,fr<0.028],xlim=c(-0.2,2),xlab="B",ylab=expression(paste("Frequency (kyr"^{-1},")")), cex=cx2,cex.lab=cx,cex.axis=cx,xaxs="i",col=cl3,axis.args=list(cex.axis=cx))
segments(0,1/41,3,1/41,lty=2,col=1,lwd=2)
segments(0,1/82,4,1/82,lty=2,col=1,lwd=2)
segments(0,1/95,3,1/95,lty=2,col=1,lwd=2)
segments(0,1/124,3,1/124,lty=2,col=1,lwd=2)
segments(0,1/405,3,1/405,lty=2,col=1,lwd=2)
text(-0.1,1/41,"41",cex=1.5,col=1)
text(-0.1,1/82,"82",cex=1.5,col=1)
text(-0.1,1/95,"95",cex=1.5,col=1)
text(-0.1,1/124,"124",cex=1.5,col=1)
text(3.1,1/40.978-1/53.722,"173",cex=1.5,col=1)
text(-0.1,1/405,"405",cex=1.5,col=1)
minor.tick(nx=5, ny=2, tick.ratio=0.6)
title("(b) VCV18 model",line=0.6,cex=cx)
dev.off()
