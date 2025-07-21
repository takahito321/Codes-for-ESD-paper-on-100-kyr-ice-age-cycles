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
func<-function(b){
    #para<-c(1.0357736667, 2.8729822923, 0.0354486875, 0.0001007094, 0.0185370520, 0.0523631525, 0.0179049705, 0.0049252340, 0.1827216363) # 0.04159999 dt=0.1
    #para<-c(1.057188e+00, 2.955397e+00, 3.561500e-02, 9.968853e-05, 1.810488e-02, 5.106517e-02, 1.889141e-02, 5.002737e-03, 1.785975e-01)
    para<-c(1.0536394044, 2.9662458029, 0.0356079021, 0.0001000922, 0.0180996836, 0.0514402004, 0.0189082535, 0.0049923333, 0.1801349684) # 0.04181133 91.7 0.8790002

    para[6:8]<-c(b,b,b^2)*para[6:8] 
    y<-rep(z[1],n)
    x<-rep(0,n)
    for(i in 1:(n-1)){
        fx<-y[i]+para[1]*x[i]-para[2]*x[i]^3/3.0
        fy<--para[3]*x[i]-para[4]*x[i]^3-para[5]-para[6]*ins[i]-para[7]*x[i]*ins[i]-para[8]*ins[i]^2
        xa<-x[i]+fx*dt
        ya<-y[i]+fy*dt
        x[i+1]<-x[i]+0.5*(fx+ya+para[1]*xa-para[2]*xa^3/3.0)*dt
        y[i+1]<-y[i]+0.5*(fy-para[3]*xa-para[4]*xa^3-para[5]-para[6]*ins[i+1]-para[7]*xa*ins[i+1]-para[8]*ins[i+1]^2)*dt
    }  
    return(y+para[9])
}


plot(t,z,type="l",ylim=c(160,0))
lines(t,func(1),col=4)

ix<-which(t>=(-1000))

w<-0.02*(0:100)
xt<-ts(func(w[1])[ix],deltat=dt)
fr<-spec.pgram(xt,pad=15.3,plot=FALSE)$freq
sp<-matrix(0,length(w),length(fr))
for(i in 1:length(w)){
    xt<-ts(func(w[i])[ix],deltat=dt)
    sp[i,]<-2*spec.pgram(xt,pad=15.3,plot=FALSE)$spec
}

df<-(1/82-1/95)/2
postscript(file="frequency.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
cx<-1.5
cx2<-1.5
#cl3<-rev(mako(100))
cl3<-rev(magma(100))
#cl3<-rev(rocket(100))
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(0.2,0,0,2.5))
par(mai = c(0.9, 1.1, 0.5, 0.2))
image.plot(w,fr[fr<0.028],sp[,fr<0.028],xlim=c(-0.2,2),zlim=c(0,98),xlab="Forcing amplitude A",ylab=expression(paste("Frequency (kyr"^{-1},")")), cex=cx2,cex.lab=cx,cex.axis=cx,xaxs="i",col=cl3,axis.args=list(cex.axis=cx))
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
text(3.1,1/40.978-1/53.722,"173",cex=1.5,col=6)
text(-0.1,1/405,"405",cex=1.5,col=1)
minor.tick(nx=5, ny=2, tick.ratio=0.6)
title("(a) Oscillator model",line=0.6,cex=cx)
dev.off()
