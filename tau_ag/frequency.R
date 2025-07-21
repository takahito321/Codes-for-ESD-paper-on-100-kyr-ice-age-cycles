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
ins<-ins-mean(ins[t>=(-1000)])

# Heun dt above
t1<-30  # kyr
t2<-10  # kyr
f1<--16 # Wm-2
f2<-16  # Wm-2
Vc<-1.4
# Heun dt above
func<-function(b){
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
        if(k==1) y[i+1]<-y[i]+(Ve-y[i])/t1*dt
        if(k==2) y[i+1]<-y[i]-Vc/t2*dt
        if(y[i+1]<0) y[i+1]<-0
    }  
    return(y)
}


plot(t,z,type="l",ylim=c(1.8,0))
lines(t,func(1),col=4)

ix<-which(t>=(-1000))

w<-0.02*(0:100)
xt<-ts(func(w[1])[ix],deltat=dt)
fr<-spec.pgram(xt,pad=15.3,plot=FALSE)$freq
sp<-matrix(0,length(w),length(fr))
th<-0.7
for(i in 1:length(w)){
    xt<-ts(func(w[i])[ix],deltat=dt)
    sp[i,]<-2*spec.pgram(xt,pad=15.3,plot=FALSE)$spec
}

df<-(1/82-1/95)/2
postscript(file="frequency_ag.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
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
image.plot(w,fr[fr<0.028],sp[,fr<0.028],xlim=c(-0.2,2),zlim=c(0,max(sp)),xlab="Forcing amplitude A",ylab=expression(paste("Frequency (kyr"^{-1},")")), cex=cx2,cex.lab=cx,cex.axis=cx,xaxs="i",col=cl3,axis.args=list(cex.axis=cx))
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
title("(c) G24 model 3",line=0.6,cex=cx)
dev.off()
