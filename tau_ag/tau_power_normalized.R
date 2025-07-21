library(palinsol)
library(viridis)
library(Hmisc)
#library(fields)
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


cx<-1.7
cx2<-1.4
cx3<-1.1
cx4<-1.5
cl<-rev(viridis(53))
cl<-brewer.pal(5, "Set1")

### Sensitivity experiments ##########################################################################
tau<-0.04*(5:50)
xt<-ts(func(1,1)[ix],deltat=dt)
fr<-spec.pgram(xt,pad=15.3,plot=FALSE)$freq

Df<-(1/82-1/95)/2

pw41<-numeric(length(tau))
pw95<-numeric(length(tau))
pw124<-numeric(length(tau))
Q<-numeric(length(tau))
for(i in 1:length(tau)){
    xt<-ts(func(tau[i],1)[ix],deltat=dt)
    sp<-2*spec.pgram(xt,pad=15.3,plot=FALSE)$spec
    pw41[i]<-sum(sp[fr>(1/41-Df) & fr<(1/41+Df)])/sum(sp)
    pw95[i]<-sum(sp[fr>(1/95-Df) & fr<(1/95+Df)])/sum(sp)
    pw124[i]<-sum(sp[fr>(1/124-Df) & fr<(1/124+Df)])/sum(sp)
}

plot(tau,pw95,type="b",col=2)
lines(tau,pw41,type="b",col=4)
lines(tau,pw124,type="b",col=3)

#plot(tau,Q,type="b",col=2)

plot(t,func(1,1),type="l")
lines(t,func(0.85,1),col=2)


tau0<-76.45
cl<-viridis(4)
postscript(file="tau_power_normalized_ag.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

# Tcyc=61.45*r+15=Tastro
r1<-(23.7-15)/61.45
r2<-(41-15)/61.45
r3<-(95-15)/61.45
r4<-(124-15)/61.45

plot(tau,pw41,ylim=c(0,0.4),type="b",pch=5,col="dodgerblue",ylab="Relative band power strength",xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,pw95,type="b",col="red",cex=cx,pch=3) 
lines(tau,pw124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","41 kyr"),col=c(cl[1],2,"dodgerblue"),cex=cx,pch=c(2,3,5),bg="white")
axis(1,c(r2,1,r3,r4),labels=c(41,76,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Timescale for forming a cycle (kyr)")),side=1,line=7.2,at=0.95,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(c) G24-3 model",line=0.6,cex=cx)
dev.off()

