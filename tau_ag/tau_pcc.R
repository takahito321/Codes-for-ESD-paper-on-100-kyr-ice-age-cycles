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
ins<-ins-mean(ins[t>=(-1000)]) #  495.6942


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


#plot(t,z,type="l",ylim=c(1.6,0))
#lines(t,func(1,1),col=4)

ix<-which(t>=(-1000))
zt<-ts(z[ix],deltat=dt)

### Sensitivity experiments ##########################################################################
tau<-0.04*(5:50)
w<-0.04*(0:50)
cc<-matrix(NA,length(tau),length(w))
for(i in 1:length(tau)){
    for(j in 1:length(w)){
        cc[i,j]<-cor(func(tau[i],w[j])[ix],zt)
    }
    
}


tau0<-76.45
# Tcyc=61.45*r+15=Tastro
r1<-(23.7-15)/61.45
r2<-(41-15)/61.45
r3<-(95-15)/61.45
r4<-(124-15)/61.45
postscript(file="tau_pcc_ag.eps", width=8, height=7, family="Times", onefile=FALSE, horizontal=FALSE)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))
cx <- 1.4
# Spectralパレットの色を逆順にしてパレット関数を作成
base_colors <- rev(brewer.pal(11, "Spectral"))
colfunc <- colorRampPalette(base_colors)

# カラーマップだけ変更
image.plot(tau, w, cc, col = colfunc(100), zlim=c(-1,1), 
           ylab="Forcing amplitude A", xlab="", cex.lab=cx, cex.axis=cx, cex=cx)

mtext(expression(paste("Timescale control parameter r")), side=1, line=2.5, col=1, cex=cx)
title("(c) G24-3 model",line=0.6,cex.main=cx)
points(1,1,pch=18,cex=2)
axis(1,c(r2,1,r3,r4),labels=c(41,76,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Timescale for forming a cycle (kyr)")),1,line=7,at=0.95,col=1,cex=cx)
abline(h=0)
dev.off()

