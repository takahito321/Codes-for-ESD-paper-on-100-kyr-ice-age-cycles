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

#plot(t,z,type="l",ylim=c(1,-1))
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

tau0<-91.7
#postscript(file="tau_pcc.eps", width=8, height=7)
postscript(file="tau_pcc.eps", width=8, height=7, family="Times", onefile=FALSE, horizontal=FALSE)
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
title("(a) VDH model", line=0.6, cex=cx)
points(1,1,pch=18,cex=2)
axis(1, at=c(23.7,41,95,124)/tau0, labels=c(23.7,41,95,124), line=4.5, col=1, col.ticks=1, col.axis=1, cex.axis=cx)
mtext(expression(paste("Internal period rT"[0]," (kyr)")), 1, line=7, at=0.75, col=1, cex=cx)
dev.off()
