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
func<-function(a,b){
    ins<-b*sin(2*pi*t/a)
    
    para<-c(0.065,0.005,0.042,2,2,0.21,0.3,12,0.11)
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
lines(t,func(41,0),col=4)

ix<-which(t>=(-1000))



### Sensitivity experiments ##########################################################################
tau0<-95
tau<-seq(20,200,by=5)
A<-matrix(NA,5,length(tau))
for(i in 1:length(tau)){
  A[1,i]<-sd(func(tau[i],0.01)[ix>mean(ix)])  
  A[2,i]<-sd(func(tau[i],0.1)[ix>mean(ix)])  
  A[3,i]<-sd(func(tau[i],0.2)[ix>mean(ix)])  
  A[4,i]<-sd(func(tau[i],0.4)[ix>mean(ix)])  
  A[5,i]<-sd(func(tau[i],0.6)[ix>mean(ix)])  
}

postscript(file="softening.eps",onefile=FALSE,horizontal=TRUE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(0.7, 1, 0.1, 0.1))
cl<-viridis(5)
cx<-1.4

plot(tau,A[5,],type="l",ylim=c(0,max(A[5,])),xlab="",ylab="Standard deviation",cex.axis=cx,cex.lab=cx,col=cl[1],lty=1,lwd=2.5)
lines(tau,A[4,],col=cl[2],lty=1,lwd=2.5)
lines(tau,A[3,],col=cl[3],lty=1,lwd=2.5)
lines(tau,A[2,],col=cl[4],lty=1,lwd=2.5)
lines(tau,A[1,],col=cl[5],lty=1,lwd=2.5)
abline(v=tau0,lty=2)
mtext(expression(paste("Forcing period T"[e]," (kyr)")), side = 1, line = 2.8, col=1, cex=cx)
#title("(b) Resonance",line=0.6,cex=cx)
points(1,1,pch=18,cex=2)
legend("topleft",c("A=0.6","A=0.4","A=0.2","A=0.1","A=0.01"),col=c(cl[1],cl[2],cl[3],cl[4],cl[5]),cex=cx,lty=1,lwd=2.5)
dev.off()

