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
pad=9
tau0<-95
tau<-0.04*(5:50)
xt<-ts(func(1,1)[ix],deltat=dt)
fr<-spec.pgram(xt,pad=pad,plot=FALSE)$freq
Df<-(1/82-1/95)/3
cx<-1.7
cx2<-1.4
cx3<-1.1
cx4<-1.5
cl<-viridis(4)

### Sensitivity experiments ##########################################################################
pw41<-numeric(length(tau))
pw82<-numeric(length(tau))
pw95<-numeric(length(tau))
pw124<-numeric(length(tau))
Q<-numeric(length(tau))
for(i in 1:length(tau)){
    xt<-ts(func(tau[i],1)[ix],deltat=dt)
    sp<-2*spec.pgram(xt,pad=pad,plot=FALSE)$spec
    pw41[i]<-sum(sp[fr>(1/41-Df) & fr<(1/41+Df)])*(fr[2]-fr[1])
    pw82[i]<-sum(sp[fr>(1/82-Df) & fr<(1/82+Df)])*(fr[2]-fr[1])
    pw95[i]<-sum(sp[fr>(1/95-Df) & fr<(1/95+Df)])*(fr[2]-fr[1])
    pw124[i]<-sum(sp[fr>(1/124-Df) & fr<(1/124+Df)])*(fr[2]-fr[1])
}

postscript(file="tau_power_vcv.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,pw41,type="b",pch=5,col="dodgerblue",ylab=expression(paste("Band power (m"^2,")")),xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,pw95,type="b",col="red",cex=cx,pch=3,lwd=2)
lines(tau,pw82,type="b",col="violet",cex=cx,pch=4)  
lines(tau,pw124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","82 kyr","41 kyr"),col=c(cl[1],2,"violet","dodgerblue"),cex=cx,pch=c(2,3,4,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Natural period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(b) VCV18 model",line=0.6,cex=1.8)
dev.off()

### Sensitivity experiments ##########################################################################
pw41<-numeric(length(tau))
pw82<-numeric(length(tau))
pw95<-numeric(length(tau))
pw124<-numeric(length(tau))
Q<-numeric(length(tau))
for(i in 1:length(tau)){
    xt<-ts(func(tau[i],1)[ix],deltat=dt)
    sp<-2*spec.pgram(xt,pad=pad,plot=FALSE)$spec
    pw41[i]<-sum(sp[fr>(1/41-Df) & fr<(1/41+Df)])/sum(sp)
    pw82[i]<-sum(sp[fr>(1/82-Df) & fr<(1/82+Df)])/sum(sp)
    pw95[i]<-sum(sp[fr>(1/95-Df) & fr<(1/95+Df)])/sum(sp)
    pw124[i]<-sum(sp[fr>(1/124-Df) & fr<(1/124+Df)])/sum(sp)
}

postscript(file="tau_power_vcv_normalized.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,pw41,ylim=c(0,0.25),type="b",pch=5,col="dodgerblue",ylab=expression(paste("Relative band power")),xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,pw95,type="b",col="red",cex=cx,pch=3,lwd=2)
lines(tau,pw82,type="b",col="violet",cex=cx,pch=4)  
lines(tau,pw124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","82 kyr","41 kyr"),col=c(cl[1],2,"violet","dodgerblue"),cex=cx,pch=c(2,3,4,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Natural period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(b) VCV model",line=0.6,cex=1.8)
dev.off()


# Q ############################################################
Q1<-tau
Q2<-tau
Q3<-tau
Qp<-tau
Q41<-tau
Q82<-tau
Q95<-tau
Q124<-tau
Q405<-tau
for(i in 1:length(tau)){
    xt<-func(tau[i],1)
    xt<-xt-mean(xt)
    P<-18.9
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q1[i]<-sqrt( Qc^2+Qs^2 )
    P<-22.4
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q2[i]<-sqrt( Qc^2+Qs^2 )
    P<-23.7
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q3[i]<-sqrt( Qc^2+Qs^2 )
    Qp[i]<-max(Q1[i],Q2[i],Q3[i])
    P<-41
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q41[i]<-sqrt( Qc^2+Qs^2 )
    P<-82
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q82[i]<-sqrt( Qc^2+Qs^2 )
    P<-95
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q95[i]<-sqrt( Qc^2+Qs^2 )
    P<-124
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q124[i]<-sqrt( Qc^2+Qs^2 )
    P<-405
    ts<- -P*(1000 %/% P)
    Qc<-2*mean( xt[t>ts]*cos(2*pi*t[t>ts]/P) )
    Qs<-2*mean( xt[t>ts]*sin(2*pi*t[t>ts]/P) )
    Q405[i]<-sqrt( Qc^2+Qs^2 )
}

Qmax<-max(c(Qp,Q41,Q82,Q95,Q124,Q405))

postscript(file="Q_vcv.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,Q41,ylim=c(0,60),type="b",pch=5,col="dodgerblue",ylab=expression(paste("Q (m)")),xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,Q95,type="b",col=rgb(1,0,0),cex=cx,pch=3,lwd=2)
lines(tau,Q82,type="b",col="darkorange",cex=cx,pch=4)  
lines(tau,Q124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","82 kyr","41 kyr"),col=c(cl[1],rgb(1,0,0),"darkorange","dodgerblue"),cex=cx,pch=c(2,3,4,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Natural period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(b) VCV18 model",line=0.6,cex.main=1.5)
dev.off()


