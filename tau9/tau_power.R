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

plot(t,z,type="l",ylim=c(1,-1))
lines(t,func(1,1),col=4)

ix<-which(t>=(-1000))
pad<-9
tau0<-91.7
cx<-1.7
cx2<-1.4
cx3<-1.1
cx4<-1.5
cl<-viridis(4)
tau<-0.04*(5:50)
xt<-ts(func(1,1)[ix],deltat=dt)
fr<-spec.pgram(xt,pad=pad,plot=FALSE)$freq
Df<-(1/82-1/95)/3

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

cx<-1.7
cx2<-1.4
cx3<-1.1
cx4<-1.5
cl<-viridis(4)
#postscript(file="tau_power.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,pw41,ylim=c(0,0.1),type="b",pch=5,col="dodgerblue",ylab=expression(paste("Band power (\u2030"^2,")")),xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,pw95,type="b",col="red",cex=cx,pch=3,lwd=2)
lines(tau,pw82,type="b",col="violet",cex=cx,pch=4)  
lines(tau,pw124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","82 kyr","41 kyr"),col=c(cl[1],2,"violet","dodgerblue"),cex=cx,pch=c(2,3,4,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Internal period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(a) SO model",line=0.6,cex=1.8)
#dev.off()


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

postscript(file="tau_power_normalized.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,pw41,ylim=c(0,0.5),type="b",pch=5,col="dodgerblue",ylab=expression(paste("Relative band power")),xlab="",cex.lab=cx,cex.axis=cx,cex=cx) 
lines(tau,pw95,type="b",col="red",cex=cx,pch=3,lwd=2)
lines(tau,pw82,type="b",col="violet",cex=cx,pch=4)  
lines(tau,pw124,type="b",col=cl[1],cex=cx,pch=2) 
legend("topright",c("124 kyr","95 kyr","82 kyr","41 kyr"),col=c(cl[1],2,"violet","dodgerblue"),cex=cx,pch=c(2,3,4,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Internal period rT"[0]," (kyr)")),1,line=7,at=0.75,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(a) SO model",line=0.6,cex=1.8)
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

postscript(file="Q.eps",onefile=FALSE,horizontal=FALSE,encoding="WinAnsi.enc",width=8,height=7)
par(mfrow=c(1,1))
par(mar=c(0,0,0,0))
par(oma=c(1,0,0,0))
par(mai = c(1.5, 1, 0.5, 0.6))

plot(tau,Q41,ylim=c(0,0.51),type="b",pch=5,col="dodgerblue",ylab=expression(paste("Q (\u2030)")),xlab="",cex.lab=cx,cex.axis=cx,cex=cx,lwd=1.5) 
lines(tau,Q95,type="b",col="red",cex=cx,pch=3,lwd=2.5)
lines(tau,Q82,type="b",col="darkorange",cex=cx,pch=4,lwd=1.8)  
lines(tau,Q124,type="b",col=cl[1],cex=cx,pch=2,lwd=1.5) 
legend("topright",c("124 kyr","95 kyr","82 kyr","41 kyr"),col=c(cl[1],2,"darkorange","dodgerblue"),cex=cx,pch=c(2,3,4,5),bg="white")
axis(1,c(23.7,41,95,124)/tau0,labels=c(23.7,41,95,124),line=4.5,col=1,col.ticks=1,col.axis=1,cex.axis=cx)
mtext(expression(paste("Internal period rT"[0]," (kyr)")),1,line=7.2,at=0.75,col=1,cex=cx)
mtext(expression(paste("Timescale control parameter r")), side = 1, line = 2.9, col=1, cex=cx)
title("(a) SO model",line=0.6,cex.main=1.5)
dev.off()
