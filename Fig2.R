library(palinsol)
#library(viridis)
#library(Hmisc)
library(RColorBrewer)


# van der Pol ##############################################################
# LR04
dt<-0.05
t1<-seq(-1100,0,dt) 
n<-length(t1)
tab<-read.table("LR04stack.txt")
z<-approx(-tab[,1],tab[,2],t1)$y
z<-z-4 

# insolation
tts<-1000*t1-50   
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
ins<-ins-495.7
ins<-ins/24

# Heun dt above
volume<-function(para){
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
ps1<-c(1.0536394044, 2.9662458029, 0.0356079021, 0.0001000922, 0.0180996836, 0.0514402004, 0.0189082535, 0.0049923333, 0.1801349684) # 0.04181133 91.7 0.879000 oscillatory
vol1<-volume(ps1)
p10<-ps1
p10[6:8]<-0
vol10<-volume(p10)


# VCV ######################################################################
dt<-0.05
t2<-seq(-1200,0,dt)
n<-length(t2)
tab<-read.table("LR04stack.txt")
z2<-approx(-tab[,1],tab[,2],t2)$y
z2<- 26*z2^2-135*z2+163

# insolation
tts<-1000*t2-50
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins2<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
ins2<-ins2-495.7
ins2<-ins2/24

# parameters
d<-1      # 1 1/zeta (zeta 10^-3/2 km^1/2) 
a<-0.065  # 1                              
k<-0.005  # 2 kappa in Table 2
cc<-0.042 # 3 c in Table 2
alpha<-2  # 4
beta<-2   # 5 b in run_V6Nf.m
g1<-1e-6  #   gamma_1
g2<-0.21  # 6 gamma_2
g3<-0.3   # 7 gamma_3
S0<-12    # 8
ep<-0.11  # 9 Fig.7: 0.07 0.082 or 0.11 
th<-1e-4

# Heun dt above
volume2<-function(para){
    x<-rep(5,n) # S (10^6 km2)
    y<-rep(0,n) # theta (deg C)
    z<-rep(2,n) # omega
    
    for(i in 1:(n-1)){
        fx<-0.8*x[i]^0.75*(para[1]-para[9]*ins2[i]-para[2]*z[i]-para[3]*y[i])
        fy<-(para[1]-para[9]*ins2[i]-para[2]*z[i])*(para[4]*z[i]+para[5]*(x[i]-para[8])-y[i])/x[i]^0.25
        fz<--para[6]*(x[i]-para[8])-para[7]*z[i]
        xa<-x[i]+fx*dt
        ya<-y[i]+fy*dt
        za<-z[i]+fz*dt
        if(xa<th) xa<-th
        x[i+1]<-x[i]+0.5*(fx+0.8*xa^0.75*(para[1]-para[9]*ins2[i+1]-para[2]*za-para[3]*ya))*dt
        y[i+1]<-y[i]+0.5*(fy+(para[1]-para[9]*ins2[i+1]-para[2]*za)*(para[4]*za+para[5]*(xa-para[8])-ya)/xa^0.25)*dt
        z[i+1]<-z[i]+0.5*(fz-para[6]*(xa-para[8])-para[7]*za)*dt
        if(x[i+1]<th) x[i+1]<-th
    }  
    return( (10^(-1.5))*( (x*10^6)^1.25 )*(10^9)/4/(10^14) )
}

ps2<-c(0.065,0.005,0.042,2,2,0.21,0.3,12,0.11)
vol2<-volume2(ps2)

# VCV I=0 ######################################################################
n<-length(t1)
# Heun dt above
volume20<-function(para){
    x<-rep(0.1,n) # S (10^6 km2)
    y<-rep(0,n) # theta (deg C)
    z<-rep(2,n) # omega
    
    for(i in 1:(n-1)){
        fx<-0.8*x[i]^0.75*(para[1]-para[2]*z[i]-para[3]*y[i])
        fy<-(para[1]-para[2]*z[i])*(para[4]*z[i]+para[5]*(x[i]-para[8])-y[i])/x[i]^0.25
        fz<--para[6]*(x[i]-para[8])-para[7]*z[i]
        xa<-x[i]+fx*dt
        ya<-y[i]+fy*dt
        za<-z[i]+fz*dt
        if(xa<th) xa<-th
        x[i+1]<-x[i]+0.5*(fx+0.8*xa^0.75*(para[1]-para[2]*za-para[3]*ya))*dt
        y[i+1]<-y[i]+0.5*(fy+(para[1]-para[2]*za)*(para[4]*za+para[5]*(xa-para[8])-ya)/xa^0.25)*dt
        z[i+1]<-z[i]+0.5*(fz-para[6]*(xa-para[8])-para[7]*za)*dt
        if(x[i+1]<th) x[i+1]<-th
    }  
    return( (10^(-1.5))*( (x*10^6)^1.25 )*(10^9)/4/(10^14) )
}

vol20<-volume20(ps2)


# Ganopolski ###############################################################
dt<-0.05
t3<-seq(-1100,0,dt)
n<-length(t3)
tab<-read.table("LR04stack.txt")
z3<-approx(-tab[,1],tab[,2],t3)$y
z3<-z3-z3[n]

# insolation
tts<-1000*t3-50
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins3<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
ins3<-ins3-mean(ins3)

# Heun dt above
tau1<-30  # kyr
tau2<-10  # kyr
f1<--16 # Wm-2
f2<-16  # Wm-2
Vc<-1.4
volume3<-function(b,ini){
    y<-rep(ini,n)
    f<-b*ins3
    k<-1
    kk<-rep(k,n)
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
        if(k==1) y[i+1]<-y[i]+(Ve-y[i])/tau1*dt
        if(k==2) y[i+1]<-y[i]-Vc/tau2*dt
        if(y[i+1]<0) y[i+1]<-0
        kk[i+1]<-k
    }  
    return(rbind(y,kk))
}

vol3<-volume3(1,z3[1])[1,]
vol30<-volume3(0,0.3)[1,]
vol300<-volume3(0,0.25)[1,]

cl<-brewer.pal(6, "Dark2")
cl<-cl[2:5]
cu<-"deepskyblue"
cu<-"dodgerblue"
#cl<-brewer.pal(4, "Set1")
#cl<-rev(viridis(5))
# plot time series #####################################################################################
cx<-1.2
cx2<-1.5
cx3<-1.2
postscript(file="Fig22.eps", onefile=FALSE, horizontal=FALSE,encoding="WinAnsi.enc")
par(mfrow=c(4,1))
par(mar=c(0,0,0,0))
par(oma=c(2.5,1,0.5,1))
par(mai = c(0.3, 0.6, 0.1, 0.05))

plot(t1,ins,xlim=c(-1100,0),col="darkorange",type="l", ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx2,xaxs="i",bty = "n",axes=F, lwd=1.5)
axis(side=2, cex.axis=cx2)
axis(side=1, cex.axis=cx2)
mtext("I(t)", side = 2, line = 2.8, col=1, cex=cx3)
text(-810,2.7,"(a) Insolation anomaly",cex=1.4,col=1,font=2)

plot(t1,z,xlim=c(-1100,0),ylim=c(1.1,-1.2),type="l", ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx2,xaxs="i",bty = "n",axes=F,col="gray10",lty=2)
mtext(expression(paste(delta^{18},"O - 4 (\u2030)")), side = 2, line = 2.8, col=1, cex=cx3)
lines(t1,vol10,col=cu,lwd=1.5)
lines(t1,vol1,col=cl[3],lwd=2)
axis(side=2, cex.axis=cx2)
axis(side=1, cex.axis=cx2)
legend(-730,-1.23,legend="LR04",col=1,lty=2,cex=1.3,bty="n",lwd=1.5)
legend(-580,-1.23,legend="Forced",col=cl[3],lty=1,cex=1.3,bty="n",lwd=2)
legend(-420,-1.23,legend="Unforced",col=cu,lty=1,cex=1.3,bty="n",lwd=1.5)
text(-900,-1,"(b) SO model",cex=1.4,col=1,font=2)

plot(t2,z2,xlim=c(-1100,0),ylim=c(160,-20),type="l", ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx2,xaxs="i",bty = "n",axes=F,col="gray10",lty=2)
mtext(expression(paste("Sea level (m)")), side = 2, line = 2.8, col=1, cex=cx3)
lines(t1,vol20,col=cu,lwd=1.5)
lines(t2,vol2,col=cl[2],lwd=2)
axis(side=2, cex.axis=cx2)
axis(side=1, cex.axis=cx2)
legend(-730,-35,legend="LR04",col=1,lty=2,cex=1.3,bty="n",lwd=1.5)
legend(-580,-35,legend="Forced",col=cl[2],lty=1,cex=1.3,bty="n",lwd=2)
legend(-420,-35,legend="Unforced",col=cu,lty=1,cex=1.3,bty="n",lwd=1.5)
text(-900,-15,"(c) VCV18 model",cex=1.4,col=1,font=2)

plot(t3,z3,xlim=c(-1100,0),ylim=c(1.8,-0.3),type="l", ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx2,xaxs="i",bty = "n",axes=F,col="gray10",lty=2)
mtext(expression(paste("Sea level (a.u.)")), side = 2, line = 2.8, col=1, cex=cx3)
#abline(h=0,col=cu,lwd=1.5)
lines(t3,vol300,col=cu,lwd=1.5)
lines(t3,vol30,col=cu,lwd=1.5)
lines(t3,vol3,col=cl[4],lwd=2)
axis(side=2, cex.axis=cx2)
axis(side=1, cex.axis=cx2)
legend(-730,-0.42,legend="LR04",col=1,lty=2,cex=1.3,bty="n",lwd=1.5)
legend(-580,-0.42,legend="Forced",col=cl[4],lty=1,cex=1.3,bty="n",lwd=2)
legend(-420,-0.42,legend="Unforced",col=cu,lty=1,cex=1.3,bty="n",lwd=1.5)
text(-900,-0.2,"(d) G24-3 model",cex=1.4,col=1,font=2)

mtext("Time (kyr)", side = 1, line = 2.8, col=1, cex=cx3)
dev.off()
