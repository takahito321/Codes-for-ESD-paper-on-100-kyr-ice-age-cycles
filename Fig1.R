library(palinsol)
library(viridis)
library(Hmisc) 

# LR04
t<--(1000:0) 
n<-length(t)
tab<-read.table("LR04stack.txt")
y<-approx(-tab[,1],tab[,2],t)$y

# insolation, orbital
tts<-1000*t-50   
insolation <- function(times, astrosol=la04, long, lat, ...){
    sapply(times, function(tt) Insol(orbit=astrosol(tt), long, lat, S0=1368))
}
ins<-insolation(tts, la04, long=pi/2, lat=65*pi/180)
obl<-sapply(tts, function(t) la04(t,degree=TRUE)["eps"])
ecc<-sapply(tts, function(t) la04(t,degree=TRUE)["ecc"])
varpi<-sapply(tts, function(t) la04(t,degree=FALSE)["varpi"])
esin<-ecc*sin(varpi)

# untuned
tab<-read.table("LR04stack_untuned.txt")
y2<-approx(-tab[,2],tab[,3],t)$y

# untuned
tab<-read.table("huybers2006_edit.txt",header=T)
y3<-approx(-tab[,1],tab[,4],t)$y

xt<-ts(ins[t>(-1000)],deltat=1)
spi<-spec.pgram(xt,pad=15.3,plot=FALSE)

xt<-ts(ecc[t>(-1000)],deltat=1)
spe<-spec.pgram(xt,pad=15.3,plot=FALSE)

xt<-ts(y[t>(-1000)],deltat=1)
sp<-spec.pgram(xt,pad=15.3,plot=FALSE)

xt<-ts(y2[t>(-1000)],deltat=1)
sp2<-spec.pgram(xt,pad=15.3,plot=FALSE)

xt<-ts(y3[t>(-1000)],deltat=1)
sp3<-spec.pgram(xt,pad=15.3,plot=FALSE)

# plot time series #####################################################################################

postscript(file="Fig1.eps", onefile=FALSE, horizontal=FALSE, encoding="WinAnsi.enc")
cx<-1.4
cx2<-1.4
cx3<-1.1
cx4<-1.2
cl<-1

mat<-c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,8),rep(6,8))
#mat<-c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),6,6,rep(c(7,8),6),9,9)
layout(matrix(mat, length(mat)/2, 2, byrow = TRUE), widths=c(1))
#layout.show(n=10)
par(oma=c(0,0.7,0,0))
par(mai = c(0.1, 0.8, 0.1, 0.8))

xl<-c(1000,0)

plot(-t,obl,xlim=xl,col=4,type="l",ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F)
mtext(expression(paste(epsilon, "(\u00B0)" )), side = 2, line = 2.8, col=4, cex=cx)
axis(side=2, cex.axis=cx)
text(970,24.5,"(a)",cex=cx2,font=2,xpd=NA) 
text(820,22.4,"obliquity",cex=cx,col=4)

par(mai = c(0, 0.8, 0, 0.8))
plot(-t,-esin,xlim=xl,ylim=c(0.06,-0.06),col=3,type="l",ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F)
lines(-t,-ecc,lty=1,col=6,lwd=1.5)
text(970,0.07,"(b)",cex=cx2,font=2,xpd=NA)
axis(side=4, cex.axis=cx)
mtext("Climatic precession", side = 4, line = 3, col=3, cex=cx3)
text(760,-0.053,"eccentricity",cex=cx,col=6)
text(410,0.054,"climatic precession",cex=cx,col=3)

par(mai = c(0, 0.8, 0, 0.8))
plot(-t,ins,xlim=xl,col="darkorange",type="l",ylab="",xlab="", cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F)
mtext(expression(paste("F"["65N"])), side = 2, line = 4.7, col="darkorange", cex=cx3)
mtext(expression(paste("(Wm"^{-2},")")), side = 2, line = 2.5, col="darkorange", cex=cx3)
axis(side=2, cex.axis=cx)
#axis(side=1, cex.axis=cx)
text(970,560,"(c)",cex=cx2,font=2,xpd=NA) 
text(750,560,"Summer Insolation at 65N",cex=cx,col="darkorange")

par(mai = c(0, 0.8, 0, 0.8))
plot(-t,y,xlim=xl,ylim=c(5.1,3.1),col=1,type="l",ylab="",xlab="",cex=cx,cex.lab=cx,cex.axis=cx,xaxs="i",bty = "n",axes=F)
lines(-t,y2+0.6,col=2)
lines(-t,y3+4.15,col=4)
mtext(expression(paste(delta^{18},"O (\u2030)")), side = 4, line = 3.2, col=1, cex=cx3)
axis(side=4, cex.axis=cx)
text(970,3.22,"(d)",cex=cx2,font=2,xpd=NA)
legend(1900,4.4,c("LR04"),col=1,lty=1,lwd=2,cex=cx,bty="n")
mtext("Age (kyr BP)", side = 1, line = 2.5, col=1, cex=cx3)
legend(960,2.9,legend=c("LR04"),cex=cx4,col=c(1),lty=1,bty="n",lwd=1,xpd = TRUE,text.col=c(1),ncol=3)
legend(830,2.9,legend=c("LR04 untuned", "Huybers 07"),cex=cx4,col=c(2,4),lty=1,bty="n",lwd=1,xpd = TRUE,text.col=c(2,4),ncol=3)
axis(side=1, cex.axis=cx)

par(mai = c(0.25, 0.8, 0.5, 0.9))
plot(spe$freq,2*spe$spec,type="l",xlim=c(0,0.053),ylim=c(0,0.065),col=6,lwd=1, cex=cx,cex.lab=cx,cex.axis=cx,ylab="",xlab="",bty = "n",axes=F) 
axis(side=2, cex.axis=cx)
mtext(expression(paste("PSD (ecc.)")), side = 2, line = 4.3, col=6, cex=cx3)
mtext(expression(paste("(kyr)")), side = 2, line = 2.6, col=6, cex=cx3)
par(new=TRUE)
plot(spi$freq,2*spi$spec,type="l",xlim=c(0,0.053),ylim=c(0,2.0e+5),col="darkorange",lwd=1.4, cex=cx,cex.lab=cx,cex.axis=cx,ylab="",xlab="",bty = "n",axes=F)
abline(v=1/95,lty=2)
abline(v=1/124,lty=2)
abline(v=1/405,lty=2)
abline(v=1/41,lty=2)
abline(v=1/19,lty=2)
abline(v=1/23.7,lty=2)
abline(v=1/22.4,lty=2)
text(1/405,1.8e+5,expression(paste(italic("405"))),cex=cx,col=cl)
text(1/124-0.0015,0.9e+5,expression(paste(italic("124"))),cex=cx,col=cl)
text(1/95,1.3e+5,expression(paste(italic("95"))),cex=cx,col=cl)
text(1/41,1.0e+5,expression(paste(italic("41"))),cex=cx,col=cl)
text(1/23.7-0.002,1.8e+5,expression(paste(italic("23.7"))),cex=cx,col=cl)
text(1/22.4+0.002,1.6e+5,expression(paste(italic("22.4"))),cex=cx,col=cl)
text(1/19,1.2e+5,expression(paste(italic("19"))),cex=cx,col=cl)
axis(side=4, cex.axis=cx)
axis(side=1, cex.axis=cx)
mtext(expression(paste("PSD (F"["65N"],")")), side = 4, line = 3.1, col="darkorange", cex=cx3)
mtext(expression(paste("(W"^{2}," m"^{-4}," kyr)")), side = 4, line = 5, col="darkorange", cex=cx3)
#mtext(expression(paste("Frequency (kyr"^{-1},")")), side = 1, line = 3.2, col=1, cex=cx3)
text(0,2e+5,"(e)",cex=cx2,font=2,xpd=NA) 

par(mai = c(0.6, 0.8, 0.15, 0.9))
plot(sp$freq,2*sp$spec,type="l",xlim=c(0,0.053),ylim=c(0,65),col=1,lwd=1, cex=cx,cex.lab=cx,cex.axis=cx,ylab="",xlab="",bty = "n",axes=F)
lines(sp2$freq,2*sp2$spec,col=2)
lines(sp3$freq,2*sp3$spec,col=4)
abline(v=1/95,lty=2)
abline(v=1/124,lty=2)
abline(v=1/405,lty=2)
abline(v=1/41,lty=2)
abline(v=1/19,lty=2)
abline(v=1/23.7,lty=2)
abline(v=1/22.4,lty=2)
text(1/95,62,expression(paste(italic("95"))),cex=cx,col=cl)
text(1/124-0.0015,42,expression(paste(italic("124"))),cex=cx,col=cl)
text(1/41,50,expression(paste(italic("41"))),cex=cx,col=cl)
text(1/23.7-0.002,50,expression(paste(italic("23.7"))),cex=cx,col=cl)
text(1/22.4+0.002,42,expression(paste(italic("22.4"))),cex=cx,col=cl)
text(1/19,50,expression(paste(italic("19"))),cex=cx,col=cl)
text(1/405,50,expression(paste(italic("405"))),cex=cx,col=cl)
axis(side=2, cex.axis=cx)
axis(side=1, cex.axis=cx)
mtext(expression(paste("PSD (",delta^{18},"O)")), side = 2, line = 4.3, col=1, cex=cx3)
mtext(expression(paste("(\u2030"^2," kyr)")), side = 2, line = 2.6, col=1, cex=cx3)
mtext(expression(paste("Frequency (kyr"^{-1},")")), side = 1, line = 3.2, col=1, cex=cx3)
legend(0.011,55,legend=c("LR04", "LR04 untuned", "Huybers 07"),cex=cx4,col=c(1,2,4),lty=1,bty="n",lwd=1,xpd = TRUE,text.col=c(1,2,4))
text(0,65,"(f)",cex=cx2,font=2,xpd=NA) 
dev.off()
