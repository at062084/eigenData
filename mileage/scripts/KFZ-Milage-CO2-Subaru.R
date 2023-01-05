# old version used in 2012
s=read.csv(file="kfz-kosten-2015.csv", header=TRUE, sep=",", colClasses=c("character",rep("numeric",4),"factor","factor"),na.strings = "NA")
s$Date=strptime(s$Date,"%d.%m.%Y")

main="Subaru 2.5XT Bj 2005. Verbrauch auf 100km"
xlab="Histogramm: Verbrauch in Liter unmodifiziert (0.5 Liter pro bin)"
sub="Rote Striche: Verbrauch seit Tuning (8 TankStops) Strichliert: Mittelwerte"
hist(s$Liters[1:62]/s$CurrentKm[1:62]*100, breaks=seq(8,18,by=0.5), main=main, xlab=xlab, sub=sub)
for (i in 63:70) abline(v=s$Liters[i]/s$CurrentKm[i]*100, col="red", lwd=2)
mu=mean(s$Liters[1:62]/s$CurrentKm[1:62]*100)
abline(v=mu, col="black", lty=2, lwd=3)
mm=mean(s$Liters[63:70]/s$CurrentKm[63:70]*100)
abline(v=mm, col="red", lty=2, lwd=3)


# new version for 201507 data
library(lattice)

s=read.csv(file="kfz-kosten-2015.csv", header=TRUE, sep=",", colClasses=c("character",rep("numeric",4),"factor","factor"),na.strings = "NA")
s$Date=strptime(s$Date,"%d.%m.%Y")

s$lpk = s$Liter/s$kmTank*100
summary(s)

bwplot(s$lpk~s$bTuned)

sOrig = subset(s,subset=bTuned==0)
sTuned = subset(s,subset=bTuned==1)
summary(sTuned)

lpk = seq(9.25,19.25,by=.5)
hOrig = hist(sOrig$lpk, breaks=lpk)
hTuned = hist(sTuned$lpk, breaks=lpk)

mids = hOrig$mids

main="Benzinverbrauch Subaru Forester 2.5XT 2005"
sub="Blau=original (105 Tanks)  Rot=getuned (93 Tanks)"
xlab="Verbrauch in Liter/100km (median: strichliert, mittel: durchgezogen)"
ylab="Anteil an Tankstops [%]"
plot(mids,hOrig$density/2*100, type="l", pch=16, col="blue", main=main, sub=sub, xlab=xlab, ylab=ylab, ylim=c(0,25))
points(mids,hOrig$density/2*100, col="blue", pch=16)
lines(mids,hTuned$density/2*100, col="red")
points(mids,hTuned$density/2*100, col="red", pch=16)
abline(v=9:19, lty=1, col="darkgrey")
abline(v=9:19+.5, lty=3, col="grey")
abline(h=seq(0,25,by=5), col="grey")
axis(1, at=9:19)

abline(v=mean(sOrig$lpk, na.rm=TRUE), lwd=2, col="blue")
abline(v=mean(sTuned$lpk, na.rm=TRUE), lwd=2, col="red")
abline(v=median(sOrig$lpk, na.rm=TRUE), lwd=2, col="blue", lty=2)
abline(v=median(sTuned$lpk, na.rm=TRUE), lwd=2, col="red", lty=2)

