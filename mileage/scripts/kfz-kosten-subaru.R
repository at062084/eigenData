s=read.csv(file="kfz-kosten.csv", header=TRUE, sep=" ", colClasses=c("character",rep("numeric",4),"factor"))
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

