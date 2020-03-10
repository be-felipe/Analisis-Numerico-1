#QUIZ ANALISIS NÚMERICO

options(digits=6)

install.packages("PolynomF")#instalar paquete 
help(PolynomF)
library(PolynomF)
########
#PUNTO 2
########

x=c(0,1,2) 
y=c(10,15,5)
grado=3
plot(x,y,xlim=c(0,2),ylim=c(0,16),col="red",xlab="x",ylab="y",cex=1,pch=19)##valores iniciales
res=poly_calc(x,y)##Polinomio interpolante
par(new=TRUE)
plot(res,xlim=c(0,2),ylim=c(0,16),col="green",xlab="x",ylab="y",main="Polinomio interpolante",cex=1,pch=19)
