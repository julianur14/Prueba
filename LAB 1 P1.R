#LABORATORIO 1 PARTE 1
set.seed(1)
datos <- rexp(25, 3)
hist(datos, col = "red")
cv <- sd(datos)/mean(datos)
cv

library(MASS)
fitdistr(datos, "exponential")

#Modelo gamma-exponencial
n = length(datos)
sx = sum(datos)
a = 10
b = 100
ap = a+n #nueva gamma dist a posteriori hecha en folio
bp = 1/((1/b)+sx)

#Calculamos media, desv. típica a posteriori y un IC al 80%
mp = ap*bp
sp = sqrt(ap*bp^2)
NP = 0.8
lb = qgamma((1-NP)/2, ap, bp) #cuantil 0.1
ub = qgamma((1+NP)/2, ap, bp) #cuantil 0.9
lb
ub

#Lo mismo por simulación
muestra = rgamma(10000, ap, bp)
mp2 = mean(muestra)
sp2 = sd(muestra)
muestrasort = sort(muestra)
lb2 = muestrasort[1000]
ub2 = muestrasort[9000]
lb2
ub2

#Predicciones
N = 1000
lambda = rgamma(1000, ap, bp)
xpred = matrix(0, N, 1)
for (i in 1:N) {
  xpred[i] = rexp(1, lambda[i])
}

hist(xpred)
mean(xpred)
sd(xpred)
xpredsort = sort(xpred)
xpredsort[950]
lb = xpredsort[20]
ub = xpredsort[980]
lb
ub


#Modelo beta binomial
arceut = data2
nodata = length(arceut)
noexitos = sum(arceut)

#Sup que a priori p sigue una beta 3, 3
a = 3
b = 3

#Parámetro dist a posteriori y momentos
ap = a +noexitos
bp = b + (nodata-noexitos)
meanp = ap/(ap+bp)
medianp = qbeta(0.5, ap, bp)
modep = (ap-1)/(ap+bp-2)

#IC 0.8
lb<-qbeta(0.1,ap,bp)
up<-qbeta(0.9,ap,bp)

#Cont hip
pbeta(0.6,ap,bp)

#Ver ejemplos en campus