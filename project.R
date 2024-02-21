library(rootSolve)
library("maxLik")
library(nortest)
library(tseries)
library(ggplot2)
library(fitdistrplus)
library(logspline)
library("DescTools")
options(scipen = 999)

#Dane do modelu
dane<-read.csv("dane_nowe.csv")

#przeksztalcenia
dane$wiek_budynku<-2023-dane$rok_budowy
dane$cena_m2<-log(dane$cena_m2)
N = length(dane$cena_m2)
y = dane$cena_m2
x1 = dane$remont
x2 = dane$wykonczenie
x3 = dane$winda
x4 = dane$balkon
x5 = dane$taras
x6 = dane$wiek_budynku
x7 = dane$spoldzielcze
x8 = dane$powierzchnia
x9 = dane$pietro
x10 = dane$pietro_max
x11 = dane$odleglosc_metro
x12 = dane$odleglosc_centrum
x13 = dane$agencja

#histogram
par(mfrow = c(1, 2))
hist(y,
     col="orange", 
     border="black",
     prob = TRUE, 
     xlab = "log(cena_m2)",
     main="Histogram log(cena_m2)",
     breaks=20)
lines(density(na.omit(y)), 
      lwd = 2, 
      col = "green")
#boxplot
boxplot(y, xlab = "log(cena_m2)", main="Boxplot log(cena_m2)",col="orange")

#Test Jarque Bera
jarque.bera.test(y)

#Szukanie właściwego rozkładu dla cena_m2, wykres Cullena i Freya
par(mfrow = c(1, 1))
descdist(data = y, discrete = FALSE, boot=1000)

#Oszacowanie parametrów
est=fitdist(y,"logis")
est
PlotQQ(y, qdist = function(p) qlogis(p, location = 9.7559653, scale = 0.1491862),
       conf.level=NA, main="Q-Q plot dla rozkładu logistycznego na oszacowanych parametrach")

# modele
eq1<-cena_m2~remont + wykonczenie + winda + balkon + taras + wiek_budynku + spoldzielcze + powierzchnia  + pietro + pietro_max  + odleglosc_metro + odleglosc_centrum + agencja
eq2<-y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13

#MNK
model.mnk<-lm(eq1, data=dane)
summary(model.mnk)

#Metoda momentów
uklad.rownan = function(b) {
  b0 = b[1]
  b1 = b[2]
  b2 = b[3]
  b3 = b[4]
  b4 = b[5]
  b5 = b[6]
  b6 = b[7]
  b7 = b[8]
  b8 = b[9]
  b9 = b[10]
  b10 = b[11]
  b11 = b[12]
  b12 = b[13]
  b13 = b[14]
  
  r0 = sum(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13)
  r1 = sum(x1*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r2 = sum(x2*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r3 = sum(x3*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r4 = sum(x4*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r5 = sum(x5*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r6 = sum(x6*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r7 = sum(x7*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r8 = sum(x8*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r9 = sum(x9*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r10 = sum(x10*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r11 = sum(x11*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r12 = sum(x12*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  r13 = sum(x13*(y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13))
  return(c(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13))
}


wynik = multiroot(f=uklad.rownan, start= c(1,1,1,1,1,1,1,1,1,1,1,1,1,1))
wynik$root

###### Testowanie hipotez ######

B = wynik$root

# Macierz m
m = cbind(
  (y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13) / N,
  (x1*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x2*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x3*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x4*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x5*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x6*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x7*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x8*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x9*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x10*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x11*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x12*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N,
  (x13*(y-B[1]-B[2]*x1-B[3]*x2-B[4]*x3-B[5]*x4-B[6]*x5-B[7]*x6-B[8]*x7-B[9]*x8-B[10]*x9-B[11]*x10-B[12]*x11-B[13]*x12-B[14]*x13))/ N
)


# Macierz J
J = t(m)%*%m

# Obliczenie macierzy D - w kolumnach kolejne warunki, w wierszach kolejne pochodne po B[j]
D = matrix(c(
  -1, -mean(x1), -mean(x2), -mean(x3), -mean(x4), -mean(x5), -mean(x6), -mean(x7),  -mean(x8),  -mean(x9),  -mean(x10),  -mean(x11),  -mean(x12),  -mean(x13),
  -mean(x1), -mean(x1*x1), -mean(x1*x2), -mean(x1*x3), -mean(x1*x4), -mean(x1*x5), -mean(x1*x6), -mean(x1*x7), -mean(x1*x8), -mean(x1*x9), -mean(x1*x10), -mean(x1*x11), -mean(x1*x12), -mean(x1*x13),
  -mean(x2), -mean(x2*x1), -mean(x2*x2), -mean(x2*x3), -mean(x2*x4), -mean(x2*x5), -mean(x2*x6), -mean(x2*x7), -mean(x2*x8), -mean(x2*x9), -mean(x2*x10), -mean(x2*x11), -mean(x2*x12), -mean(x2*x13),
  -mean(x3), -mean(x3*x1), -mean(x3*x2), -mean(x3*x3), -mean(x3*x4), -mean(x3*x5), -mean(x3*x6), -mean(x3*x7), -mean(x3*x8), -mean(x3*x9), -mean(x3*x10), -mean(x3*x11), -mean(x3*x12), -mean(x3*x13),
  -mean(x4), -mean(x4*x1), -mean(x4*x2), -mean(x4*x3), -mean(x4*x4), -mean(x4*x5), -mean(x4*x6), -mean(x4*x7), -mean(x4*x8), -mean(x4*x9), -mean(x4*x10), -mean(x4*x11), -mean(x4*x12), -mean(x4*x13),
  -mean(x5), -mean(x5*x1), -mean(x5*x2), -mean(x5*x3), -mean(x5*x4), -mean(x5*x5), -mean(x5*x6), -mean(x5*x7), -mean(x5*x8), -mean(x5*x9), -mean(x5*x10), -mean(x5*x11), -mean(x5*x12), -mean(x5*x13),
  -mean(x6), -mean(x6*x1), -mean(x6*x2), -mean(x6*x3), -mean(x6*x4), -mean(x6*x5), -mean(x6*x6), -mean(x6*x7), -mean(x6*x8), -mean(x6*x9), -mean(x6*x10), -mean(x6*x11), -mean(x6*x12), -mean(x6*x13),
  -mean(x7), -mean(x7*x1), -mean(x7*x2), -mean(x7*x3), -mean(x7*x4), -mean(x7*x5), -mean(x7*x6), -mean(x7*x7), -mean(x7*x8), -mean(x7*x9), -mean(x7*x10), -mean(x7*x11), -mean(x7*x12), -mean(x7*x13),
  -mean(x8), -mean(x8*x1), -mean(x8*x2), -mean(x8*x3), -mean(x8*x4), -mean(x8*x5), -mean(x8*x6), -mean(x8*x7), -mean(x8*x8), -mean(x8*x9), -mean(x8*x10), -mean(x8*x11), -mean(x8*x12), -mean(x8*x13),
  -mean(x9), -mean(x9*x1), -mean(x9*x2), -mean(x9*x3), -mean(x9*x4), -mean(x9*x5), -mean(x9*x6), -mean(x9*x7), -mean(x9*x8), -mean(x9*x9), -mean(x9*x10), -mean(x9*x11), -mean(x9*x12), -mean(x9*x13),
  -mean(x10), -mean(x10*x1), -mean(x10*x2), -mean(x10*x3), -mean(x10*x4), -mean(x10*x5), -mean(x10*x6), -mean(x10*x7), -mean(x10*x8), -mean(x10*x9), -mean(x10*x10), -mean(x10*x11), -mean(x10*x12), -mean(x10*x13),
  -mean(x11), -mean(x11*x1), -mean(x11*x2), -mean(x11*x3), -mean(x11*x4), -mean(x11*x5), -mean(x11*x6), -mean(x11*x7), -mean(x11*x8), -mean(x11*x9), -mean(x11*x10), -mean(x11*x11), -mean(x11*x12), -mean(x11*x13),
  -mean(x12), -mean(x12*x1), -mean(x12*x2), -mean(x12*x3), -mean(x12*x4), -mean(x12*x5), -mean(x12*x6), -mean(x12*x7), -mean(x12*x8), -mean(x12*x9), -mean(x12*x10), -mean(x12*x11), -mean(x12*x12), -mean(x12*x13),
  -mean(x13), -mean(x13*x1), -mean(x13*x2), -mean(x13*x3), -mean(x13*x4), -mean(x13*x5), -mean(x13*x6), -mean(x13*x7), -mean(x13*x8), -mean(x13*x9), -mean(x13*x10), -mean(x13*x11), -mean(x13*x12), -mean(x13*x13)
), nrow = 14, ncol = 14)


vcov = (solve(D)%*%J%*%t(solve(D)))

std.err = sqrt(diag(vcov))
std.err

# Testujemy, że beta1 = 0
z_test1 = (B[1] - 0)/std.err[1]
p_value1 <- 2 * pt(abs(z_test1), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta2 = 0
z_test2 = (B[2] - 0)/std.err[2]
p_value2 <- 2 * pt(abs(z_test2), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta3 = 0
z_test3 = (B[3] - 0)/std.err[3]
p_value3 <- 2 * pt(abs(z_test3), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta4 = 0
z_test4 = (B[4] - 0)/std.err[4]
p_value4 <- 2 * pt(abs(z_test4), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta5 = 0
z_test5 = (B[5] - 0)/std.err[5]
p_value5 <- 2 * pt(abs(z_test5), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta6 = 0
z_test6 = (B[6] - 0)/std.err[6]
p_value6 <- 2 * pt(abs(z_test6), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta7 = 0
z_test7 = (B[7] - 0)/std.err[7]
p_value7 <- 2 * pt(abs(z_test7), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta8 = 0
z_test8 = (B[8] - 0)/std.err[8]
p_value8 <- 2 * pt(abs(z_test8), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta9 = 0
z_test9 = (B[9] - 0)/std.err[9]
p_value9 <- 2 * pt(abs(z_test9), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta10 = 0
z_test10 = (B[10] - 0)/std.err[10]
p_value10 <- 2 * pt(abs(z_test10), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta11 = 0
z_test11 = (B[11] - 0)/std.err[11]
p_value11 <- 2 * pt(abs(z_test11), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta12 = 0
z_test12 = (B[12] - 0)/std.err[12]
p_value12 <- 2 * pt(abs(z_test12), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta13 = 0
z_test13 = (B[13] - 0)/std.err[13]
p_value13 <- 2 * pt(abs(z_test13), df = N - 1, lower.tail = FALSE)

# Testujemy, że beta14 = 0
z_test14 = (B[14] - 0)/std.err[14]
p_value14 <- 2 * pt(abs(z_test14), df = N - 1, lower.tail = FALSE)

c(z_test1, z_test2, z_test3, z_test4, z_test5, z_test6, z_test7, z_test8, z_test9, z_test10, z_test11, z_test12, z_test13, z_test14)
c(p_value1, p_value2, p_value3, p_value4, p_value5, p_value6, p_value7, p_value8, p_value9, p_value10, p_value11, p_value12, p_value13, p_value14)

#Model metodą największej wiarygodności
logLik<-function(b){
  b0 = b[1]
  b1 = b[2]
  b2 = b[3]
  b3 = b[4]
  b4 = b[5]
  b5 = b[6]
  b6 = b[7]
  b7 = b[8]
  b8 = b[9]
  b9 = b[10]
  b10 = b[11]
  b11 = b[12]
  b12 = b[13]
  b13 = b[14]
  sigma = b[15]
  -1/2*N*log(2*pi)-N*log(sigma)-1/(2*sigma^2)*sum((y-b0-b1*x1-b2*x2-b3*x3-b4*x4-b5*x5-b6*x6-b7*x7-b8*x8-b9*x9-b10*x10-b11*x11-b12*x12-b13*x13)^2)
}

wynik=maxLik(logLik, start=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
wynik$estimate

# Wyświetlenie oszacowanych parametrów i błędów standardowych
vcov <- solve(-wynik$hessian)
standard_errors <- sqrt(diag(vcov))
z.test=(wynik$estimate / standard_errors)
p_values <- 2 * (1-pnorm(abs(z.test)))
cat("Oszacowane parametry:\n", wynik$estimate, "\n")
cat("Błędy standardowe:\n", standard_errors, "\n")
cat("Statystyki Z:\n", z.test, "\n")
cat("P-wartości:\n", p_values, "\n")
