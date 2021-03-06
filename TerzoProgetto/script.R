################### TERZO PROGETTO STATISTICA #########################
# Librerie ----
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(readr)
library(zoo)
library(ggfortify)

# Grafico Interattivo ----

#Carico il dataset
tabella <- read_csv("tabella.csv", col_types = cols(`CaseVendute` = col_number(), 
                                                           Data = col_date(format = "%Y-%m-%d")))

# Area Chart Interattiva
p <- tabella %>%
  ggplot( aes(x=Data, y=CaseVendute)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Case Monofamiliari Vendute (Migliaia)") +
  theme_ipsum()

p <- ggplotly(p)
p

# Costruzione e Analisi Time Series ----
case = ts(data = tabella$CaseVendute, start = 1963, end = c(2019,10), deltat = 1/12)

# ACF
acf(case, 53, main = "Autocorrelazione Case Vendute", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(case), 53, main = "Autocorrelazione Case Vendute (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

#Plot Anni Sovrapposti (Fino allo scorso anno)
m_case=matrix(case[1:672],nrow=12,ncol=56)

ts.plot(m_case,col=rainbow(56),  main = "Confronto Anni", ylab = "Case Monofamiliari Vendute", xlab="Mese")

ts.plot(scale(m_case),col=rainbow(56),  main = "Confronto Stagionalità negli Anni", ylab = "Case Monofamiliari Vendute (Normalizzato)", xlab="Mese")
lines(rowMeans(scale(m_case)),lwd=3,col="black")

case.sd=vector("numeric",12)
for(i in 1:12){
       case.sd[i]=sd(scale(m_case[i,]))
   }
case.m=rowMeans(scale(m_case))
plot(case.m,pch=20,type="b",ylim=range(c(-2.5,2.5)),  main = "Margini della Stagionalità", ylab = "Case Monofamiliari Vendute (Normalizzato)", xlab="Mese")
arrows(1:12,case.m-case.sd,1:12,case.m+case.sd,length=0.02,angle=90,
                 code=3,col="green3")
points(case.m+case.sd,type="b",pch=20,col="gray")
points(case.m-case.sd,type="b",pch=20,col="gray")


# Decomposizione ----

# Caso Additivo
case.dea = decompose(case, type = "additive")
autoplot(case.dea,main="Decomposizione Additiva")

# Caso Moltiplicativo
case.dem = decompose(case, type = "multiplicative")
autoplot(case.dem,main="Decomposizione Moltiplicativa")

#Ho ottimizzato le finestre in modo da minimizzare la deviazione standard dei residui
# Stl Additivo
case.stla = stl(case,s.window=13)
autoplot(case.stla,main="STL Additivo")

# Stl Moltiplicativo
case.stlm = stl(log(case),s.window=19)
autoplot(case.stlm, main="STL Moltiplicativo")

# Confronto Stagionalità
plot(case.stla$time.series[,1],col=alpha(rgb(0,1,0), 0.5),main = "Confronto Stagionalità", ylab = "Stagionalità", xlab="Anno")
lines(case.dea$seasonal,col=alpha(rgb(1,0,0), 1))
lines(mean(case.dem$trend,na.rm=T)*(case.dem$seasonal-1),col=alpha(rgb(0,0,1), 0.5))
lines(case.stlm$time.series[,1]*mean(case.stla$time.series[,2],na.rm=T),col=alpha(rgb(0,1,1), 0.5))
legend(
  "bottomright",
  legend = c("Dec. Additiva", "Dec. Moltiplicativa","STL Additivo","STL Moltiplicativo"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5), alpha(rgb(0,1,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Confronto Residui
plot(case.dea$random,col=alpha(rgb(1,0,0), 0.5),main = "Confronto Residui", ylab = "Residui", xlab="Anno")
lines(mean(case.dem$trend,na.rm=T)*(case.dem$random-1),col=alpha(rgb(0,0,1), 0.5))
lines(case.stla$time.series[,3],col=alpha(rgb(0,1,0), 0.5))
lines(case.stlm$time.series[,3]*mean(case.stla$time.series[,2],na.rm=T),col=alpha(rgb(0,1,1), 0.5))
legend(
  "bottomright",
  legend = c("Dec. Additiva", "Dec. Moltiplicativa","STL Additivo","STL Moltiplicativo"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5), alpha(rgb(0,1,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Confronto Trend
plot(case.dea$trend,col=alpha(rgb(1,0,0), 0.5),main = "Confronto Trend", ylab = "Trend", xlab="Anno")
lines(case.dem$trend,col=alpha(rgb(0,0,1), 0.5))
lines(case.stla$time.series[,2],col=alpha(rgb(0,1,0), 0.5))
lines(exp(case.stlm$time.series[,2]),col=alpha(rgb(0,1,1), 0.5))
legend(
  "topleft",
  legend = c("Dec. Additiva", "Dec. Moltiplicativa","STL Additivo","STL Moltiplicativo"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5), alpha(rgb(0,1,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Test sui residui ----

# Additivo
smoothScatter(na.omit(case.dea$random),main = "Residui Additivi", ylab = "Residui", xlab="Anno")

hist(case.dea$random,30,freq=F,main = "Istogramma dei Residui Additivi", ylab = "Densità", xlab="Residui")
lines(density(na.omit(case.dea$random)),col="blue")
lines(sort(na.omit(case.dea$random)),dnorm(sort(na.omit(case.dea$random)),mean(na.omit(case.dea$random)),sd(na.omit(case.dea$random))),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

qqnorm(case.dea$random,main = "Normal Q-Q Plot Residui Additivi")
qqline(case.dea$random)

acf(na.omit(case.dea$random), 50, main = "Autocorrelazione Residui Additivi", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(na.omit(case.dea$random)), 50, main = "Autocorrelazione Residui Additivi (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

shapiro.test(case.dea$random)
sd(acf(na.omit(case.dea$random), plot = F)$acf)
1 - var(window(case.dea$random,c(1963,7),c(2019,4)))/var(window(case,c(1963,7),c(2018,7)))

# Moltiplicativo

smoothScatter(case.dem$random,main = "Residui Moltiplicativi", ylab = "Residui", xlab="Anno")

#I residui moltiplicativi sono l'esponenziale dei residui additivi sui logaritmi dei dati
case.dem.r = na.omit(log(case.dem$random))

hist(case.dem.r,30,freq=F,main = "Istogramma dei Residui Moltiplicativi", ylab = "Densità", xlab="Residui")
lines(density(case.dem.r),col="blue")
lines(sort(case.dem.r),dnorm(sort(case.dem.r),mean(case.dem.r),sd(case.dem.r)),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

qqnorm(case.dem.r,main = "Normal Q-Q Plot Residui Moltiplicativi")
qqline(case.dem.r)

acf(case.dem.r, 50, main = "Autocorrelazione Residui Moltiplicativi", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(case.dem.r), 50, main = "Autocorrelazione Residui Moltiplicativi (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

shapiro.test(case.dem.r)
sd(acf(case.dem.r, plot = F)$acf)
1 - var(window(case.dem.r,c(1963,7),c(2019,4)))/var(window(log(case),c(1963,7),c(2018,7)))


# STL Additivo
smoothScatter(case.stla$time.series[,3],main = "Residui STL Additivo", ylab = "Residui", xlab="Anno")

hist(case.stla$time.series[,3],30,freq=F,main = "Istogramma dei Residui STL Additivo", ylab = "Densità", xlab="Residui")
lines(density(na.omit(case.stla$time.series[,3])),col="blue")
lines(sort(na.omit(case.stla$time.series[,3])),dnorm(sort(na.omit(case.stla$time.series[,3])),mean(na.omit(case.stla$time.series[,3])),sd(na.omit(case.stla$time.series[,3]))),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

qqnorm(case.stla$time.series[,3],main = "Normal Q-Q Plot Residui STL Additivo")
qqline(case.stla$time.series[,3])

acf(na.omit(case.stla$time.series[,3]), 50,main = "Autocorrelazione Residui STL Additivo", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(na.omit(case.dem$random)), 50, main = "Autocorrelazione Residui STL Additivo (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

shapiro.test(case.stla$time.series[,3])
sd(acf(case.stla$time.series[,3], plot = F)$acf)
1 - var(window(case.stla$time.series[,3],1963,c(2019,10)))/var(window(case,c(1963,7),c(2018,7)))

# STL Moltiplicativo
smoothScatter(case.stlm$time.series[,3],main = "Residui STL Moltiplicativo", ylab = "Residui", xlab="Anno")

# I residui in questo caso sono centrati in zero perchè la decomposizione è stata fatta sul logaritmo della serie
case.stam.r = log(na.omit(case.stlm$time.series[,3]) + 1)

hist(case.stam.r,30,freq=F,main = "Istogramma dei Residui STL Moltiplicativo", ylab = "Densità", xlab="Residui")
lines(density(case.stam.r),col="blue")
lines(sort(case.stam.r),dnorm(sort(case.stam.r),mean(case.stam.r),sd(case.stam.r)),col="red")
legend(
  "topleft",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

qqnorm(case.stam.r,main = "Normal Q-Q Plot Residui STL Moltiplicativo")
qqline(case.stam.r)

acf(case.stam.r, 50,main = "Autocorrelazione Residui STL Moltiplicativo", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(case.stam.r), 50, main = "Autocorrelazione Residui STL Moltiplicativo (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

shapiro.test(case.stam.r)
sd(acf(case.stam.r, plot = F)$acf)
1 - var(case.stam.r)/var(log(case))

# Holt Winters ----

# Automatico alpha: 0.6646637 beta : 0 gamma: 0.4901472
case.hwAuto = HoltWinters(case, seasonal = "mult")
autoplot(case.hwAuto$fitted)

plot(case.hwAuto$fitted[,1], xlim=c(1964,2021), col=alpha(rgb(1,0,0), 0.5),main = "Holt-Winters con Parametri Automatici", ylab = "Case Monofamiliari Vendute", xlab="Anno")
lines(case, col=alpha(rgb(0,0,1), 0.5))
lines(predict(case.hwAuto,24),col=alpha(rgb(1,0,0), 0.5))
polygon(c(time(predict(case.hwAuto,24)),rev(time(predict(case.hwAuto,24)))), c(predict(case.hwAuto,24) + quantile(residuals(case.hwAuto), 0.975),rev(predict(case.hwAuto,24) + quantile(residuals(case.hwAuto), 0.025))), col = alpha(rgb(0,1,0), 0.3),border = alpha(rgb(0,0,0), 0))   
segments(2019 + ((1/12)*10),0,2019+ ((1/12)*10),130, alpha(rgb(1,0,1), 0.5))
legend(
  "topleft",
  legend = c("Holt-Winters", "Osservazioni", "Margini Previsione"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Ottimizzato 
x=1:36
coef(lm(case[1:36]~x))

case.hwOtt = HoltWinters(case, alpha = 0.4, beta = 0.05, gamma = 0.1, seasonal = "mult",l.start=47.3, b.start=-0.00232)
autoplot(case.hwOtt$fitted)

plot(case.hwOtt$fitted[,1], xlim=c(1964,2021), col=alpha(rgb(1,0,0), 0.5),main = "Holt-Winters con Parametri Ottimizzati", ylab = "Case Monofamiliari Vendute", xlab="Anno")
lines(case, col=alpha(rgb(0,0,1), 0.5))
lines(predict(case.hwOtt,24),col=alpha(rgb(1,0,0), 0.5))
lines(predict(case.hwOtt,24)+quantile(residuals(case.hwOtt),0.05),col=alpha(rgb(0,1,0), 0.5))
lines(predict(case.hwOtt,24)+quantile(residuals(case.hwOtt),0.95),col=alpha(rgb(0,1,0), 0.5))
segments(2019 + ((1/12)*10),0,2019+ ((1/12)*10),130, alpha(rgb(1,0,1), 0.5))
legend(
  "topleft",
  legend = c("Holt-Winters", "Osservazioni", "Margini Previsione"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5)),
  lwd = 2,
  cex = 0.6
)


# Confronto con STL ----
# Automatico
#Trend
ts.plot(exp(case.stlm$time.series[,2]),case.hwAuto$fitted[,2],col=c(alpha(rgb(0,0,1), 0.5),alpha(rgb(1,0,0), 0.5)),main = "Confroto con Trend STL", ylab = "Case Monofamiliari Vendute", xlab="Anno")
#Stagionalità
ts.plot(case.stlm$time.series[,1]*mean(case.stla$time.series[,2],na.rm=T),mean(case.hwAuto$fitted[,2], na.rm = TRUE) * (case.hwAuto$fitted[,4] - 1),col=c(alpha(rgb(0,0,1), 0.5),alpha(rgb(1,0,0), 0.5)),main = "Confroto con Stagionalità STL", ylab = "Case Monofamiliari Vendute", xlab="Anno")
legend(
  "topleft",
  legend = c("Holt-Winters p. automatici", "Osservazioni"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Ottimizzato
#Trend
ts.plot(exp(case.stlm$time.series[,2]),case.hwOtt$fitted[,2],col=c(alpha(rgb(0,0,1), 0.5),alpha(rgb(1,0,0), 0.5)),main = "Confroto con Trend STL", ylab = "Case Monofamiliari Vendute", xlab="Anno")
#Stagionalità
ts.plot(case.stlm$time.series[,1]*mean(case.stla$time.series[,2],na.rm=T),mean(case.hwOtt$fitted[,2], na.rm = TRUE) * (case.hwOtt$fitted[,4] - 1),col=c(alpha(rgb(0,0,1), 0.5),alpha(rgb(1,0,0), 0.5)),main = "Confroto con Stagionalità STL", ylab = "Case Monofamiliari Vendute", xlab="Anno")
legend(
  "topright",
  legend = c("Holt-Winters p. ottimizzati", "Osservazioni"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Autoregressione ----
pacf(case, 70, frequency = 12, main = "Autocorrelazione Parziale", ylab = "PACF", xlab="Ritardo (Anni)")

length(case)

# Autoregressione Manuale
lag = 46
m_case = matrix(nrow = length(case)-lag, ncol = lag)
for (i in 1:lag) {
  m_case[,i] = case[(i):(length(case)-lag-1+ i)]
}
m_case <- data.frame(m_case)
case.lm <- lm(X46 ~ .-X32-X29-X43-X11-X21-X14-X4-X37-X13-X7-X2-X5-X15-X16-X35-X19-X30-X18-X23-X24-X1-X3-X42-X44-X8-X6-X40-X39-X38-X36, data = m_case)
summary(case.lm)
case.lm.r = ts(resid(case.lm), frequency = 12, end = c(2019,10))

# Autoregressione con Yule-Walker
case.arYW = ar(case, method = "yule-walker")

# Autoregressione con Minimi Quadrati
case.arMQ = ar(case, method = "ols")

# Confronto Autoregressioni
ts.plot(case,case- case.lm.r,case-case.arYW$resid, case-case.arMQ$resid,col=c(alpha(rgb(0,0,0), 1),alpha(rgb(0,1,0), 0.5), alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)))

# Previsione ----

# Autoregressione Manuale
anni = 2
l = length(case)
p = 1:(l + 12 * anni)
p[1:l] = case
  for (i in 1:(12 * anni)) {
    lags = c(p[l + i - 9],p[l + i - 10],p[l + i - 12],p[l + i - 17],p[l + i - 20],p[l + i - 22],p[l + i - 25],p[l + i - 26],p[l + i - 27],p[l + i - 28],p[l + i - 31],p[l + i - 33],p[l + i - 34],p[l + i - 41],p[l + i - 45])
    p[l + i] = coef(case.lm) %*% c(1,lags)
    } 

case.lm.p = ts(p, frequency = 12, start = 1963)
plot(case, xlim = c(1963,2021), col = alpha(rgb(0,0,1),0.5),main = "Autoregressione con Parametri Ottimizzati", ylab = "Case Monofamiliari Vendute", xlab="Anno")
lines(window(case.lm.p,c(2019,11)), col = alpha(rgb(1,0,0),0.5))
lines(case- case.lm.r, col = alpha(rgb(1,0,0),0.5))
lines(window(case.lm.p,c(2019,11))+quantile(na.omit(residuals(case.lm)),0.05),col=alpha(rgb(0,1,0), 0.5))
lines(window(case.lm.p,c(2019,11))+quantile(na.omit(residuals(case.lm)),0.95),col=alpha(rgb(0,1,0), 0.5))
segments(2019 + ((1/12)*10),0,2019+ ((1/12)*10),130, alpha(rgb(1,0,1), 0.5))
legend(
  "topleft",
  legend = c("Autoregressione", "Osservazioni", "Margini Predizione"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Autoregressione con Yule-Walker
case.arYW.p = predict(case.arYW, n.ahead = 24, se.fit = FALSE)

plot(case, xlim = c(1963,2021), col = alpha(rgb(0,0,1),0.5),main = "Autoregressione con Yule-Walker", ylab = "Case Monofamiliari Vendute", xlab="Anno")
lines(case.arYW.p, col = alpha(rgb(1,0,0),0.5))
lines(case-case.arYW$resid, col = alpha(rgb(1,0,0),0.5))
lines(case.arYW.p+quantile(na.omit(residuals(case.arYW)),0.05),col=alpha(rgb(0,1,0), 0.5))
lines(case.arYW.p+quantile(na.omit(residuals(case.arYW)),0.95),col=alpha(rgb(0,1,0), 0.5))
segments(2019 + ((1/12)*10),0,2019+ ((1/12)*10),130, alpha(rgb(1,0,1), 0.5))
legend(
  "topleft",
  legend = c("Autoregressione", "Osservazioni", "Margini Predizione"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Autoregressione con Minimi Quadrati
case.arMQ.p = predict(case.arMQ, n.ahead = 24, se.fit = FALSE)

plot(case, xlim = c(1963,2021), col = alpha(rgb(0,0,1),0.5),main = "Autoregressione con Minimi Quadrati", ylab = "Case Monofamiliari Vendute", xlab="Anno")
lines(case.arMQ.p, col = alpha(rgb(1,0,0),0.5))
lines(case-case.arMQ$resid, col = alpha(rgb(1,0,0),0.5))
lines(case.arMQ.p+quantile(na.omit(residuals(case.arMQ)),0.05),col=alpha(rgb(0,1,0), 0.5))
lines(case.arMQ.p+quantile(na.omit(residuals(case.arMQ)),0.95),col=alpha(rgb(0,1,0), 0.5))
segments(2019 + ((1/12)*10),0,2019+ ((1/12)*10),130, alpha(rgb(1,0,1), 0.5))
legend(
  "topleft",
  legend = c("Autoregressione", "Osservazioni", "Margini Predizione"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5), alpha(rgb(0,1,0), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Test sui residui ----

# HoltWinters Automatico ----
# Residui
smoothScatter(na.omit(resid(case.hwAuto)),main = "Residui HoltWinters Automatico", ylab = "Residui", xlab="Anno")
acf(na.omit(resid(case.hwAuto)), 50, main = "Autocorrelazione Residui HoltWinters Automatico", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(na.omit(resid(case.hwAuto))), 50, main = "Autocorrelazione HoltWinters Automatico (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

# Istogramma Residui
hist(residuals(case.hwAuto),25,freq=F,main = "Istogramma dei Residui HoltWinters Automatico", ylab = "Densità", xlab="Residui")
lines(density(residuals(case.hwOtt)),col="blue")
lines(sort(na.omit(residuals(case.hwAuto))),dnorm(sort(na.omit(residuals(case.hwAuto))),mean(na.omit(residuals(case.hwAuto))),sd(na.omit(residuals(case.hwAuto)))),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Q-Q Plot
qqnorm(residuals(case.hwAuto),main = "Normal Q-Q Plot HoltWinters Automatico")
qqline(residuals(case.hwAuto))

# Shapiro Test
shapiro.test(residuals(case.hwAuto))

# Varianza Residui/Varianza Osservazioni
1 - var(residuals(case.hwAuto))/var(window(case,1964,c(2019,10)))

# HoltWinters Ottimizzato ----
# Residui
smoothScatter(na.omit(resid(case.hwOtt)),main = "Residui HoltWinters Ottimizzato", ylab = "Residui", xlab="Anno")
acf(na.omit(resid(case.hwOtt)), 50, main = "Autocorrelazione Residui HoltWinters Ottimizzato", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(na.omit(resid(case.hwOtt))), 50, main = "Autocorrelazione HoltWinters Ottimizzato (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")

# Istogramma Residui
hist(residuals(case.hwOtt),25,freq=F,main = "Istogramma dei Residui HoltWinters Ottimizzato", ylab = "Densità", xlab="Residui")
lines(density(residuals(case.hwOtt)),col="blue")
lines(sort(na.omit(residuals(case.hwOtt))),dnorm(sort(na.omit(residuals(case.hwOtt))),mean(na.omit(residuals(case.hwOtt))),sd(na.omit(residuals(case.hwOtt)))),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Q-Q Plot
qqnorm(residuals(case.hwOtt),main = "Normal Q-Q Plot HoltWinters Ottimizzato")
qqline(residuals(case.hwOtt))

# Shapiro Test
shapiro.test(residuals(case.hwOtt))

# Varianza Residui/Varianza Osservazioni
1 - var(residuals(case.hwOtt))/var(window(case,1964,c(2019,10)))

# Autoregressione Yule-Walker ----
# Residui
smoothScatter(na.omit(resid(case.arYW)),main = "Residui Autoregressione Yule-Walker", ylab = "Residui", xlab="Anno")
acf(na.omit(resid(case.arYW)), 50, main = "Autocorrelazione Residui Autoregressione Yule-Walker", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(na.omit(resid(case.arYW))), 50, main = "Autocorrelazione Autoregressione Yule-Walker (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
pacf(na.omit(resid(case.arYW)), 50, main = "Autocorrelazione Parziale Residui Autoregressione Yule-Walker", ylab = "PACF", xlab="Ritardo (Anni)")

# Istogramma Residui
hist(residuals(case.arYW),25,freq=F,main = "Istogramma dei Residui Autoregressione Yule-Walker", ylab = "Densità", xlab="Residui")
lines(density(na.omit(residuals(case.arYW))),col="blue")
lines(sort(na.omit(residuals(case.arYW))),dnorm(sort(na.omit(residuals(case.arYW))),mean(na.omit(residuals(case.arYW))),sd(na.omit(residuals(case.arYW)))),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Q-Q Plot
qqnorm(residuals(case.arYW),main = "Normal Q-Q Plot Residui Autoregressione Yule-Walker")
qqline(residuals(case.arYW))

# Shapiro Test
shapiro.test(residuals(case.arYW))

# Varianza Residui/Varianza Osservazioni
1 - var(na.omit(residuals(case.arYW)))/var(window(case,c(1965,3),c(2019,10)))

# Autoregressione Manuale ----
# Residui
smoothScatter(case.lm.r,main = "Residui Autoregressione Manuale", ylab = "Residui", xlab="Anno")
acf(case.lm.r, 50, main = "Autocorrelazione Residui Autoregressione Manuale", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
acf(diff(case.lm.r), 50, main = "Autocorrelazione Autoregressione Manuale (Differenza)", ylab = "Funzione di Autocorrelazione", xlab="Ritardo (Anni)")
pacf(case.lm.r, 50, main = "Autocorrelazione Parziale Residui Autoregressione Manuale", ylab = "PACF", xlab="Ritardo (Anni)")

# Istogramma Residui
hist(case.lm.r,25,freq=F,main = "Istogramma dei Residui Autoregressione Manuale", ylab = "Densità", xlab="Residui")
lines(density(case.lm.r),col="blue")
lines(sort(case.lm.r),dnorm(sort(case.lm.r),mean(case.lm.r),sd(case.lm.r)),col="red")
legend(
  "topright",
  legend = c("Distr. Gaussiana", "Distr. Residui"),
  col = c(alpha(rgb(1,0,0), 0.5), alpha(rgb(0,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)

# Q-Q Plot
qqnorm(case.lm.r,main = "Normal Q-Q Plot Autoregressione Manuale")
qqline(case.lm.r)

# Shapiro Test
shapiro.test(case.lm.r)

# Varianza Residui/Varianza Osservazioni
1 - var(case.lm.r)/var(window(case,c(1966,11),c(2019,10)))


# Accuratezza Modelli ----
train = window(case, end = c(2017, 10))
test = window(case, c(2017, 11))

# HoltWinters Automatico 
case.hwAuto = HoltWinters(train, alpha = 0.6646637, beta = 0, gamma = 0.4901472, seasonal = "mult")
# HoltWinters Ottimizzato
case.hwOtt = HoltWinters(train, alpha = 0.4, beta = 0.05, gamma = 0.1, seasonal = "mult",l.start=47.3, b.start=-0.00232)
# Autoregressione con Yule-Walker
case.arYW = ar(train, method = "yule-walker")
# Autoregressione con Minimi Quadrati
case.arMQ = ar(train, method = "ols")
# Autoregressione Manuale
lag = 46
m_case = matrix(nrow = length(train)-lag, ncol = lag)
for (i in 1:lag) {
  m_case[,i] = train[(i):(length(train)-lag-1+ i)]
}
m_case <- data.frame(m_case)
case.lm <- lm(X46 ~ .-X32-X29-X43-X11-X21-X14-X4-X37-X13-X7-X2-X5-X15-X16-X35-X19-X30-X18-X23-X24-X1-X3-X42-X44-X8-X6-X40-X39-X38-X36, data = m_case)
summary(case.lm)

#Predizioni
case.hwAuto.p = predict(case.hwAuto,24)
case.hwOtt.p = predict(case.hwOtt,24)
case.arYW.p = (predict(case.arYW, n.ahead = 24, se.fit = FALSE))
anni = 2
l = length(train)
p = 1:(l + 12 * anni)
p[1:l] = train
for (i in 1:(12 * anni)) {
  lags = c(p[l + i - 9],p[l + i - 10],p[l + i - 12],p[l + i - 17],p[l + i - 20],p[l + i - 22],p[l + i - 25],p[l + i - 26],p[l + i - 27],p[l + i - 28],p[l + i - 31],p[l + i - 33],p[l + i - 34],p[l + i - 41],p[l + i - 45])
  p[l + i] = coef(case.lm) %*% c(1,lags)
} 
case.lm.p = ts(p, frequency = 12, start = 1963)
case.lm.p = window(case.lm.p,c(2017, 11))

plot( case.lm.p, col= alpha(rgb(1,0,1), 0.5),main = "Confronto Accuratezza Modelli", ylab = "Case Monofamiliari Vendute", xlab="Anno", ylim=c(30,70))
lines(case.hwAuto.p, col= alpha(rgb(0,0,1), 0.5))
lines(case.hwOtt.p, col = alpha(rgb(0,1,0), 0.5))
lines(case.arYW.p, col = alpha(rgb(1,0,0), 0.5))
lines(test, col = alpha(rgb(0,0,0), 0.5))
legend(
  "top",
  legend = c("test","hwAuto", "hwOtt","arYW","lm"),
  col = c(alpha(rgb(0,0,0), 0.5),alpha(rgb(0,0,1), 0.5),alpha(rgb(0,1,0), 0.5),alpha(rgb(1,0,0), 0.5),alpha(rgb(1,0,1), 0.5)),
  lwd = 2,
  cex = 0.6
)
#polygon(c(time(test),rev(time(test))), c(case.lm.p + quantile(resid(case.lm), 0.975),rev(case.lm.p + quantile(resid(case.lm), 0.025))), col = alpha(rgb(1,0,1), 0.05),border = alpha(rgb(0,0,0), 0))   
polygon(c(time(test),rev(time(test))), c(case.hwAuto.p + quantile(resid(case.hwAuto), 0.975),rev(case.hwAuto.p + quantile(resid(case.hwAuto), 0.025))), col = alpha(rgb(0,0,1), 0.05),border = alpha(rgb(0,0,0), 0))   
polygon(c(time(test),rev(time(test))), c(case.hwOtt.p + quantile(resid(case.hwOtt), 0.975),rev(case.hwOtt.p + quantile(resid(case.hwOtt), 0.025))), col = alpha(rgb(0,1,0), 0.05),border = alpha(rgb(0,0,0), 0))   
polygon(c(time(test),rev(time(test))), c(case.arYW.p + quantile(na.omit(resid(case.arYW)), 0.975),rev(case.arYW.p + quantile(na.omit(resid(case.arYW)), 0.025))), col = alpha(rgb(1,0,0), 0.05),border = alpha(rgb(0,0,0), 0))   

# Errore Medio
mean(abs(test - case.hwAuto.p))
mean(abs(test - case.hwOtt.p))
mean(abs(test - case.arYW.p))
mean(abs(test - case.lm.p))

# Previsione Finale ----
case.arYW = ar(case, method = "yule-walker")
case.arYW.p = round(predict(case.arYW, n.ahead = 24, se.fit = FALSE))

plot(case.arYW.p, col = "Red", ylim=c(45,80), main = "Previsione", ylab = "Case Monofamiliari Vendute", xlab="Anno")
polygon(c(time(case.arYW.p),rev(time(case.arYW.p))), c(case.arYW.p + quantile(na.omit(resid(case.arYW)), 0.975),rev(case.arYW.p + quantile(na.omit(resid(case.arYW)), 0.025))), col = alpha(rgb(0,1,0), 0.05),border = alpha(rgb(0,0,0), 0))   
lines(case.arYW.p+quantile(na.omit(residuals(case.arYW)),0.025),col=alpha(rgb(0,1,0), 0.5))
lines(case.arYW.p+quantile(na.omit(residuals(case.arYW)),0.975),col=alpha(rgb(0,1,0), 0.5))
abline(lm(case.arYW.p~time(case.arYW.p)), col=alpha(rgb(0,0,1), 0.5))
grid (NULL,NULL, lty = 6, col=alpha(rgb(0,0,0), 0.2))