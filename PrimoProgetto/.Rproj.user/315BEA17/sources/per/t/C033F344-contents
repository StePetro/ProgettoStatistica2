#preparazione dataset -------------------------------------------------------------

#filmEsteso
library(readr)
filmEstesoLettura <-
  read_delim("Dataset/filmEsteso.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)

filmEsteso = filmEstesoLettura[, 2:11]
row.names(filmEsteso) <- filmEstesoLettura$Film
save(filmEsteso, file = "filmEsteso.Rdata")
rm(filmEstesoLettura)

#film
library(readr)
filmLettura <-
  read_delim("Dataset/film.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)

film = filmLettura[, 3:19]
row.names(film) <- filmLettura$Film
film.classe = as.vector(unlist(filmLettura$ClasseBotteghino))
film.std = scale(film)
row.names(film.std) <- filmLettura$Film
save(film, file = "film.Rdata")
save(film.classe, file = "film.classe.Rdata")
save(film.std, file = "film.std.Rdata")
rm(filmLettura)

#analisi preliminari---------------------------------------------------------------

#scatterplot
library(GGally)
ggpairs(
  filmEsteso,
  upper = list(continuous = wrap("points")),
  lower = list(continuous = wrap("smooth_loess"))
)
ggpairs(
  film,
  upper = list(continuous = wrap("points")),
  lower = list(continuous = wrap("points")),
  mapping = ggplot2::aes(colour = film.classe)
)

#corrplot
library(corrplot)
corrplot.mixed(cor(filmEsteso),
               lower = "number",
               upper = "ellipse",
               tl.pos = "lt")
corrplot.mixed(cor(film),
               lower = "number",
               upper = "ellipse",
               tl.pos = "lt")

#esempio di grafico scatterplot + andamento tra Anno e IncassoMondiale
library(ggplot2)
ggplot(film) +
  geom_point(aes(Anno, IncassoMondiale, colour = film.classe)) +
  geom_smooth(aes(Anno, IncassoMondiale, col = "Vis. Andamento"))

#Analisi componenti principali-----------------------------------------------------

#analisi
film.pca = princomp(film.std[,-17])
summary(film.pca)
loadings(film.pca)
varimax(loadings(film.pca)[, 1:8])

#plot varianza comulativa
plot(film.pca, main = "")

plot(
  cumsum(film.pca$sdev ^ 2) / sum(film.pca$sdev ^ 2),
  type = "b",
  ylim = c(0, 1),
  xlab = "Componente Principale",
  ylab = "Proporzione Varianza Accumulata"
)
segments(1, 0.8, 16, 0.8, col = "red")

#biplot
biplot(film.pca)

library(ggbiplot)
ggbiplot(film.pca,
         obs.scale = 1,
         var.scale = 1,
         groups = film.classe) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

ggbiplot(
  film.pca,
  obs.scale = 1,
  var.scale = 1,
  groups = film.classe,
  choices = c(1, 3)
) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

ggbiplot(
  film.pca,
  obs.scale = 1,
  var.scale = 1,
  groups = film.classe,
  choices = c(2, 3)
) +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#Regressione-----------------------------------------------------------------------

#Analisi senza modifiche al modello
film.lm = lm(IncassoMondiale ~ ., data = film)
summary(film.lm)

#Modello Ottimizzato

film.log = film
film.log$LikesProtagonista = log(film.log$LikesProtagonista + 1)
film.log$BestWikiRankGlobale = log(film.log$BestWikiRankGlobale)
film.log$BestWikiRankLocale = log(film.log$BestWikiRankLocale)
film.log$LunghezzaTrama = log(film.log$LunghezzaTrama + 1)
film.log$LikesProtagonista = log(film.log$LikesRegista + 1)
film.log$LikesProtagonista = log(film.log$Morti)

film.log.lm = lm(IncassoMondiale ~ ., data = film.log)
summary(film.log.lm)

#Modello Ridotto
film.log.lm.ridotto = lm(
  IncassoMondiale ~ . - Rating - SRR - NumeroAttori - Durata - Anno - LunghezzaTrama -
    LikesRegista - BestWikiRankGlobale - BestWikiRankLocale - WikiPageQuality -
    LikesProtagonista - Votanti - Morti,
  data = film.log
)
summary(film.log.lm.ridotto)

#confronto tra i modelli
n = 1000
erroreMedio1 = rep(0, n)
erroreMedio2 = rep(0, n)
erroreMedio3 = rep(0, n)

for (i in 1:n) {
  campioni = sample(326, 10)
  
  trainingSet1 = film[-campioni,]
  trainingSet2 = film.log[-campioni,]
  trainingSet3 = film.log[-campioni,]
  testSet1 = film[campioni,]
  testSet2 = film.log[campioni,]
  testSet3 = film.log[campioni,]
  
  film.lm1 = lm(IncassoMondiale ~ ., data =
                  trainingSet1)
  film.lm2 = lm(IncassoMondiale ~ ., data = trainingSet2)
  film.lm3 = lm(
    IncassoMondiale ~ . - Rating - SRR - NumeroAttori - Durata - Anno - LunghezzaTrama -
      LikesRegista - BestWikiRankGlobale - BestWikiRankLocale - WikiPageQuality -
      LikesProtagonista - Votanti - Morti,
    data = trainingSet3
  )
  
  predizione1 = predict(film.lm1, newdata = testSet1)
  predizione2 = predict(film.lm2, newdata = testSet2)
  predizione3 = predict(film.lm3, newdata = testSet3)
  
  erroreMedio1[i] = mean(abs(predizione1 - testSet1$IncassoMondiale))
  erroreMedio2[i] = mean(abs(predizione2 - testSet2$IncassoMondiale))
  erroreMedio3[i] = mean(abs(predizione3 - testSet3$IncassoMondiale))
}

plot(
  erroreMedio3,
  type = "l",
  col = "green",
  xlab = "Iterazione",
  ylab = "Errore Medio"
)
lines(erroreMedio1, type = "l", col = "red")
lines(erroreMedio2, type = "l", col = "blue")

segments(0, mean(erroreMedio1), n, mean(erroreMedio1), col = "darkRed")
segments(0, mean(erroreMedio2), n, mean(erroreMedio2), col = "darkBlue")
segments(0, mean(erroreMedio3), n, mean(erroreMedio3), col = "darkGreen")

mean(erroreMedio1)
mean(erroreMedio2)
mean(erroreMedio3)

#n = 1000
#> mean(erroreMedio1)
#[1] 37558020
#> mean(erroreMedio2)
#[1] 37131018
#> mean(erroreMedio3)
#[1] 69377192

#n = 10000
#> mean(erroreMedio1)
#[1] 37645204
#> mean(erroreMedio2)
#[1] 37179913
#> mean(erroreMedio3)
#[1] 68781142

#plot residui
library(ggfortify)
autoplot(film.log.lm)

#Shapiro test
shapiro.test(resid(film.lm))

#istogramma residui
hist(
  resid(film.log.lm),
  40,
  freq = F,
  main = "Istogramma dei Residui",
  ylab = "Densità",
  xlab = "Residui"
)
lines(density(resid(film.log.lm)), col = "red")
lines(sort(resid(film.log.lm)), dnorm(sort(resid(film.log.lm)), mean(resid(film.log.lm)), sd(resid(film.log.lm))), col =
        "blue")

#Margini di Previsione
campioni = sample(316, 10)
trainingSet = film.log[-campioni,]
testSet = film.log[campioni,]

film.log.lm = lm(IncassoMondiale ~ ., data =
                   trainingSet)
predizione = predict(film.log.lm, newdata = testSet)
predizione
t(testSet$IncassoMondiale)

film.log.lm.ci = predict(film.log.lm, testSet, interval = "confidence")
film.log.lm.pi = predict(film.log.lm, testSet, interval = "prediction")

plot(
  as.vector(unlist(testSet$IncassoMondiale)),
  pch = 19,
  col = "red",
  xlim = c(1, 10),
  ylim = c(min(film.log.lm.pi[, 2]), max(film.log.lm.pi[, 3])),
  ylab = "Incasso Mondiale",
  xlab = "Indice"
)

x = 1:10

points(x - 0.05, film.log.lm.ci[, 1], pch = 20, col = "blue")
segments(x - 0.05, film.log.lm.ci[, 2], x - 0.05, film.log.lm.ci[, 3], col =
           "blue")

points(x + 0.05, film.log.lm.pi[, 1], pch = 19, col = "green3")
segments(x + 0.05, film.log.lm.pi[, 2], x + 0.05, film.log.lm.pi[, 3], col =
           "green3")