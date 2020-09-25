#################### CODICE SECONDO PROGETTO STATISTICA ###################################

#Pulisce l'Ambiente
rm(list = ls())

#Caricamento Dataset e Analisi Preliminali -------------------------------------------------------------

# confusion matrix su posterior probabilities
mconfmat <- function(response, predictor, p = 0.5) {
  cm = matrix(nrow = 2, ncol = 2)
  
  # binaria
  cm[1, 1] = sum((predictor >= p) & (response == 1)) # true positive
  cm[1, 2] = sum((predictor >= p) &(response == 0)) # false positive
  cm[2, 1] = sum((predictor <= p) &(response == 1)) # false negative
  cm[2, 2] = sum((predictor <= p) & (response == 0)) # true negative
  
  cm <- data.frame(cm)
  rownames(cm) <- c("predicted 1", "predicted 0")
  colnames(cm) <- c("actual 1", "actual 0")
  cm
}

#Nominations
library(readr)
nominationsLettura <-
  read_delim(
    "tabella.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

nom = nominationsLettura[, 3:19]
row.names(nom) <- nominationsLettura$Film
nom.classe = as.vector(unlist(nominationsLettura$Nomination))
rm(nominationsLettura)

#scatterplot
library(GGally)
ggpairs(
  nom,
  upper = list(continuous = wrap("points")),
  lower = list(continuous = wrap("points")),
  mapping = ggplot2::aes(colour = nom.classe)
)

library(ggbiplot)
ggbiplot(
  princomp(nom)  ,
  obs.scale = 1,
  var.scale = 1,
  varname.size = 0,
  var.axes = F,
  groups = nom.classe,
  ellipse = TRUE,
  circle = TRUE
) +
  theme(
    legend.direction = 'horizontal',
    legend.position = 'top',
    legend.title = element_blank()
  )

# Classi all'interno del dataset
library(readr)
nominationsLettura <-
  read_delim("tabella.csv",
             ";",
             escape_double = FALSE,
             trim_ws = TRUE)

nom = nominationsLettura[, 2:19]
row.names(nom) <- nominationsLettura$Film
rm(nominationsLettura)
#nom$Nomination <-2*nom$Nomination -1

#corrplot
library(corrplot)
corrplot.mixed(cor(nom),
               lower = "number",
               upper = "ellipse",
               tl.pos = "lt")

# Confronto Metodi di Classificazione -----------------------------------------------------------------

# Modello di Analisi del Discriminante Lineare
library(MASS)
nom.lda = lda(Nomination ~ ., data = nom, CV = F)
summary(nom.lda)
nom.lda.post = predict(nom.lda)$posterior[, 2]
sum((nom.lda.post > 0.5) == (nom$Nomination > 0.5)) / length(nom$Nomination)
mconfmat(nom$Nomination, nom.lda.post)

# Modello di Analisi del Discriminante Quadratico
nom.qda = qda(Nomination ~ ., data = nom, CV = F)
summary(nom.qda)
nom.qda.post = predict(nom.qda)$posterior[, 2]
sum((nom.qda.post > 0.5) == (nom$Nomination > 0.5)) / length(nom$Nomination)
mconfmat(nom$Nomination, nom.qda.post)

# Modello di Regressione Lineare Generalizzato
nom.glm = glm(Nomination ~ ., family = binomial, data = nom)
summary(nom.glm)
nom.glm.p = predict(nom.glm, type = "response")
sum((nom.glm.p > 0.5) == (nom$Nomination > 0.5)) / length(nom$Nomination)
mconfmat(nom$Nomination, nom.glm.p)

# Modello di Regressione Lineare Gaussiano
#nom.lm = lm(Nomination~.,data=nom)
#summary(nom.lm)
#sum((predict(nom.lm)>0)==(nom$Nomination>0))/length(nom$Nomination)
#nom.lm.p = predict(nom.lm)
#mconfmat(nom$Nomination>0,nom.lm.p>0)

library(pROC)
qdaRoc <- roc(nom$Nomination, nom.qda.post)
glmRoc <- roc(nom$Nomination, nom.glm.p)
ldaRoc <- roc(nom$Nomination, nom.lda.post)
#lmRoc <- roc(nom$Nomination,nom.lm.p)

plot(
  glmRoc,
  col = "red",
  main = "Confronto Curve ROC",
  ylab = "Sensibilità  (tasso positivi veri)",
  xlab = "Specificità  (tasso negativi veri)"
)
lines(ldaRoc, col = "green3")
lines(qdaRoc, col = "blue")
#(lmRoc, col = "violet")
legend(
  "bottomright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

qdaRoc$auc
glmRoc$auc
ldaRoc$auc

#10-folds cross validation----------------------------------------------------------
set.seed(1255797182)

#Creo 10 partizioni e le mescolo
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]
#Eseguo la validatione

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

cmGlm = matrix(nrow = 2, ncol = 2)
cmLda = matrix(nrow = 2, ncol = 2)
cmQda = matrix(nrow = 2, ncol = 2)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom[testIndexes, ]
  trainData <- nom[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ ., data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ ., data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ ., family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
  
  if (i > 1) {
    cmGlm[1, 1] = cmGlm[1, 1] + sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = cmGlm[1, 2] + sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = cmGlm[2, 1] + sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = cmGlm[2, 2] + sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = cmLda[1, 1] + sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = cmLda[1, 2] + sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = cmLda[2, 1] + sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = cmLda[2, 2] + sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = cmQda[1, 1] + sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = cmQda[1, 2] + sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = cmQda[2, 1] + sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = cmQda[2, 2] + sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = c(predGlm, nom.glm.p)
    predLda = c(predLda, nom.lda.post)
    predQda = c(predQda, nom.qda.post)
    actualValue = c(actualValue, testData$Nomination)
    
  } else{
    cmGlm[1, 1] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = nom.glm.p
    predLda = nom.lda.post
    predQda = nom.qda.post
    actualValue = testData$Nomination
  }
}

# Curve ROC
library(pROC)
qdaRoc <- roc(actualValue, predQda)
glmRoc <- roc(actualValue, predGlm)
ldaRoc <- roc(actualValue, predLda)

plot(
  glmRoc,
  col = "red",
  main = "Confronto Curve ROC Cross Validazione",
  ylab = "Sensibilità  (tasso positivi veri)",
  xlab = "Specificità  (tasso negativi veri)"
)
lines(ldaRoc, col = "green3")
lines(qdaRoc, col = "blue")
legend(
  "bottomright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

qdaRoc$auc
glmRoc$auc
ldaRoc$auc


#Grafico Cross Validazione
plot(
  main = "Cross Validazione",
  accLda,
  type = "l",
  col = "green",
  xlab = "Partizione",
  ylab = "Accuratezza",
  ylim = c(0.50,1)
)
lines(accGlm, type = "l", col = "red")
lines(accQda, type = "l", col = "blue")
legend(
  "topright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

segments(0,  mean(accLda), n,  mean(accLda), col = "darkGreen")
segments(0, mean(accGlm), n, mean(accGlm), col = "darkRed")
segments(0, mean(accQda), n, mean(accQda), col = "darkBlue")

#Accuratezza Media
mean(accLda)
mean(accGlm)
mean(accQda)

#Matrice di Confusione Totale
cmLda <- data.frame(cmLda)
rownames(cmLda) <- c("predicted 1", "predicted 0")
colnames(cmLda) <- c("actual 1", "actual 0")
cmGlm <- data.frame(cmGlm)
rownames(cmGlm) <- c("predicted 1", "predicted 0")
colnames(cmGlm) <- c("actual 1", "actual 0")
cmQda <- data.frame(cmQda)
rownames(cmQda) <- c("predicted 1", "predicted 0")
colnames(cmQda) <- c("actual 1", "actual 0")

cmLda
cmGlm
cmQda

#Test Robustezza Modelli --------------------------------------------------
set.seed(-1348923468)
n = nrow(nom)
idx = sample(n, n)
accGlmMedia = rep(0, n)
accLdaMedia = rep(0, n)
accQdaMedia = rep(0, n)
nomScambiata = nom

for (j in 1:n) {
  if (nomScambiata$Nomination[idx[j]] == 1) {
    nomScambiata$Nomination[idx[j]] = 0
  } else{
    nomScambiata$Nomination[idx[j]] = 1
  }
  
  set.seed(1255797182)
  
  #Creo 10 partizioni e le mescolo
  folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
  folds <- folds[sample(nrow(nom))]
  #Eseguo la validatione
  
  accGlm = rep(0, 10)
  accLda = rep(0, 10)
  accQda = rep(0, 10)
  
  for (i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- nom[testIndexes, ]
    trainData <- nomScambiata[-testIndexes, ]
    
    nom.lda = lda(Nomination ~ ., data = trainData, CV = F)
    nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
    accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
      length(testData$Nomination)
    
    nom.qda = qda(Nomination ~ ., data = trainData, CV = F)
    nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
    accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
      length(testData$Nomination)
    
    nom.glm = glm(Nomination ~ ., family = binomial, data = trainData)
    nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
    accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
  }
  accGlmMedia[j] = mean(accGlm)
  accLdaMedia[j] = mean(accLda)
  accQdaMedia[j] = mean(accQda)
}

plot(
  accGlmMedia,
  type = "l",
  col = "red",
  main = "Confronto Robustezza Modelli Logaritmici",
  ylab = "Accuratezza Media Convalida Incrociata",
  xlab = "Indici Scambiati"
)
lines(accLdaMedia, col = "green")
lines(accQdaMedia, col = "blue")
legend(
  "topright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)
segments(-10, 0.7, 250, 0.7, col = "grey")
segments(-10, 0.3, 400, 0.3, col ="grey")

# Ottimizzazione e Semplificazione Modelli ------------------------------------------------
#Modello Logaritmico ----
nom.log = nom

# nom.log$Anno = log(nom.log$Anno)
# cor(nom$Nomination, nom.log$Anno)
# cor(nom$Nomination, nom$Anno)
# nom.log$WikiPageQuality = log(nom.log$WikiPageQuality)
# cor(nom$Nomination, nom.log$WikiPageQuality)
# cor(nom$Nomination, nom$WikiPageQuality)
# nom.log$BestWikiRankLocale = log(nom.log$BestWikiRankLocale)
# cor(nom$Nomination, nom.log$BestWikiRankLocale)
# cor(nom$Nomination, nom$BestWikiRankLocale)
# nom.log$BestWikiRankGlobale = log(nom.log$BestWikiRankGlobale)
# cor(nom$Nomination, nom.log$BestWikiRankGlobale)
# cor(nom$Nomination, nom$BestWikiRankGlobale)
# nom.log$LikesProtagonista = log(nom.log$LikesProtagonista + 1)
# cor(nom$Nomination, nom.log$LikesProtagonista)
# cor(nom$Nomination, nom$LikesProtagonista)
# nom.log$Durata = log(nom.log$Durata)
# cor(nom$Nomination, nom.log$Durata)
# cor(nom$Nomination, nom$Durata)
# nom.log$Morti = log(nom.log$Morti)
# cor(nom$Nomination, nom.log$Morti)
# cor(nom$Nomination, nom$Morti)
# nom.log$Rating = log(nom.log$Rating)
# cor(nom$Nomination, nom.log$Rating)
# cor(nom$Nomination, nom$Rating)
# nom.log$Votanti = log(nom.log$Votanti)
# cor(nom$Nomination, nom.log$Votanti)
# cor(nom$Nomination, nom$Votanti)
nom.log$LikesFilm = log(nom.log$LikesFilm + 1) ##
cor(nom$Nomination, nom.log$LikesFilm)
cor(nom$Nomination, nom$LikesFilm)
nom.log$LikesRegista = log(nom.log$LikesRegista + 1) ###
cor(nom$Nomination, nom.log$LikesRegista)
cor(nom$Nomination, nom$LikesRegista)
# nom.log$Budget = log(nom.log$Budget)
# cor(nom$Nomination, nom.log$Budget)
# cor(nom$Nomination, nom$Budget)
# nom.log$IncassoDomestico = log(nom.log$IncassoDomestico + 1)
# cor(nom$Nomination, nom.log$IncassoDomestico)
# cor(nom$Nomination, nom$IncassoDomestico)
nom.log$LunghezzaTrama = log(nom.log$LunghezzaTrama) #
cor(nom$Nomination, nom.log$LunghezzaTrama)
cor(nom$Nomination, nom$LunghezzaTrama)
nom.log$NumeroAttori = log(nom.log$NumeroAttori + 1) #
cor(nom$Nomination, nom.log$NumeroAttori)
cor(nom$Nomination, nom$NumeroAttori)
nom.log$SRR = log(nom.log$SRR) #
cor(nom$Nomination, nom.log$SRR)
cor(nom$Nomination, nom$SRR)
nom.log$IncassoMondiale = log(nom.log$IncassoMondiale) #
cor(nom$Nomination, nom.log$IncassoMondiale)
cor(nom$Nomination, nom$IncassoMondiale)

#10-folds cross validation Modello Logaritmico----------------------------------------------------------

set.seed(1255797182)

#Creo 10 partizioni e le mescolo
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

#Eseguo la validatione

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

cmGlm = matrix(nrow = 2, ncol = 2)
cmLda = matrix(nrow = 2, ncol = 2)
cmQda = matrix(nrow = 2, ncol = 2)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ ., data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ ., data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ ., family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
  
  if (i > 1) {
    cmGlm[1, 1] = cmGlm[1, 1] + sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = cmGlm[1, 2] + sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = cmGlm[2, 1] + sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = cmGlm[2, 2] + sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = cmLda[1, 1] + sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = cmLda[1, 2] + sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = cmLda[2, 1] + sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = cmLda[2, 2] + sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = cmQda[1, 1] + sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = cmQda[1, 2] + sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = cmQda[2, 1] + sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = cmQda[2, 2] + sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = c(predGlm, nom.glm.p)
    predLda = c(predLda, nom.lda.post)
    predQda = c(predQda, nom.qda.post)
    actualValue = c(actualValue, testData$Nomination)
    
  } else{
    cmGlm[1, 1] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = nom.glm.p
    predLda = nom.lda.post
    predQda = nom.qda.post
    actualValue = testData$Nomination
  }
}

#Confronto Curve ROC
library(pROC)
qdaRoc <- roc(actualValue, predQda)
glmRoc <- roc(actualValue, predGlm)
ldaRoc <- roc(actualValue, predLda)

plot(
  glmRoc,
  col = "red",
  main = "Confronto Curve ROC Ottimizzazzione Logaritmica",
  ylab = "Sensibilità  (tasso positivi veri)",
  xlab = "Specificità  (tasso negativi veri)"
)
lines(ldaRoc, col = "green3")
lines(qdaRoc, col = "blue")
legend(
  "bottomright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

qdaRoc$auc
glmRoc$auc
ldaRoc$auc


#Grafico Cross Validazione
plot(
  main = "Cross Validazione Ottimizzazzione Logaritmica",
  accLda,
  type = "l",
  col = "green",
  xlab = "Partizione",
  ylab = "Accuratezza",
  ylim = c(0.5,1)
)
lines(accGlm, type = "l", col = "red")
lines(accQda, type = "l", col = "blue")
legend(
  "bottomleft",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

segments(0,  mean(accLda), n,  mean(accLda), col = "darkGreen")
segments(0, mean(accGlm), n, mean(accGlm), col = "darkRed")
segments(0, mean(accQda), n, mean(accQda), col = "darkBlue")

#Accuratezza Media
mean(accLda)
mean(accGlm)
mean(accQda)

#Matrice di Confusione Totale
cmLda <- data.frame(cmLda)
rownames(cmLda) <- c("predicted 1", "predicted 0")
colnames(cmLda) <- c("actual 1", "actual 0")
cmGlm <- data.frame(cmGlm)
rownames(cmGlm) <- c("predicted 1", "predicted 0")
colnames(cmGlm) <- c("actual 1", "actual 0")
cmQda <- data.frame(cmQda)
rownames(cmQda) <- c("predicted 1", "predicted 0")
colnames(cmQda) <- c("actual 1", "actual 0")

cmLda
cmGlm
cmQda

#Test Robustezza Modelli Logaritmici--------------------------------------------------
set.seed(-1348923468)
n = nrow(nom)
idx = sample(n, n)
accGlmMedia = rep(0, n)
accLdaMedia = rep(0, n)
accQdaMedia = rep(0, n)
nomScambiata = nom.log

for (j in 1:n) {
  if (nomScambiata$Nomination[idx[j]] == 1) {
    nomScambiata$Nomination[idx[j]] = 0
  } else{
      nomScambiata$Nomination[idx[j]] = 1
  }
  
  set.seed(1255797182)

  #Creo 10 partizioni e le mescolo
  folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
  folds <- folds[sample(nrow(nom))]
  #Eseguo la validatione
  
  accGlm = rep(0, 10)
  accLda = rep(0, 10)
  accQda = rep(0, 10)

  for (i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- nom.log[testIndexes, ]
    trainData <- nomScambiata[-testIndexes, ]
    
    nom.lda = lda(Nomination ~ ., data = trainData, CV = F)
    nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
    accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
      length(testData$Nomination)
    
    nom.qda = qda(Nomination ~ ., data = trainData, CV = F)
    nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
    accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
      length(testData$Nomination)
    
    nom.glm = glm(Nomination ~ ., family = binomial, data = trainData)
    nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
    accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
  }
  accGlmMedia[j] = mean(accGlm)
  accLdaMedia[j] = mean(accLda)
  accQdaMedia[j] = mean(accQda)
}

plot(
  accGlmMedia,
  type = "l",
  col = "red",
  main = "Confronto Robustezza Modelli Logaritmici",
  ylab = "Accuratezza Media Convalida Incrociata",
  xlab = "Indici Scambiati"
)
lines(accLdaMedia, col = "green")
lines(accQdaMedia, col = "blue")
legend(
  "topright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)
segments(-10, 0.7, 250, 0.7, col = "grey")
segments(-10, 0.3, 400, 0.3, col ="grey")

#Modello Ridotto --------------------------------------------------------------------------------------

accGlmMedia = rep(0, 17)
accQdaMedia = rep(0, 17)
accLdaMedia = rep(0, 17)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ . , data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ . , data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ . , family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[1] = mean(accGlm)
accQdaMedia[1] = mean(accQda)
accLdaMedia[1] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm , data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm , data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm , family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[2] = mean(accGlm)
accQdaMedia[2] = mean(accQda)
accLdaMedia[2] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[3] = mean(accGlm)
accQdaMedia[3] = mean(accQda)
accLdaMedia[3] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[4] = mean(accGlm)
accQdaMedia[4] = mean(accQda)
accLdaMedia[4] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[5] = mean(accGlm)
accQdaMedia[5] = mean(accQda)
accLdaMedia[5] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[6] = mean(accGlm)
accQdaMedia[6] = mean(accQda)
accLdaMedia[6] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[7] = mean(accGlm)
accQdaMedia[7] = mean(accQda)
accLdaMedia[7] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[8] = mean(accGlm)
accQdaMedia[8] = mean(accQda)
accLdaMedia[8] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality , data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality , data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality , family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[9] = mean(accGlm)
accQdaMedia[9] = mean(accQda)
accLdaMedia[9] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[10] = mean(accGlm)
accQdaMedia[10] = mean(accQda)
accLdaMedia[10] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[11] = mean(accGlm)
accQdaMedia[11] = mean(accQda)
accLdaMedia[11] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[12] = mean(accGlm)
accQdaMedia[12] = mean(accQda)
accLdaMedia[12] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[13] = mean(accGlm)
accQdaMedia[13] = mean(accQda)
accLdaMedia[13] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico , data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico , data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[14] = mean(accGlm)
accQdaMedia[14] = mean(accQda)
accLdaMedia[14] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno , data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno , data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno , family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[15] = mean(accGlm)
accQdaMedia[15] = mean(accQda)
accLdaMedia[15] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno - LikesRegista, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno - LikesRegista, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno - LikesRegista, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[16] = mean(accGlm)
accQdaMedia[16] = mean(accQda)
accLdaMedia[16] = mean(accLda)

#Creo 10 partizioni e le mescolo
set.seed(1255797182)
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno - LikesRegista - Rating, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno - LikesRegista - Rating, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
    LikesProtagonista - IncassoDomestico - Anno - LikesRegista - Rating, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
}

accGlmMedia[17] = mean(accGlm)
accQdaMedia[17] = mean(accQda)
accLdaMedia[17] = mean(accLda)


#Grafico Accuratezza
plot(
  main = "Accuratezza Dopo Riduzione",
  accLdaMedia,
  type = "l",
  col = "green",
  xlab = "Fattori Eliminati - 1",
  ylab = "Accuratezza Cross Validation Media"
)
lines(accGlmMedia, type = "l", col = "red")
lines(accQdaMedia, type = "l", col = "blue")
legend(
  "bottom",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

# Glm 12 0.8008523
# Qda 16 0.7700758
# Lda 9 0.7973485

#10-folds cross validation Modelli Ridotti e Ottimizzati----------------------------------------------------------

set.seed(1255797182)

#Creo 10 partizioni e le mescolo
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

#Eseguo la validatione

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

cmGlm = matrix(nrow = 2, ncol = 2)
cmLda = matrix(nrow = 2, ncol = 2)
cmQda = matrix(nrow = 2, ncol = 2)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
                  LikesProtagonista - IncassoDomestico - Anno - LikesRegista, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
    length(testData$Nomination)
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
  
  if (i > 1) {
    cmGlm[1, 1] = cmGlm[1, 1] + sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = cmGlm[1, 2] + sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = cmGlm[2, 1] + sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = cmGlm[2, 2] + sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = cmLda[1, 1] + sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = cmLda[1, 2] + sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = cmLda[2, 1] + sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = cmLda[2, 2] + sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = cmQda[1, 1] + sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = cmQda[1, 2] + sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = cmQda[2, 1] + sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = cmQda[2, 2] + sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = c(predGlm, nom.glm.p)
    predLda = c(predLda, nom.lda.post)
    predQda = c(predQda, nom.qda.post)
    actualValue = c(actualValue, testData$Nomination)
    
  } else{
    cmGlm[1, 1] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = nom.glm.p
    predLda = nom.lda.post
    predQda = nom.qda.post
    actualValue = testData$Nomination
  }
}

#Confronto Curve ROC
library(pROC)
qdaRoc <- roc(actualValue, predQda)
glmRoc <- roc(actualValue, predGlm)
ldaRoc <- roc(actualValue, predLda)

plot(
  glmRoc,
  col = "red",
  main = "Confronto Curve ROC Modelli Ridotti",
  ylab = "Sensibilità  (tasso positivi veri)",
  xlab = "Specificità  (tasso negativi veri)"
)
lines(ldaRoc, col = "green3")
lines(qdaRoc, col = "blue")
legend(
  "bottomright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

qdaRoc$auc
glmRoc$auc
ldaRoc$auc


#Grafico Cross Validazione
plot(
  main = "Cross Validazione Modelli Ridotti",
  accLda,
  type = "l",
  col = "green",
  xlab = "Partizione",
  ylab = "Accuratezza",
  ylim=c(0.5,1)
)
lines(accGlm, type = "l", col = "red")
lines(accQda, type = "l", col = "blue")
legend(
  "bottom",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)

segments(0,  mean(accLda), n,  mean(accLda), col = "darkGreen")
segments(0, mean(accGlm), n, mean(accGlm), col = "darkRed")
segments(0, mean(accQda), n, mean(accQda), col = "darkBlue")

#Accuratezza Media
mean(accLda)
mean(accGlm)
mean(accQda)

#Matrice di Confusione Totale
cmLda <- data.frame(cmLda)
rownames(cmLda) <- c("predicted 1", "predicted 0")
colnames(cmLda) <- c("actual 1", "actual 0")
cmGlm <- data.frame(cmGlm)
rownames(cmGlm) <- c("predicted 1", "predicted 0")
colnames(cmGlm) <- c("actual 1", "actual 0")
cmQda <- data.frame(cmQda)
rownames(cmQda) <- c("predicted 1", "predicted 0")
colnames(cmQda) <- c("actual 1", "actual 0")

cmLda
cmGlm
cmQda

# Seed secondo test 824292529

#Test Robustezza Modelli Logaritmici--------------------------------------------------
set.seed(-1348923468)
n = nrow(nom)
idx = sample(n, n)
accGlmMedia = rep(0, n)
accLdaMedia = rep(0, n)
accQdaMedia = rep(0, n)
nomScambiata = nom.log

for (j in 1:n) {
  if (nomScambiata$Nomination[idx[j]] == 1) {
    nomScambiata$Nomination[idx[j]] = 0
  } else{
    nomScambiata$Nomination[idx[j]] = 1
  }
  
  set.seed(1255797182)
  
  #Creo 10 partizioni e le mescolo
  folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
  folds <- folds[sample(nrow(nom))]
  #Eseguo la validatione
  
  accGlm = rep(0, 10)
  accLda = rep(0, 10)
  accQda = rep(0, 10)
  
  for (i in 1:10) {
    testIndexes <- which(folds == i, arr.ind = TRUE)
    testData <- nom.log[testIndexes, ]
    trainData <- nomScambiata[-testIndexes, ]
    
    nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, data = trainData, CV = F)
    nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
    accLda[i] = sum((nom.lda.post > 0.5) == (testData$Nomination > 0.5)) /
      length(testData$Nomination)
    
    nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
                    LikesProtagonista - IncassoDomestico - Anno - LikesRegista, data = trainData, CV = F)
    nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
    accQda[i] = sum((nom.qda.post > 0.5) == (testData$Nomination > 0.5)) /
      length(testData$Nomination)
    
    nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                    Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, family = binomial, data = trainData)
    nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
    accGlm[i] = sum((nom.glm.p > 0.5) == (testData$Nomination > 0.5)) / length(testData$Nomination)
  }
  accGlmMedia[j] = mean(accGlm)
  accLdaMedia[j] = mean(accLda)
  accQdaMedia[j] = mean(accQda)
}

plot(
  accGlmMedia,
  type = "l",
  col = "red",
  main = "Confronto Robustezza Modelli Ridotti",
  ylab = "Accuratezza Media Convalida Incrociata",
  xlab = "Indici Scambiati"
)
lines(accLdaMedia, col = "green")
lines(accQdaMedia, col = "blue")
legend(
  "topright",
  legend = c("glm", "lda", "qda"),
  col = c("red", "green", "blue"),
  lwd = 2
)
segments(-10, 0.7, 250, 0.7, col = "grey")
segments(-10, 0.3, 400, 0.3, col ="grey")

#Gestione Sensibilità----------------------------------------------------------

sensGlm = rep(0, 11)
sensLda = rep(0, 11)
sensQda = rep(0, 11)

accMediaGlm = rep(0, 11)
accMediaLda = rep(0, 11)
accMediaQda = rep(0, 11)
for (j in 1:11) {

set.seed(1255797182)

#Creo 10 partizioni e le mescolo
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

#Eseguo la validatione

accGlm = rep(0, 10)
accLda = rep(0, 10)
accQda = rep(0, 10)

cmGlm = matrix(nrow = 2, ncol = 2)
cmLda = matrix(nrow = 2, ncol = 2)
cmQda = matrix(nrow = 2, ncol = 2)

p = 0.5 - ((j-1)*0.05)

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.lda = lda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, data = trainData, CV = F)
  nom.lda.post = predict(nom.lda, newdata = testData)$posterior[, 2]
  accLda[i] = sum((nom.lda.post > p ) == (testData$Nomination >p )) /
    length(testData$Nomination)
  
  nom.qda = qda(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
                  LikesProtagonista - IncassoDomestico - Anno - LikesRegista, data = trainData, CV = F)
  nom.qda.post = predict(nom.qda, newdata = testData)$posterior[, 2]
  accQda[i] = sum((nom.qda.post > p ) == (testData$Nomination > p )) /
    length(testData$Nomination)

  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > p ) == (testData$Nomination > p )) / length(testData$Nomination)

  if (i > 1) {
    cmGlm[1, 1] = cmGlm[1, 1] + sum((nom.glm.p >=p ) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = cmGlm[1, 2] + sum((nom.glm.p >= p ) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = cmGlm[2, 1] + sum((nom.glm.p <= p ) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = cmGlm[2, 2] + sum((nom.glm.p <= p ) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = cmLda[1, 1] + sum((nom.lda.post >= p ) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = cmLda[1, 2] + sum((nom.lda.post >= p ) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = cmLda[2, 1] + sum((nom.lda.post <= p ) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = cmLda[2, 2] + sum((nom.lda.post <= p ) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = cmQda[1, 1] + sum((nom.qda.post >= p ) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = cmQda[1, 2] + sum((nom.qda.post >= p ) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = cmQda[2, 1] + sum((nom.qda.post <= p ) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = cmQda[2, 2] + sum((nom.qda.post <= p ) &(testData$Nomination == 0)) # true negative
    
    predGlm = c(predGlm, nom.glm.p)
    predLda = c(predLda, nom.lda.post)
    predQda = c(predQda, nom.qda.post)
    actualValue = c(actualValue, testData$Nomination)
    
  } else{
    cmGlm[1, 1] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative
    cmLda[1, 1] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmLda[1, 2] = sum((nom.lda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmLda[2, 1] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmLda[2, 2] = sum((nom.lda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    cmQda[1, 1] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 1)) # true positive
    cmQda[1, 2] = sum((nom.qda.post >= 0.5) &(testData$Nomination == 0)) # false positive
    cmQda[2, 1] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 1)) # false negative
    cmQda[2, 2] = sum((nom.qda.post <= 0.5) &(testData$Nomination == 0)) # true negative
    
    predGlm = nom.glm.p
    predLda = nom.lda.post
    predQda = nom.qda.post
    actualValue = testData$Nomination
  }

}
sensGlm[j] = cmGlm[1, 1] / (cmGlm[1, 1] + cmGlm[2, 1])
sensLda[j] = cmLda[1, 1] / (cmLda[1, 1] + cmLda[2, 1])
sensQda[j] = cmQda[1, 1] / (cmQda[1, 1] + cmQda[2, 1])

accMediaGlm[j] = mean(accGlm)
accMediaLda[j] = mean(accLda)
accMediaQda[j] = mean(accQda)
}

graficoGlm = matrix(nrow = 11, ncol = 2)
graficoGlm[,2] = accMediaGlm
graficoGlm[,1] = seq(0.50, 0, -0.05 )
graficoLda = matrix(nrow = 11, ncol = 2)
graficoLda[,2] = accMediaLda
graficoLda[,1] = seq(0.50, 0, -0.05 )
graficoQda = matrix(nrow = 11, ncol = 2)
graficoQda[,2] = accMediaQda
graficoQda[,1] = seq(0.50, 0, -0.05 )

#Grafico Accuratezza e Sensibilità
plot(
  graficoGlm,
  type = "l",
  col = "firebrick1",
  main = "Confronto Accuratezza e Sensibilità",
  ylab = "Accuratezza-Sensibilità Media Convalida Incrociata",
  xlab = "Soglia Probabilità Positiva",
  ylim = c(0.3,1),
  xlim = c(0.50, 0)
)
lines(graficoLda, col = "darkolivegreen1")
lines(graficoQda, col = "cyan")

graficoGlm = matrix(nrow = 11, ncol = 2)
graficoGlm[,2] = sensGlm
graficoGlm[,1] = seq(0.50, 0, -0.05 )
graficoLda = matrix(nrow = 11, ncol = 2)
graficoLda[,2] = sensLda
graficoLda[,1] = seq(0.50, 0, -0.05 )
graficoQda = matrix(nrow = 11, ncol = 2)
graficoQda[,2] = sensQda
graficoQda[,1] = seq(0.50, 0, -0.05 )

lines(graficoGlm, col = "darkred")
lines(graficoLda, col = "darkgreen")
lines(graficoQda, col = "darkblue")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
legend(
  "bottomleft",
  legend = c("Accuratezza Glm", "Accuratezza Lda", "Accuratezza Qda", "Sensibilità Glm", "Sensibilità Lda", "Sensibilità Qda"),
  col = c("firebrick1", "darkolivegreen1", "cyan", "darkred","darkgreen","darkblue"),
  lwd = 2,
  cex = 0.6
)


# P = 0.35 glm
set.seed(1255797182)

#Creo 10 partizioni e le mescolo
folds <- cut(seq(1, nrow(nom)), breaks = 10, labels = FALSE)
folds <- folds[sample(nrow(nom))]

#Eseguo la validatione

accGlm = rep(0, 10)
cmGlm = matrix(nrow = 2, ncol = 2)

p = 0.35

for (i in 1:10) {
  testIndexes <- which(folds == i, arr.ind = TRUE)
  testData <- nom.log[testIndexes, ]
  trainData <- nom.log[-testIndexes, ]
  
  nom.glm = glm(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
                  Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama, family = binomial, data = trainData)
  nom.glm.p = predict(nom.glm, type = "response", newdata = testData)
  accGlm[i] = sum((nom.glm.p > p ) == (testData$Nomination > p )) / length(testData$Nomination)
  
  if (i > 1) {
    cmGlm[1, 1] = cmGlm[1, 1] + sum((nom.glm.p >=p ) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = cmGlm[1, 2] + sum((nom.glm.p >= p ) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = cmGlm[2, 1] + sum((nom.glm.p <= p ) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = cmGlm[2, 2] + sum((nom.glm.p <= p ) &(testData$Nomination == 0)) # true negative
    
    predGlm = c(predGlm, nom.glm.p)
    actualValue = c(actualValue, testData$Nomination)
    
  } else{
    cmGlm[1, 1] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 1)) # true positive
    cmGlm[1, 2] = sum((nom.glm.p >= 0.5) &(testData$Nomination == 0)) # false positive
    cmGlm[2, 1] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 1)) # false negative
    cmGlm[2, 2] = sum((nom.glm.p <= 0.5) &(testData$Nomination == 0)) # true negative

    predGlm = nom.glm.p
    actualValue = testData$Nomination
  }
}

cmGlm <- data.frame(cmGlm)
rownames(cmGlm) <- c("predicted 1", "predicted 0")
colnames(cmGlm) <- c("actual 1", "actual 0")
cmGlm

#Plot Partizioni ----

# Classi nominali all'interno del dataset
nom.log[,1] <- as.factor(t(as.vector(nom.classe)))

#qda
library(klaR)
partimat(Nomination ~ .- LikesFilm - SRR - BestWikiRankGlobale - Durata - NumeroAttori -
           Morti - BestWikiRankLocale - WikiPageQuality - Votanti - Budget - LunghezzaTrama -
           LikesProtagonista - IncassoDomestico - Anno - LikesRegista, data = nom.log, method = "qda", col.correct='darkgreen', col.wrong='red')

library(ggplot2)
ggplot(nom.log) +
  geom_point(aes(IncassoMondiale, Rating, colour = Nomination))