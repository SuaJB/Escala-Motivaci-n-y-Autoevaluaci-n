setwd("/Users/Sua/OneDrive/Tesis/Análisis/")
validacionEFA <-na.omit(read.csv("efa.csv", header=T, sep=";"))

valM<- validacionEFA[,2:16]
colnames(valM) <- c("   MI1  "," MEI1 ","MEE1", "   A1   ", "   MI2  ", " MEI2 ", "MEE2", "   A2   ", "   MI3  ", " MEI3 ", "MEE3", "   A3   ", "   MI4  ", " MEI4 ", "MEE4")
valAu<- validacionEFA[,17:29]


install.packages("psych")  ######PCA/EFA amongst many other things!
install.packages("REdaS")  ######produces KMO and Bartletts test
install.packages("readxl") ######reads excel
install.packages("GPArotation")
install.packages("psych")
install.packages("random.polychor.pa")
###############pull packages out of the library
library(random.polychor.pa)
library(GPArotation)
library(psych)
library(readxl)
library(REdaS)
## Escala SIMS ##

bart_spher(valM) ###### produces Bartletts test of spherecity (you want this to be significant)
KMO(valM)       ###### Kaiser-Meyer-Olkin measure, you want to be above .7
poly_values = polychoric(valM)
load("polychoric")
# Scree plot
random.polychor.pa(nrep=15, data.matrix=valM, q.eigen=.99, nstep=5)
fa.parallel(valM,fm="pa",fa="fa",main= "Scree Plot", cor="poly")
poly_model <- fa(valM, nfactor=4, cor="poly", fm="mle", rotate = "quartimin")
colnames(poly_model$loadings) <- c("MI", "MEE","MEI", "AM" )
summary(poly_model)
fa.diagram(poly_model)
poly_model$loadings

qqnorm(valM$MEE2, pch = 1, frame = FALSE)
qqline(valM$MEE2, col = "steelblue", lwd = 2)

?fa

## Encuesta SSCAT ##

bart_spher(valAu) ###### produces Bartletts test of spherecity (you want this to be significant)
KMO(valAu)       ###### Kaiser-Meyer-Olkin measure, you want to be above .7

# Scree plot
random.polychor.pa(nrep=15, data.matrix=valAu, q.eigen=.99, nstep=5)
fa.parallel(valAu, fm="pa", fa="fa", main = "Scree Plot", cor="poly")
poly_model2 <- fa(valAu, nfactor=3, cor="poly", fm="mle", rotate = "quartimin")
summary(poly_model2)
colnames(poly_model$loadings) <- c("MI", "MEE","MEI" )
fa.diagram(poly_model2)
poly_model2$loadings

qqnorm(valAu$M5, pch = 1, frame = FALSE)
qqline(valAu$M5, col = "steelblue", lwd = 2)

                   
