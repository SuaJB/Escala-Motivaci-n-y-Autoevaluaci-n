install.packages("foreign", dependencies=TRUE)
install.packages("lavaan", dependencies=TRUE)
install.packages("semPlot", dependencies=TRUE)
install.packages("equaltestMI")
install.packages("semTools")
install.packages("polycor")
install.packages("Rcsdp")
install.packages("remotes")
remotes::install_github("jsaraviadrago/bluegrafi")
library(bluegrafir)
library(Rcsdp)
library(polycor)
library(semTools)
library(equaltestMI)
library(foreign) 
library(lavaan)
library(semPlot)
library(psych)


setwd("/Users/Sua/OneDrive/Tesis/Análisis/EFA.CFA/")
validacioncfa <-na.omit(read.csv("cfa.csv", header=T, sep=";"))
validacionEFA <-na.omit(read.csv("efa.csv", header=T, sep=";"))

valM<- validacionEFA[,2:16]
valm<- validacioncfa[,2:16]
colnames(valm) <- c("MI1","MEI1","MEE1", "A1", "MI2", "MEI2", "MEE2", "A2", "MI3", "MEI3", "MEE3", "A3", "MI4", "MEI4", "MEE4")
colnames(valM) <- c("MI1","MEI1","MEE1", "A1", "MI2", "MEI2", "MEE2", "A2", "MI3", "MEI3", "MEE3", "A3", "MI4", "MEI4", "MEE4")
valau<- validacioncfa[,17:28]
colnames(valau) <- c( "AU1" , "AU2" , "AU3",  "AU4" , "AU6",  "AU7",  "AU8",  "AU9",  "AU10", "AU11",
                      "AU12","AU13")


m1a  <- ' SA =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '

onefac3items_au <- cfa(m1a, data=valau, ordered=TRUE, estimator="ULS") 

summary(onefac3items_au, fit.measures=TRUE, standardized=TRUE)

fitMeasures(onefac3items_au, c("df","chisq","cfi", "tli","srmr", "rmsea" ))


glb.algebraic(valau)

comp_reliability(onefac3items_au)

semPaths(onefac3items_au, what = "std", fade=F)

## https://stackoverflow.com/questions/41395611/cfa-in-r-lavaan-with-ordinal-data-polychoric-correlation-included
## Maximum Likelihood ( ML ) and Diagonally Weighted Least Squares ( DWLS ) Estimation Procedures : A Comparison of Estimation Bias with Ordinal and Multivariate Non-Normal Data




m2a  <- ' MI =~  MI3 + MI1 + MI2  + MI4 
          MEI=~ MEI1 + MEI2 + MEI3
          MEE =~  MEE1 + MEE2 + 1*MEE3 
          AM =~ A3 + MEE4  + A1'

onefac3items_m <- cfa(m2a, data=valm, ordered=TRUE, estimator="ULS") 

summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE)

fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))


https://www.researchgate.net/post/What_fit_thresholds_for_CFI_TLI_RMSEA_when_using_DWLS_in_CFA_and_SEM



semPaths(onefac3items_m, what = "std", fade=F)
semPaths(onefac3items_m, "par", edge.label.cex = 1.2, fade = FALSE)
alpha(abs(valm[,c(9,5,1,13)])) 
alpha(abs(valm[,c(10,2,6,14)]))
alpha(abs(valm[,c(3,7,11,15,3,7,11,15)]))
?semPaths
alpha(valm, check.keys = T)

glb.algebraic(valm)


##Multigroup CFA
#SIMS
grupo <- rep(1,99)
valm <- cbind(grupo,valm )
grupo <- rep(2,100)
valM <- cbind(grupo, valM)
master <- rbind(valm, valM)
master$grupo <- as.factor(master$grupo)
str(master)

m2a  <- ' MI =~ MI3 + MI1 + MI2  + MI4 
          MEI=~ MEI1 + MEI2 + MEI3
          MEE =~ MEE1 + MEE2 + 1*MEE3
          AM =~ A3 + MEE4  + A1'

#Si ambos grupos se relacionan igual las variables con los factores
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS", group = "grupo") 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))

#si ambos grupos tienen los mismos loadings
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE,
                      estimator="ULS", group = "grupo",
                      group.equal = "loadings",
                      parameterization= "theta") 
summary(onefac3items_m)
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))

#si ambos grupos tienen los mismos loadings
m2a  <- ' MI =~ MI3 + MI1 + MI2  + MI4 
          MEI=~ MEI1 + MEI2 + MEI3
          MEE =~ MEE1 + MEE2 + 1*MEE3
          AM =~ A3 + MEE4  + A1
          MEE3 ~~ c(1, 1)*MEE3'
onefac3items_m <- cfa(m2a, data=master, ordered = TRUE,
                      estimator="ULS", group = "grupo",
                      group.equal = c("loadings","thresholds"),
                      parameterization= "theta") 
summary(onefac3items_m)
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))

#si ambos grupos empiezan en el mismo lugar

m2a  <- ' MI =~  MI3 + MI1 + MI2  + MI4 
          MEI=~ MEI1 + MEI2 + MEI3
          MEE =~  MEE1 + MEE2 + 1*MEE3 
          AM =~ A3 + MEE4  + A1'

onefac3items_m1 <- cfa(m2a, data=master, ordered = TRUE, estimator="ULS",
                      group = "grupo",
                      group.equal = c("loadings", "thresholds", "residuals"),
                      parameterization= "theta") 

fitMeasures(onefac3items_m1, c("df","chisq","cfi", "tli","srmr", "rmsea" ))

#si el error entre items es el mismo entre grupos. Me dice si hay mas variabilidad dentro del grupo

onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals")) 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))
summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#Covarianza de Residuales de las variables observadas
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals",
                                      "residual.covariances")) 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))
summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#Variación entre los factores de los grupos
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals",
                                      "residual.covariances", "lv.variances")) 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))
summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#Covarianción entre los factores de los grupos
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals",
                                      "lv.variances","residual.covariances",
                                      "lv.covariances")) 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))
summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#SSCAT

m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
colnames(valAu) <- c( "AU1" , "AU2" , "AU3",  "AU4" ,"AU5", "AU6",  "AU7",  "AU8",  "AU9",  "AU10", "AU11",
                     "AU12","AU13")
valAu <- valAu[,-5]
grupo <- rep(1,99)
valau <- cbind(grupo,valau )
grupo <- rep(2,100)
valAu <- cbind(grupo, valAu)
master <- rbind(valau, valAu)
master$grupo <- as.factor(master$grupo)
unique(valAu[,10])

str(master)

table(is.na(master))
#Si ambos grupos se relacionan igual las variables con los factores
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      parameterization= "theta") 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))

summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#si ambos grupos tienen los mismos loadings
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE,
                      estimator="ULS", meanstructure = TRUE, group = "grupo",
                      group.equal = "loadings",
                      parameterization= "theta") 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))


#si el error entre items es el mismo entre grupos. Me dice si hay mas variabilidad dentro del grupo

m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2'

onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "thresholds"),
                      parameterization= "delta") 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))

summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#Covarianza de Residuales de las variables observadas
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals","thresholds"),
                      parameterization= "theta") 

fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))


summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#Variación entre los factores de los grupos
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals",
                                      "residual.covariances", "lv.variances")) 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))
summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)

#Covarianción entre los factores de los grupos
onefac3items_m <- cfa(m2a, data=master, ordered=TRUE, estimator="ULS",
                      meanstructure = TRUE, group = "grupo",
                      group.equal = c("loadings", "residuals",
                                      "lv.variances","residual.covariances",
                                      "lv.covariances")) 
fitMeasures(onefac3items_m, c("df","chisq","cfi", "tli","srmr", "rmsea" ))
summary(onefac3items_m, fit.measures=TRUE, standardized=TRUE, rsquare=T)