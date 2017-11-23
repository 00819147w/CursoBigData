# Crear carpeta de trabajo
setwd("g:/MASTER BIG DATA/proyectosR/Modulo3/ejercicio")
library(knitr)

# Crear carpeta para el curso unidad 2
if(!file.exists("./datosrow")){dir.create("./datosrow")}

# Descargamos fichero de trabajo y descomprimimos
fileURL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"
download.file(fileURL,destfile="./datosrow/student.zip") 
unzip("./datosrow/student.zip", exdir="./datosrow")

#Verificamos ficheros
list.files("./datosrow")

#Importamos los dos ficheros csv tablas
EstudiosMat<-read.table("./datosrow/student-mat.csv",
                        row.names=NULL, sep=";", header=TRUE)
EstudiosPor<-read.table("./datosrow/student-por.csv",
                        row.names=NULL, sep=";", header=TRUE)
summary(EstudiosMat)
#vemos que hay 353 filas. Y para valores numéricos no hay datos fuera de margen. Las notas G1
#g2 y g3, por ejemplo, vemos que sus mínimos son cero o superior y el máximo 20 o inferior.
summary(EstudiosPor)

#Ponemos los nombres de Columnas en minúsculas para evitar errores
names(EstudiosMat) <- tolower(names(EstudiosMat))
names(EstudiosPor) <- tolower(names(EstudiosPor))

#Primer análisis similar al caso anterior. En una primera imagen no observamos valores fuera de rango
#Verificamos que no se ha perdido ningún valor

prueba<-(EstudiosMat[is.na(EstudiosMat)==TRUE])
prueba
prueba<-(EstudiosPor[is.na(EstudiosPor)==TRUE])
prueba


#merge de las dos tablas
library(dplyr)
EstudiosMatPor <- merge(EstudiosMat,EstudiosPor, by=c("school","sex","age", "address",
                                                      "famsize", "pstatus","medu", "fedu",
                                                      "mjob", "fjob","reason", "nursery",
                                                      "internet"), all=FALSE, suffixes=c("mat","por")) 
dim(EstudiosMatPor)[1] 

names(EstudiosMatPor)
Diferencias<-EstudiosMatPor[,c(16:18,20,30:33,36:38,40,50:53)]
Diferencias$passpor<-ifelse(Diferencias$g3mat>9,1,0)
Diferencias$passmat<-ifelse(Diferencias$g3mat>9,1,0)
head(Diferencias)
names(Diferencias)

#Vemos que los campos diferentes son failures, paid y absences.

library(corrplot)
#Veamos dichas relaciones

pairs(Diferencias[,c(2,4:8,10,12:18)])
str(Diferencias[,c(2,4:8,10,12:18)])
#Pasamos a valor numérico
Diferencias$paidpor<-ifelse(Diferencias$paidpor=="no",0,1)
Diferencias$paidmat<-ifelse(Diferencias$paidmat=="no",0,1)

#ANALISIS EXPLORATORIO
CorDiferencias<-cor(Diferencias[,c(2,4:8,10,12:18)])
corrplot(CorDiferencias,type="upper")



#Generamos variable de si han aprobado o no
EstudiosMat$pass<-ifelse(EstudiosMat$g3>9,1,0)
EstudiosPor$pass<-ifelse(EstudiosPor$g3>9,1,0)
library(ggplot2)

qplot(factor(EstudiosMat$mjob))
ggplot(EstudiosMat, aes(goout)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(absences)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(famsize)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(pstatus)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(famsize)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(failures)) + geom_bar()+ facet_wrap(~ pass)
#proporcion failures menor si aprueban
ggplot(EstudiosMat, aes(age)) + geom_bar()+ facet_wrap(~ pass)
#ages varia en función de la edad
ggplot(EstudiosMat, aes(higher)) + geom_bar()+ facet_wrap(~ pass)
#mayor proporción los que aprueban que quieren ir a estudios superiores
ggplot(EstudiosMat, aes(famrel)) + geom_bar()+ facet_wrap(~ pass)
#mejores relaciones familiares
ggplot(EstudiosMat, aes(dalc)) + geom_bar()+ facet_wrap(~ pass)
#menos alcohol
ggplot(EstudiosMat, aes(romantic)) + geom_bar()+ facet_wrap(~ pass)
#menos romanticos
ggplot(EstudiosMat, aes(freetime)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(internet)) + geom_bar()+ facet_wrap(~ pass)
ggplot(EstudiosMat, aes(failures)) + geom_bar()+ facet_wrap(~ medu)
ggplot(EstudiosMat, aes(failures)) + geom_bar()+ facet_wrap(~ fedu)
ggplot(EstudiosMat, aes(failures)) + geom_bar()+ facet_wrap(~ studytime)
#la educación de los padres y el tiempo de estudio está correlacionado con failures. Más educación menos fallos
ggplot(EstudiosMat, aes(medu)) + geom_bar()+ facet_wrap(~ higher)
ggplot(EstudiosMat, aes(fedu)) + geom_bar()+ facet_wrap(~ higher)
ggplot(EstudiosMat, aes(studytime)) + geom_bar()+ facet_wrap(~ higher)


#Ver diferencias entre ambas escuelas

avgGrades <- (EstudiosMat %>% group_by(school) %>% summarise( meang1=mean(g1),meang2=mean(g2), meang3=mean(g3)))
avgGrades
avgGrades <- (EstudiosPor %>% group_by(school) %>% summarise( meang1=mean(g1),meang2=mean(g2), meang3=mean(g3)))
avgGrades
#Las medias mejoran ligeramente en Portugués en la escuela GP en los últimos periodos.


###ejecución del estudio para ver correlaciones

#Unión de las dos tablas. Aunque hay 300 estudiantes iguales, tomamos la observación repetida
#para reforzar correlaciones diferentes como fallos, notas anteriores, etc.
Estudios<-bind_rows(EstudiosMat,EstudiosPor)
dim(Estudios) 



#Analizar qué ocurre con las mejores notas
prueba<-subset(Estudios,g3>15)
summary(prueba)
prueba1<-subset(Estudios,g3<6)
summary(prueba1)

#mejores notas., guardian madre, traveltime poco, FAILURES, SCHOOLSUP,HIGHER, internet
#famrel muy buena. Alohol poco, AUSENCIAS. Muy relacionadas, failures, schoolsup, higher,famrel,
#alc,dalc,absences,g1,g2
#peores notas.medu,fedu,FAILURES, NO SCHOOLSUP

#Generamos tabla con datos fantasma. Quitamos el trabajo dado que no hay correlacion importante
Estudios$pass<-ifelse(Estudios$g3<9,0,1)
EstudiosFantasma<-Estudios
EstudiosFantasma$sex<-ifelse(EstudiosFantasma$sex=="F",1,2)
EstudiosFantasma$pstatus<-ifelse(EstudiosFantasma$pstatus=="A",1,2)
EstudiosFantasma$nursery<-ifelse(EstudiosFantasma$nursery=="no",0,1)
EstudiosFantasma$internet<-ifelse(EstudiosFantasma$internet=="no",0,1)
EstudiosFantasma$schoolsup<-ifelse(EstudiosFantasma$schoolsup=="no",0,1)
EstudiosFantasma$famsup<-ifelse(EstudiosFantasma$famsup=="no",0,1)
EstudiosFantasma$higher<-ifelse(EstudiosFantasma$higher=="no",0,1)
EstudiosFantasma$romantic<-ifelse(EstudiosFantasma$romantic=="no",0,1)

#Eliminamos campos con poca variación.
EstudiosFantasmaRed<-EstudiosFantasma[,c(2,3,6:8,13:17,21:27,30:34)]
EstudiosRed<-Estudios[,c(2,3,6:8,13:17,21:27,30:34)]


#CORRELACIONES

CorrEstudiosFantasmaRed<-cor(EstudiosFantasmaRed)
corrplot(CorrEstudiosFantasmaRed,type="upper")


##se demuestra que los valores son g1,g2,g3 y en menor medida y por orden, failure, higher y age


#FACTORIZACIÓN DE LOS ELEMENTOS
AnalisisEstudios<-(EstudiosRed[,1:21])

AnalisisEstudios$medu<-as.factor(AnalisisEstudios$medu)
AnalisisEstudios$fedu<-as.factor(AnalisisEstudios$fedu)
AnalisisEstudios$traveltime<-as.factor(AnalisisEstudios$traveltime)
AnalisisEstudios$studytime<-as.factor(AnalisisEstudios$studytime)
AnalisisEstudios$famrel<-as.factor(AnalisisEstudios$famrel)
AnalisisEstudios$freetime<-as.factor(AnalisisEstudios$freetime)
AnalisisEstudios$goout<-as.factor(AnalisisEstudios$goout)
AnalisisEstudios$dalc<-as.factor(AnalisisEstudios$dalc)
AnalisisEstudiosPass<-AnalisisEstudios[,1:20]
AnalisisEstudiosPass$Pass<-Estudios[,34]

#probamos Agnes, para ver outliers, otras relaciones, etc.

#install.packages("cluster",dependencies=TRUE)
#install.packages("dendroextras",dependencies=TRUE)
library(cluster)
library(dendroextras)

set.seed(1234)
prueba<-select(AnalisisEstudiosPass,-Pass)
idsamp <- sample(1:dim(prueba)[1], 75)
prueba.samp <- prueba[idsamp, ]
hc.clust <- hclust(dist(prueba.samp), method = "ave")
plot(hc.clust, hang = -1)
plot(hc.clust, hang = -1, labels = AnalisisEstudiosPass$Pass[idsamp])

# se corta el gráfico del arbol en cuatro grupos rect.hclust(hc.clust, k = 4)
#el atributo label de cutree da el numero de la observación
rect.hclust(hc.clust, k = 4)
cutree(hc.clust, k=4)
agnes.clust <- agnes(x=prueba.samp, diss = FALSE, stand = TRUE, method = "average")
dend.agnes <-as.dendrogram(agnes.clust)


plot(colour_clusters(dend.agnes, k=4), xlab="")
labels(dend.agnes)<- as.character(AnalisisEstudiosPass$Pass[agnes.clust$order])
plot(colour_clusters(dend.agnes, k=4), xlab="")


#Analizarmos los clusters finales para ver datos. Vemos que los dos finales
#aunque no tienen intención de realizar estudios superiores, pasan. El segundo con notas
#g1,g2 por debajo de 10
AnalisisEstudiosPass[c(999,953,896,718,312),]


#APRENDIZAJE SUPERVISADO.- REGRESION LINEAL

library(gplots)
library(ROCR)

set.seed(1234)
train.sample <- sample(1:1044,size=(800),replace=F)
train.EstudiosPass <- AnalisisEstudiosPass[train.sample,]
test.EstudiosPass <- AnalisisEstudiosPass[-train.sample,]
dim(train.EstudiosPass)

logistic.reg.model <- glm(Pass ~.,family=binomial,data=train.EstudiosPass) 
summary(logistic.reg.model)

# vemos si funciona con la Predicción

test.class.output <- predict(logistic.reg.model,newdata=test.EstudiosPass,type='response') 
head(test.class.output)

test.class.output <- ifelse(test.class.output > 0.5,1,0) 
head(test.class.output)

#Veamos la precisión. Valoración del modelo curva ROC

clasif.error <- mean(test.class.output != test.EstudiosPass$Pass) 
clasif.error
precision <- 1 - clasif.error
precision
pr <- prediction(test.class.output, test.EstudiosPass$Pass) 
prf <- performance(pr, measure = "tpr", x.measure = "fpr") 
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#repetir lo mismo pero para menos variables más representativas del analisis exploratorio
AnalisisEstudiosPassR<-AnalisisEstudiosPass[,c(2,8,11,18:21)]

set.seed(1234)
train.sample <- sample(1:1044,size=(800),replace=F)
train.EstudiosPass <- AnalisisEstudiosPassR[train.sample,]
test.EstudiosPass <- AnalisisEstudiosPassR[-train.sample,]
dim(train.EstudiosPass)

logistic.reg.model <- glm(Pass ~.,family=binomial,data=train.EstudiosPass) 
summary(logistic.reg.model)

# vemos si funciona con la predicción

test.class.output <- predict(logistic.reg.model,newdata=test.EstudiosPass,type='response') 
head(test.class.output)

test.class.output <- ifelse(test.class.output > 0.5,1,0) 
head(test.class.output)

#Veamos la precisión. valoración del modelo. curva ROC

clasif.error <- mean(test.class.output != test.EstudiosPass$Pass) 
clasif.error
precision <- 1 - clasif.error
precision
pr <- prediction(test.class.output, test.EstudiosPass$Pass) 
prf <- performance(pr, measure = "tpr", x.measure = "fpr") 
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#tenemos una precisión de un 95% con un AUC (area under curve) de un 91,4%


#APRENDIZAJE SUPERVISADO - ARBOL DE DECISIÓN
library(party)
AnalisisEstudiosPass$Pass<-as.factor(ifelse(AnalisisEstudiosPass$Pass==0,"No Pasa","Pasa"))
set.seed(1234)
train.sample <- sample(1:1044,size=(800),replace=F)
train.EstudiosPass <- AnalisisEstudiosPass[train.sample,]
test.EstudiosPass <- AnalisisEstudiosPass[-train.sample,]
dim(train.EstudiosPass)

ctree.model <- ctree(Pass ~ ., data=train.EstudiosPass)
print(ctree.model)
plot(ctree.model)
plot(ctree.model, type="simple")

#Validación del modelo con los datos de test

test.EstudiosPass.pred <- predict(ctree.model, newdata = test.EstudiosPass) 
table(test.EstudiosPass.pred, test.EstudiosPass$Pass)

#repitamos los valores pero con solo los datos principales
AnalisisEstudiosPassR$Pass<-as.factor(ifelse(AnalisisEstudiosPassR$Pass==0,"No Pasa","Pasa"))
set.seed(1234)
train.sample <- sample(1:1044,size=(800),replace=F)
train.EstudiosPass <- AnalisisEstudiosPassR[train.sample,]
test.EstudiosPass <- AnalisisEstudiosPassR[-train.sample,]
dim(train.EstudiosPass)

ctree.model <- ctree(Pass ~ ., data=train.EstudiosPass)
print(ctree.model)
plot(ctree.model)
plot(ctree.model, type="simple")

test.EstudiosPass.pred <- predict(ctree.model, newdata = test.EstudiosPass) 
table(test.EstudiosPass.pred, test.EstudiosPass$Pass)

#En este segundo gráfico se vé la importancia de la educación temprana.

