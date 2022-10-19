##Tarea 7
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez

#1. Con los los archivos de datos adjuntos en Excel.

#a) Cree archivos con formato .csv para su lectura en R.
library(readxl)
bd1_T7_bd01 <- read_excel("bd1-T7.xlsx", sheet = "bd01")
bd1_T7_bd02 <- read_excel("bd1-T7.xlsx", sheet = "bd02")
bd1_T7_bd03 <- read_excel("bd1-T7.xlsx", sheet = "bd03")


bd2_T7 <- read_excel("bd2-T7.xlsx")
names(bd2_T7) <- c("id-pais","pais", "IQ","PIB")
bd2_T7 <- bd2_T7[-c(1:2),c(1:4)]

wine.datos <- read_excel("wine.datos.xlsx")

write.csv(bd1_T7_bd01,"bd1_T7_bd01.csv")
write.csv(bd1_T7_bd02,"bd1_T7_bd02.csv")
write.csv(bd1_T7_bd03,"bd1_T7_bd03.csv")
write.csv(bd2_T7,"bd2_T7.csv")
write.csv(wine.datos,"wine.datos.csv")

#Lectura de archivos csv
bd1_T7_bd01 <- read.csv("bd1_T7_bd01.csv")
bd1_T7_bd02 <- read.csv("bd1_T7_bd02.csv")
bd1_T7_bd03 <- read.csv("bd1_T7_bd03.csv")


#b) Con el archivo 1 calcule la media ponderada de calificaciones por matrícula.

library(dplyr)


#Media ponderada
media_p <- bd1_T7_bd03%>% 
  group_by(id.est) %>% 
  summarise(media_ponderada = sum(calificacion * creditos)/sum(creditos))

#Media ponderada2
media_p2 <- bd1_T7_bd03%>% 
  group_by(id.est) %>% 
  mutate(weight = creditos/sum(creditos)) %>% 
  summarise(media_ponderada = sum(calificacion * weight))



#install.packages("matrixStats")
#library("matrixStats")   
#matrixStats::weightedMean()

#c) Realice un empalme (merge) de los datos por id-est y cree un objeto data.frame con todas las
#variables, incluyendo la columna de calif_ponderada.

union <- merge(x = bd1_T7_bd01, y = bd1_T7_bd02, z=bd1_T7_bd03, by = "id.est")

base_merge_mediap <- cbind(union[,-c(2,4)],media_p)

head(base_merge_mediap,4)

#d) Exporte el objeto creado a un archivo tipo csv.

write.csv(base_merge_mediap,"base_merge_mediap.csv")

#e) Con el archivo 2 de datos de coeficiente intelectual y producto interno bruto, cree un objeto
#data.frame con todos los datos, incluyendo los valores perdidos; luego, remplace los valores
#perdidos con la mediana de los datos correspondientes a la columna donde aparecen los datos
#perdidos.

bd2_T7 <- read_excel("bd2-T7.xlsx")
names(bd2_T7) <- c("id-pais","pais", "IQ","PIB")
bd2_T7 <- as.data.frame(bd2_T7[-c(1:2),c(1:4)])
class(bd2_T7)

#cambiar & por NA
bd2_T7[bd2_T7 == "&"] <- NA

#NA por fila y columna
which(is.na(bd2_T7), arr.ind=TRUE)
bd2_T7[!complete.cases(bd2_T7), ]

#omitir NAs
bd2_T7_sna <- na.omit(bd2_T7)

#convertir variables PIB e IQ a numericas
bd2_T7_sna[,c(3,4)] <- as.numeric(c(bd2_T7_sna$IQ,bd2_T7_sna$PIB))

summary(bd2_T7_sna$PIB) #median PIB= 7662

#se cambian los Na por la mediana
bd2_T7$PIB[is.na(bd2_T7$PIB)] <- 7662

#convertir variables PIB e IQ a numericas
bd2_T7[,c(3,4)] <- as.numeric(c(bd2_T7$IQ,bd2_T7$PIB))

#f)Explore los datos entre PIB e IQ

plot(bd2_T7)#matriz de diagramas de dispersion IQ y PIB tienen una ralacion no lineal

cor(bd2_T7$PIB,bd2_T7$IQ) # coeficiente de correlacion de 0.711

#g) Efectúe una regresión lineal entre PIB (var dependiente) e IQ (var independiente)

#Modelo lineal
ml <- lm(bd2_T7$PIB~bd2_T7$IQ)
summary(ml)

#Modelo polinomial de segundo orden
ml2 <- lm(bd2_T7$PIB~bd2_T7$IQ+I(bd2_T7$IQ^2))
summary(ml2)


library(ggplot2)
ggplot(bd2_T7, aes(x=IQ, y=PIB)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  geom_smooth(method='lm', formula=y~x+I(x^2), se=FALSE, col='tomato') +
  theme_light()
     
#h) Verifique la normalidad de los residuales del modelo.

shapiro.test(ml$residuals) #no se rechaza la Ho, los residuales del ml son normales

plot(ml) #varias graficas

#Histograma
hist(ml$residuals)

#qqplot
library(car)
qqPlot(ml$residuals)

#i)Elabore una gráfica para comparar el modelo estimado con los datos observados


plot(ml$fitted.values, bd2_T7$PIB,col="blue", pch=20)
abline(lm(ml$fitted.values~bd2_T7$PIB),col="red")

library(ggplot2)
ggplot(bd2_T7, aes(x=ml$fitted.values, y=PIB)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=T, col='dodgerblue1') +
  theme_light()


#j)Luego realice el mismo análisis realizando una transformación logarítmica de ambas
#variables; esto es, ln(PIB) vs ln(IQ)


#Modelo lineal
mllog <- lm(log(bd2_T7$PIB)~log(bd2_T7$IQ))
summary(mllog)


library(ggplot2)
ggplot(bd2_T7, aes(x=log(IQ), y=log(PIB))) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=T, col='dodgerblue1') +
  theme_light()

#normalidad de los residuales del modelo log

shapiro.test(mllog$residuals) #no se rechaza la Ho, los residuales del mllog son normales

plot(mllog) #varias graficas

#Histograma
hist(mllog$residuals)

#qqplot
library(car)
qqPlot(mllog$residuals)

#gráfica para comparar el modelo estimado con los datos observados


library(ggplot2)
ggplot(bd2_T7, aes(x=mllog$fitted.values, y=log(PIB))) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=T, col='dodgerblue1') +
  theme_light()







#k) Verifique la normalidad de los residuos del modelo.

#normalidad de los residuales del modelo log

shapiro.test(mllog$residuals) #no se rechaza la Ho, los residuales del mllog son normales

plot(mllog) #varias graficas

#Histograma
hist(mllog$residuals)

#qqplot
library(car)
qqPlot(mllog$residuals)

#l)Analice e interprete sus resultados (traduzca previamente, el texto en inglés de la
#descripción de los datos).

#La inteligencia, riqueza y pobreza de las naciones
#RICARDO LYNN; Universidad de Ulster, Coleraine, Irlanda del Norte
#TATU VANHANEN; Universidad de Helsinki, Finlandia

#RESUMEN
#Los coeficientes intelectuales nacionales evaluados por las Matrices Progresivas se calcularon para 60 naciones y
#fue examinada su relación con los ingresos per cápita a fines de la década de 1990  con las tasas de ingresos posteriores a la Segunda Guerra Mundial.
#Se encontró que los coeficientes intelectuales nacionales están correlacionados en 0,757 con el PIB real (Bruto
#Producto Interno) per cápita 1998 y 0.706 con PNB (Producto Nacional Bruto) per cápita
#1998; y en 0,605 con el crecimiento del PIB per cápita 1950-90 y 0,643 con el crecimiento del PIB per cápita
#PNB per cápita 1976-98. 

#Los resultados se interpretan en términos de un modelo causal en que los coeficientes intelectuales son 
#el principal determinante de la riqueza y la pobreza de las naciones en el mundo contemporáneo.

#Conclusión: es evidente que hay correlación entre el PIB e IQ, aunado a eso la correlación no es causalidad.
#Se pueden utilzar estas variables para hacer predicciones relativamente confiables

#2. Con base en el script ACP visto en clase, corra el script con los datos de wine.datos adjunto. Cree
#un archivo csv. consulte el link señalado para hacer una breve síntesis en español de la aplicación
#del Análisis de componentes principales para reducir dimensionalidad e introduzca comentarios
#al script. Elabore las gráficas indicadas con etiquetas en español.

wine.datos <- read_excel("wine.datos.xlsx")
write.csv(wine.datos,"wine.datos.csv")
head(wine.datos)

# cambiar nombre de variables a español
colnames(wine.datos) <- c("cultivar","alcohol","acido malico","minerales",
"alcalinidad de minerales", "magnesio", "fenoles totales", "flavonoides", 
"fenoles no flavonoides", "proantiocianinas", "intensidad de color", "matiz", 
"OD280/OD315 de vinos diluidos", "prolina")

# variable cualitativa (cultivar)
wineClasses <- factor(wine.datos$cultivar)

# matriz de diagramas de dispersion con pairs

pairs(wine.datos[,-1], col = wineClasses, upper.panel = NULL, pch = 16, 
      cex = 0.5)
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16,
       col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)


#seleccionar correlaciones más altas >=0.7
cor(wine.datos[,-1])
which(cor(wine.datos[,-1])>= 0.7, arr.ind=TRUE)

dev.off() # clear the format from the previous plot

#PCA
winePCA <- prcomp(scale(wine.datos[,- c(1, 2, 3, 14)]))
summary(winePCA) #con los primeros 3 PCAs se acumula 0.71% de varianza

#PCA1 vs PCA2
plot(winePCA$x[,1:2], col = wineClasses,pch=16)
abline(h=0)
abline(v=0)

#biplot
biplot(winePCA)

#PCA1 vs PCA2
winePCA2 <- princomp(scale(wine.datos[,- c(1, 2, 3, 14)]))
plot(winePCA2$scores[,1],winePCA2$scores[,2],pch=21,
     bg=c("blue","red","green")[unclass(wine.datos$cultivar)])
abline(h=0)
abline(v=0)

#En estadística, el análisis de componentes principales (en español ACP, en inglés, PCA) es una técnica utilizada para describir un conjunto de datos en términos de nuevas variables («componentes») no correlacionadas. 
#Los componentes se ordenan por la cantidad de varianza original que describen, por lo que la técnica es útil para reducir la dimensionalidad de un conjunto de datos.
#Técnicamente, el ACP busca la proyección según la cual los datos queden mejor representados en términos de mínimos cuadrados. Esta convierte un conjunto de observaciones de variables posiblemente correlacionadas en un conjunto de valores de variables sin correlación lineal llamadas componentes principales.
#El ACP se emplea sobre todo en análisis exploratorio de datos y para construir modelos predictivos. 


#Espacio de trabajo

save.image("T7COA501IFQI.RData")
