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

#b) Con el archivo 1 calcule la media ponderada de calificaciones por matrícula.

#c) Realice un empalme (merge) de los datos por id-est y cree un objeto data.frame con todas las
#variables, incluyendo la columna de calif_ponderada.

#d) Exporte el objeto creado a un archivo tipo csv.

#e) Con el archivo 2 de datos de coeficiente intelectual y producto interno bruto, cree un objeto
#data.frame con todos los datos, incluyendo los valores perdidos; luego, remplace los valores
#perdidos con la mediana de los datos correspondientes a la columna donde aparecen los datos
#perdidos.

#f)Explore los datos entre PIB e IQ

#g) Efectúe una regresión lineal entre PIB (var dependiente) e IQ (var independiente)

#h) Verifique la normalidad de los residuales del modelo.

#i)Elabore una gráfica para comparar el modelo estimado con los datos observados

#j)Luego realice el mismo análisis realizando una transformación logarítmica de ambas
#variables; esto es, ln(PIB) vs ln(IQ)

#k) Verifique la normalidad de los residuos del modelo.

#l)Analice e interprete sus resultados (traduzca previamente, el texto en inglés de la
#descripción de los datos).

#2. Con base en el script ACP visto en clase, corra el script con los datos de wine.datos adjunto. Cree
#un archivo csv. consulte el link señalado para hacer una breve síntesis en español de la aplicación
#del Análisis de componentes principales para reducir dimensionalidad e introduzca comentarios
#al script. Elabore las gráficas indicadas con etiquetas en español.

head(wine.datos)

# Name the variables
colnames(wine.datos) <- c("Cvs","Alcohol","Malic acid","Ash","Alcalinity of ash", "Magnesium", "Total phenols", "Flavanoids", "Nonflavanoid phenols", "Proanthocyanins", "Color intensity", "Hue", "OD280/OD315 of diluted wines", "Proline")

# The first column corresponds to the classes
wineClasses <- factor(wine.datos$Cvs)

# Use pairs

pairs(wine.datos[,-1], col = wineClasses, upper.panel = NULL, pch = 16, 
      cex = 0.5)
legend("topright", bty = "n", legend = c("Cv1","Cv2","Cv3"), pch = 16,
       col = c("black","red","green"),xpd = T, cex = 2, y.intersp = 0.5)



dev.off() # clear the format from the previous plot
winePCA <- prcomp(scale(wine.datos[,- c(1, 2, 3, 14)]))

summary(winePCA)

plot(winePCA$x[,1:2], col = wineClasses)


#Espacio de trabajo

save.image("T7COA501IFQI.RData")
