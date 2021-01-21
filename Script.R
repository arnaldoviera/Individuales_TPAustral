##limpiar ambiente de trabajo
rm(list =ls())


##setear directorio de trabajo
WD <- getwd()
Files_IDM <- paste


##Impostar Dataset
library(readxl)
cardio <- read_excel("cardio.xls")
View(cardio)

##Instalación de paquetes

lista_paquetes = c("funModeling","factoextra","caret","ggthemes","rpart", "rpart.plot","ggcorrplot","dplyr","corrplot",'tidyverse','Hmisc','dplyr','PerformanceAnalytics','psych','corrplot','readr','tidyverse', 'DescTools', 'here','blockcluster', 'knitr', 'readxl', 'ggplot2',"cowplot")
install.packages("PerformanceAnalytics")

nuevos_paquetes = lista_paquetes[!(lista_paquetes %in% installed.packages()[,"Package"])]

if(length(nuevos_paquetes)) install.packages(nuevos_paquetes, dependencies = TRUE)

##carga de librerías
library(dplyr)
library(readxl)
library(ggplot2)
library(corrplot)
library(psych)
library(ggcorrplot)
library(PerformanceAnalytics)	
library(ggthemes)
library(funModeling)
library(readr)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(DataExplorer)
library(gridExtra)
library(caret)
library(factoextra)
library(knitr)




##EDA
summary(cardio)	
glimpse(cardio)
print(status(cardio))
freq(cardio) 
print(profiling_num(cardio))
plot_num(cardio)
describe(cardio)
head(cardio)
str (cardio)


##evalua datos faltantes
plot_missing(cardio)


##pasar a factor género

cardio$sexo <- as.factor(cardio$sexo)

##a.	Describa la distribución univariada de las variables presente en el conjunto de datos.  ¿Se evidencian outliers en alguna de ellas?
plot_num(cardio)

##b. Calcule e interprete la matriz de correlaciones.

cardio_cont <- select(cardio,id,imc, perimetro_abdo, hto, glicemia, ct, hdl, tgd)
  
chart.Correlation(cardio_cont, histogram = F, pch = 19)

pairs.panels(cardio,pch='.') 

cor.plot(cor(cardio_cont))

corrplot(cor(cardio_cont), order = "hclust")


####  Analisis de componentes principales     ####   

xPCA<-prcomp(cardio_cont, scale=T)

summary(xPCA)

str(xPCA)

xPCA$rotation

xPCA$center

xPCA$scale

xPCA$x

# Analizar las correlaciones de las componentes con las variables observadas
cor(cardio_cont,xPCA$x)

# Crear el biplot

biplot(xPCA) 


## análisis de cluster: jerárquico, aglomerativos y politéticos (VER). Porque había pocas observacione (parto de cada individuo como grupo) sin hacer nada antes ya que son relativamente pocas variables.

# seleccionar solo las variables de las características nutricionales
vars_to_cluster <- c( "imc", "perimetro_abdo", "hto", "glicemia", "ct", "hdl", "tgd")

CCont_Cluster <- cardio_cont %>% 
  dplyr::select(all_of(vars_to_cluster))
summary(CCont_Cluster)

CCont_Cluster <- scale(CCont_Cluster)
summary(CCont_Cluster)

# Calcular las distancias
d   <- dist(CCont_Cluster, method = "euclidean")

# Realizar el agrupamiento jerárquico
fit <- hclust(d, method = "ward.D")

# Graficar el dendograma
plot(fit) 

# Indentificar los distintos clusters elegidos 
rect.hclust(fit, k = 4, border = "red")


# Indice Silhouette  ---
fviz_nbclust(CCont_Cluster, kmeans, method = "silhouette") +
  labs(title    = "Número óptimo de clusters a considerar",
       subtitle = "Indice Silhouette")

# Suma de cuadrado error (o within)---
wss <- (nrow(CCont_Cluster) - 1) * sum(apply(CCont_Cluster, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(CCont_Cluster, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters (within)")

groups <- cutree(fit, k = 4) 

# Unir los clusters formado con los datos originales para poder describir los grupos
CCont_Cluster         <- data.frame(CCont_Cluster)
CCont_Cluster$cluster <- groups
cardio_cont$cluster  <- groups


# Caracterizar los segmentos encontrados (utilizando las variables estandarizadas)
formula_para_describir <- as.formula(
  paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

tablaResumen <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = CCont_Cluster
)

kable(
  tablaResumen %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)


# Caracterizar los segmentos encontrados (utilizando las variables originales)
tablaResumen <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = CCont_Cluster
)

kable(
  tablaResumen %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)


