##limpiar ambiente de trabajo
rm(list =ls())


##setear directorio de trabajo
getwd()
setwd("C:/Users/vieraa/Documents/GitHub/Individuales_TPAustral")
Files_IDM <- paste


##Impostar Dataset
library(readxl)
cardio <- read_excel("cardio.xls")

cardiosID <- select(cardio, -id)

##Instalación de paquetes

lista_paquetes = c("funModeling","factoextra", "MVN","caret","ggthemes","rpart", "rpart.plot","ggcorrplot","dplyr","corrplot",'tidyverse','Hmisc','dplyr','PerformanceAnalytics','psych','corrplot','readr','tidyverse', 'DescTools', 'here','blockcluster', 'knitr', 'readxl', 'ggplot2',"cowplot")

nuevos_paquetes = lista_paquetes[!(lista_paquetes %in% installed.packages()[,"Package"])]

if(length(nuevos_paquetes)) install.packages(nuevos_paquetes, dependencies = TRUE)
install.packages("PerformanceAnalytics")

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
library(MVN)
library("plyr")
library(rpart)
library(rpart.plot)
library(caret)


##EDA
summary(cardio)	
glimpse(cardio)
str(cardio)
names(cardio)
class(cardio)
print(status(cardio))
print(profiling_num(cardio))
plot_num(cardio)
describe(cardio)
head(cardio)
str (cardio)


##evalua datos faltantes
plot_missing(cardio)


##pasar a factor género

cardio$sexo <- as.factor(cardio$sexo)

cardiosID <- select(cardio, -id)

##a.	Describa la distribución univariada de las variables presente en el conjunto de datos.  ¿Se evidencian outliers en alguna de ellas?
plot_num(cardio)
##RTAa Se evidencian outliers en triglicéridos (tgd), glicemia,Hematorcrito (hto). No sólo son poco frecuentes, sino que en los gráficos de frecuencia se evidencia una interrupción de la curva.  

##b. Calcule e interprete la matriz de correlaciones.

cardio_cont <- select(cardiosID,imc,perimetro_abdo, hto, glicemia, ct, hdl, tgd)
  
chart.Correlation(cardio_cont, histogram = F, pch = 19)

##este es el mejor!
pairs.panels(cardiosID,pch='.') 

cor.plot(cor(cardio_cont))

corrplot(cor(cardio_cont), order = "hclust", addrect = 3, tl.pos = "d")
##RTAB: Con respecto a las correlaciones, se destaca una que surje a simple vista: imc/perímetro_abdo. Entendiendo el origen de cada variable y cómo se contruye, tiene sentido que tengan una fuerte correlación positiva: a medida que la relación del peso y la altura suben, el perímetro abdominarl también debería subir. Lo que quizás cabe destacar que las distintas variables tienen información redundante (lo veremos en el próximo ejercicio). 

##El resto de las correlaciones no son significativas, sean estas positivas o negativas. Lo que quizás llama la atención es que la variable hdl (que vendría a ser el colesterol "bueno") tiene una leve correlación negativa con el resto de las variables.
## En cambio, con el ct (colesterol total ) tiene correlación casi nula, aún cuando el hdl forma parte de la medición del ct. En este sentido, podríamos  inferir que la no-correlación entre hdl y ct se debe a una variable que también forma parte del ct, pero que no está incluida en el TP: el ldl o colesterol "malo".



####  Analisis de componentes principales     ####   

##estamos usando matriz R y no sigma
xPCA<-prcomp(cardio_cont, scale=T)

summary(xPCA)

str(xPCA)
#RTAC:Siguiendo con el análisis anterior, al tener poca correlación entre las variables, no existen pocos componentes que nos expliquen los datos. Para llegar a un 80% o más de explicación tenemos que llegar a la componente 5 de las 8 que estamos analizando. 
##por otro lado las primeras 2 componentes logran explicar uin 45% de variabilidad y al ser una variable categórica, no logran explicar el sexo..
xPCA$x

xPCA$rotation

xPCA$center

xPCA$scale

xPCA$x

view(xPCA)

# Analizar las correlaciones de las componentes con las variables observadas
cor(cardio_cont,xPCA$x)

# Crear el biplot
## esto está mal?
biplot(xPCA) 


## análisis de cluster: jerárquico, aglomerativos y politéticos (VER). Porque había pocas observacione (parto de cada individuo como grupo) sin hacer nada antes ya que son relativamente pocas variables.

# seleccionar solo las variables de las características nutricionales
vars_to_cluster <- c("imc", "perimetro_abdo" , "hto", "glicemia", "ct", "hdl", "tgd")

CCont_Cluster <- cardiosID %>% 
  dplyr::select(all_of(vars_to_cluster))
summary(CCont_Cluster)

CCont_Cluster <- scale(CCont_Cluster)
summary(CCont_Cluster)

# Calcular las distancias euclidea
d   <- dist(CCont_Cluster, method = "euclidean")

# Realizar el agrupamiento jerárquico
fit <- hclust(d, method = "ward.D")

# Graficar el dendograma
plot(fit) 

# Indentificar los distintos clusters elegidos con 4 clusters
rect.hclust(fit, k = 2, border = "green")
##me quedé con 2

# Indice Silhouette  ---
fviz_nbclust(CCont_Cluster, kmeans, method = "silhouette") +
  labs(title    = "Número óptimo de clusters a considerar",
       subtitle = "Indice Silhouette")
##me quedo con 2 "en teoría"

# Suma de cuadrado error (o within)---
wss <- (nrow(CCont_Cluster) - 1) * sum(apply(CCont_Cluster, 2, var))
for (i in 2:10) {
  wss[i] <- sum(kmeans(CCont_Cluster, centers = i)$withinss)
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Suma de cuadrados dentro de los clusters (within)")

groups <- cutree(fit, k = 2) 


# Unir los clusters formado con los datos originales para poder describir los grupos
CCont_Cluster         <- data.frame(CCont_Cluster)
CCont_Cluster$cluster <- groups
cardio_cont$cluster  <- groups


# Caracterizar los segmentos encontrados (utilizando las variables estandarizadas)
formula_para_describir <- as.formula(paste0( paste(vars_to_cluster, collapse = " + "), " ~ cluster") 
)

tablaResumen <- describeBy(formula_para_describir, 
  mat = TRUE, 
  data = CCont_Cluster
)

kable(
  tablaResumen %>% 
    dplyr::mutate(variable = rownames(.),
                  cv= 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)

# Caracterizar los segmentos encontrados (utilizando las variables originales)
tablaResumenO <- describeBy(
  formula_para_describir, 
  mat = TRUE, 
  data = cardio_cont
)

kable(
  tablaResumenO %>% 
    dplyr::mutate(variable = rownames(.),
                  cv       = 100 * sd / abs(mean) ) %>% 
    dplyr::rename(cluster = group1) %>% 
    dplyr::select(variable, cluster, n, mean, median, cv, min, max) %>% 
    arrange(as.numeric(as.character(cluster))), 
  digits = 2
)
view(tablaResumen)


##RTAD: A partir del análisis de cluster realizado, detectamos 2 grandes grupos. El grupo 1 con la mayoría la media de las variables más bajas, menos el hdl o colesterol "bueno".


## acá viene el tema del árbol


##construir variable dicotómica de obesidad

##factorizar variables
##factorizar variables
cardio <- cardio %>% 
  mutate(obesidad=factor(obesidad), 
         sexo=factor(sexo))


##crear variable
cardio<-cardio %>% 
  group_by(imc) %>%  
  mutate(obesidad = ifelse(30 < imc, 1, 0))

ind <- sample(2, nrow(cardio), replace = TRUE, prob = c(0.7, 0.3))
cardio_train <- cardio[ind == 1, ]
cardio_test <- cardio[ind == 2, ] 


## Almacenar Fórmula. Se descartan los nombres, ID y Cabina. 
formula_cardio <- formula(obesidad ~  sexo + hto + glicemia + ct + hdl + tgd)


##Armado del arbol 
arbol <- rpart(formula_cardio, data = cardio_train, 
                 control = rpart.control(
                   minbucket = 1, 
                   minsplit = 1, 
                   maxdepth=30,
                   CP = 0
                 ))

##gráficos 4
prp(arbol, extra=101, type=2,  xsep="/", box.palette = "auto",
    round=0, leaf.round = 2, shadow.col = "gray", yes.text="Si", 
    no.text = "No")
rpart.plot(arbol)
printcp (arbol)
plotcp (arbol)


## Poda del Arbol 
Parbol <- prune(arbol, cp = arbol$cptable[which.min(arbol$cptable[,"xerror"]), "CP"] )
printcp(Parbol)
rpart.plot(Parbol)

##predicción sobre Train con arbol podado
Prediccion1 <- predict(Parbol, newdata = cardio_train)
table(Prediccion1, cardio_train$obesidad)
sum((Prediccion1 == cardio_train$obesidad) / length(cardio_train$obesidad))*100


