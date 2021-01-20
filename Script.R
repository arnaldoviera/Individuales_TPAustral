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

lista_paquetes = c("funModeling","caret","ggthemes","rpart", "rpart.plot","ggcorrplot","dplyr","corrplot",'tidyverse','Hmisc','dplyr','PerformanceAnalytics','psych','corrplot','readr','tidyverse', 'DescTools', 'here','blockcluster', 'knitr', 'readxl', 'ggplot2',"cowplot")
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


##EDA
summary(cardio)	
glimpse(cardio)
print(status(cardio))
freq(cardio) 
print(profiling_num(cardio))
plot_num(cardio)
describe(cardio)
head(cardio)


##evalua datos faltantes
plot_missing(cardio)


##pasar a factor género

cardio$sexo <- as.factor(cardio$sexo)

##a.	Describa la distribución univariada de las variables presente en el conjunto de datos.  ¿Se evidencian outliers en alguna de ellas?
plot_num(cardio)

##b. Calcule e interprete la matriz de correlaciones.

cardio_cont <- select(cardio,imc, perimetro_abdo, hto, glicemia, ct, hdl, tgd)
  

chart.Correlation(cardio_cont, histogram = F, pch = 19)


