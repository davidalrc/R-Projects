# Para este análisis se usarán los paquetes neuralnet, caret y tidyverse 
# tidyverse ya se encuentran instalado 

install.packages(c("neuralnet","caret"))
library(tidyverse)
library(neuralnet)
library(caret)
library(datasets)

data("diamonds")
head(diamonds)

df<-diamonds %>% # "%>%"  Operador pipe sirve para concatenar múltiples operaciones dplyr 
    filter(cut %in% c("Ideal","Good") )  # %in%  identifica si un elemento pertenece a un vector
#Por medio de la operación anterior se esta filtrando seccionando la base principal solo 
# en aquellos que pertenezcan a las categorias "Ideal" y "Good" presentes en la columna cut
head(df)  # Se revisa la tabla resultante  del filtrado anterior  

df<- sample_n(df,5000)  #Se toma una muestra del conjunto principal de diamantes
# Se lleva a cabo el muestreo con el fin de que el algoritmo tenga un procesamiento 
# mucho más rápido. 
dim(df) # se procede a consular la dimensión del data frame muestreado 

df$binary<-ifelse(df$cut == "Ideal",1,0) #Se crea una nueva colummna llamada binary 
head(df)                                 #Que toma el valor 1 si cut es"ideal" y 0 si no lo es 
                                         # Esto se hace dado que se trabajará con un algoritmo 
                                         # de clasificación 

#A continuación se convertirá la colummna binary en un "factor"  

df$binary<- as.factor(df$binary)
names(df)
df<- df[,-2] #Se elimina la colummna "Color" que no es necesaria para el análisis 
head(df)

# Se procede a crear una partición de los datos para poder entrenar el modelo  
# se crea una partición de entrenamiento y otra de prueba  

rows<- createDataPartition(df$binary,p=0.7,list=FALSE, times=1)
train<- df[rows,] # el número de filas resultantes debe ser equivalente al 70% de la muestra de df 
head(train)
dim(train)
test<- df[-rows,] #El número de filas resultantes debe ser equivalente al 30% de la muestra de df 

control<-trainControl(method="repeatedcv",number=2,repeats = 2)  

model<- train(binary~.,data=train,method="ranger",trControl=control)
model #Se corre y entrena el modelo de  "random forest 

pred<- predict(model,test) # Se generan predicciones a partir de la muestra de prueba 
confusionMatrix(pred,test$binary) #se genera la matriz de confusión para conocer las metricas 
# del modelo y su nivel de acierto para predecir si un diamante es tipo "Ideal" o  "Good"  




