# Cargamos las librerías necesarias

install.packages("readr")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("ggplot2")
install.packages("remotes")
install.packages("caTools")
install.packages("class")
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)
library(remotes)
library(caTools)
library(class)
library(tidyverse)
library(cluster)
library(factoextra)

remotes::install_github("cran/DMwR")
library(DMwR)

# Cargamos los datos
datos <- read.csv("mallCustomers.csv")

# Verificamos la estructura y los tipos de los datos
str(datos)

# Resumen de los datos
summary(datos)

# * Preprocesamiento de datos

# Verificamos si hay valores faltantes
sum(is.na(datos))

# * Eliminación de outliers

# Creamos un boxplot de la variable Age
boxplot(datos$Age, main = "Age")

# Creamos un boxplot de la variable AnnualIncome
boxplot(datos$Annual.Income, main = "AnnualIncome")

# Creamos un boxplot de la variable SpendingScore
boxplot(datos$Spending.Score, main = "SpendingScore")

# No se encuentra ningún outlier en las variables	

# * Visualización y comprensión de variables

# Creamos un histograma de la variable Age
ggplot(datos, aes(x = Age)) + geom_histogram(binwidth = 5, fill = "black", color = "white") + labs(title = "Distribución de la edad", x = "Edad", y = "Frecuencia")

# Creamos un histograma de la variable AnnualIncome
ggplot(datos, aes(x = Annual.Income)) + geom_histogram(binwidth = 5, fill = "black", color = "white") + labs(title = "Distribución del ingreso anual", x = "Ingreso anual", y = "Frecuencia")

# Creamos un histograma de la variable SpendingScore
ggplot(datos, aes(x = Spending.Score)) + geom_histogram(binwidth = 5, fill = "black", color = "white") + labs(title = "Distribución del puntaje de gasto", x = "Puntaje de gasto", y = "Frecuencia")

# Convertimos las variables categóricas a factores
datos$Genre <- as.factor(datos$Genre)

# Verificamos la estructura actualizada de los datos
str(datos)

# Eliminamos la columna CustomerID
datos_sin_id <- datos[, -which(names(datos) == "CustomerID")]

# * Aplicamos el algoritmo de agrupamiento K-means

set.seed(123)  # Para reproducibilidad
kmeans_model <- kmeans(datos_sin_id[, c("Age", "Annual.Income", "Spending.Score")], centers = 5)

# Visualizar los centros de los grupos
kmeans_model$centers

# Añadir el grupo asignado a cada cliente en los datos
datos_sin_id$cluster <- as.factor(kmeans_model$cluster)

# b) Construir dendrogramas jerárquicos
# Método single
dend_single <- hclust(dist(datos_sin_id[, c("Age", "Annual.Income", "Spending.Score")]), method = "single")
# Método centroid
dend_centroid <- hclust(dist(datos_sin_id[, c("Age", "Annual.Income", "Spending.Score")]), method = "centroid")
# Método ward
dend_ward <- hclust(dist(datos_sin_id[, c("Age", "Annual.Income", "Spending.Score")]), method = "ward.D2")

# Comparar los dendrogramas
fviz_dend(list("Single" = dend_single, "Centroid" = dend_centroid, "Ward" = dend_ward), k = 5)

# c) Repetir las tareas por género
# Separar datos por género
male_customers <- datos_sin_id %>% filter(Gender == "Male")
female_customers <- datos_sin_id %>% filter(Gender == "Female")

# Aplicar k-means por género
kmeans_male <- kmeans(male_customers[, c("Age", "Annual.Income", "Spending.Score")], centers = 5)
kmeans_female <- kmeans(female_customers[, c("Age", "Annual.Income", "Spending.Score")], centers = 5)

# Añadir el grupo asignado a cada cliente en los datos separados por género
male_customers$cluster <- as.factor(kmeans_male$cluster)
female_customers$cluster <- as.factor(kmeans_female$cluster)

# Comparar los grupos encontrados por género
summary(male_customers$cluster)
summary(female_customers$cluster)
