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
library(readr)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(ggplot2)
library(remotes)
library(caTools)
library(class)

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