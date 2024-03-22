# Cargar la librería necesaria
library(cluster)
library(ggplot2)

# Cargar los datos
mall_customers <- read.csv("mallCustomers.csv")

# Verificar la estructura de los datos
str(mall_customers)

# Explorar las primeras filas de los datos
head(mall_customers)

# Aplicar el algoritmo k-means
# Primero, determinar el valor óptimo de k usando el método del codo

set.seed(123) # Establecer una semilla para reproducibilidad

wcss <- vector() # Vector para almacenar la suma de los cuadrados dentro del clúster

# Calcular WCSS para diferentes valores de k
for (i in 1:10) {
  kmeans_model <- kmeans(mall_customers[, c("Age", "Annual.Income", "Spending.Score")], centers = i, nstart = 10)
  wcss[i] <- kmeans_model$tot.withinss
}

# Graficar el método del codo para seleccionar el valor óptimo de k
plot(1:10, wcss, type = 'b', main = "Método del Codo", xlab = "Número de Clústeres (k)", ylab = "WCSS")

# A partir de la gráfica, seleccionar el valor óptimo de k
# Supongamos que el valor óptimo de k es 5

# Aplicar k-means con k óptimo
kmeans_model <- kmeans(mall_customers[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)

# Agregar las etiquetas de los clústeres a los datos originales
mall_customers$cluster <- kmeans_model$cluster

# Visualizar los resultados del agrupamiento
ggplot(mall_customers, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_model$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes utilizando K-Medias", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()

# Obtener los centroides del modelo kmeans
centroids <- kmeans_model$centers

# Calcular la distancia euclidiana entre cada punto y su centroide correspondiente
distances_to_centroid <- as.matrix(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")], method = "euclidean"))

# Calcular la suma total de cuadrados dentro del clúster
withinss <- rep(0, 5)  # 5 es el número de clústeres
for (i in 1:5) {
  withinss[i] <- sum((distances_to_centroid[, i])^2)
}

# Calcular la suma total de cuadrados dentro de los clústeres
tot_withinss <- sum(withinss)

# Calcular la suma total de cuadrados
totss <- sum((distances_to_centroid)^2)

# Calcular la suma de cuadrados entre de los clústeres
betweenss <- totss - tot_withinss

# Calcular la información retenida
information <- betweenss / totss


# Imprimir los resultados
cat("Suma total de cuadrados dentro del clúster (tot_withinss):", tot_withinss, "\n")
cat("Suma total de cuadrados (totss):", totss, "\n")
cat("Suma de cuadrados entre de los clústeres (betweenss):", betweenss, "\n")
cat("Información retenida:", information, "\n")

# * Construir dendrogramas jerárquicos

# Realizar agrupamientos jerárquicos con diferentes métodos
dendrogram_single <- hclust(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")]), method = "single")
dendrogram_centroid <- hclust(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")]), method = "centroid")
dendrogram_ward <- hclust(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")]), method = "ward.D")

# Visualizar los dendrogramas
par(mfrow = c(1, 3))  # Organizar las gráficas en una fila
plot(dendrogram_single, main = "Método Single", xlab = "", ylab = "Distancia")
plot(dendrogram_centroid, main = "Método Centroid", xlab = "", ylab = "Distancia")
plot(dendrogram_ward, main = "Método Ward", xlab = "", ylab = "Distancia")

# * Repetir las tareas por género

# Cargar los datos
mall_customers <- read.csv("mallCustomers.csv")

# Separar los datos por género
mall_customers_male <- subset(mall_customers, Genre == "Male")
mall_customers_female <- subset(mall_customers, Genre == "Female")

# Aplicar el algoritmo K-Medias para clientes masculinos
kmeans_male <- kmeans(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)
mall_customers_male$cluster <- kmeans_male$cluster

# Aplicar el algoritmo K-Medias para clientes femeninos
kmeans_female <- kmeans(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)
mall_customers_female$cluster <- kmeans_female$cluster

# Visualizar los resultados del agrupamiento para clientes masculinos
ggplot(mall_customers_male, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_male$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes masculinos utilizando K-Medias", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()

# Visualizar los resultados del agrupamiento para clientes femeninos
ggplot(mall_customers_female, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_female$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes femeninos utilizando K-Medias", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()


# Realizar agrupamientos jerárquicos para cada género
dendrogram_male_single <- hclust(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), method = "single")
dendrogram_male_centroid <- hclust(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), method = "centroid")
dendrogram_male_ward <- hclust(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), method = "ward.D")

dendrogram_female_single <- hclust(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), method = "single")
dendrogram_female_centroid <- hclust(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), method = "centroid")
dendrogram_female_ward <- hclust(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), method = "ward.D")

# Comparar los dendrogramas por género
par(mfrow = c(2, 3))  # Organizar las gráficas en una matriz 2x3

# Dendrogramas para clientes masculinos
plot(dendrogram_male_single, main = "Single Linkage", xlab = "", ylab = "Distance")
plot(dendrogram_male_centroid, main = "Centroid", xlab = "", ylab = "Distance")
plot(dendrogram_male_ward, main = "Ward's Method", xlab = "", ylab = "Distance")

# Dendrogramas para clientes femeninos
plot(dendrogram_female_single, main = "Single Linkage", xlab = "", ylab = "Distance")
plot(dendrogram_female_centroid, main = "Centroid", xlab = "", ylab = "Distance")
plot(dendrogram_female_ward, main = "Ward's Method", xlab = "", ylab = "Distance")