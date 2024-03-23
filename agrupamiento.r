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
  labs(title = "Segmentación de clientes utilizando k-means", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
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

table(mall_customers$cluster)

prop.table(table(mall_customers$cluster))

cluster_stats <- aggregate(. ~ cluster, data = mall_customers[, c("Annual.Income", "Spending.Score", "cluster")], FUN = function(x) c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))
cluster_stats
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

# Calcular los coeficientes de correlación de Cophenetic
cophenetic_corr_single <- cophenetic(dendrogram_single)
cophenetic_corr_centroid <- cophenetic(dendrogram_centroid)
cophenetic_corr_ward <- cophenetic(dendrogram_ward)

cat("Coeficiente de correlación de Cophenetic para Single Linkage:", cor(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_single), "\n")
cat("Coeficiente de correlación de Cophenetic para Centroid Linkage:", cor(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_centroid), "\n")
cat("Coeficiente de correlación de Cophenetic para Ward Linkage:", cor(dist(mall_customers[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_ward), "\n")

# Cortar el dendrograma para obtener los grupos
num_grupos <- 5  # Especifica el número deseado de grupos
grupos <- cutree(dendrogram_centroid, k = num_grupos)

# Agregar la información de los grupos al dataframe original
mall_customers$grupo <- grupos

# Visualizar los grupos obtenidos
table(mall_customers$grupo)
prop.table(table(mall_customers$grupo))

# Visualizar los resultados del agrupamiento jerárquico
ggplot(mall_customers, aes(x = Annual.Income, y = Spending.Score, color = factor(grupo))) +
  geom_point() +
  labs(title = "Segmentación de clientes utilizando Agrupamiento Jerárquico", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()

# * Repetir las tareas por género

# Cargar los datos
mall_customers <- read.csv("mallCustomers.csv")

# Separar los datos por género
mall_customers_male <- subset(mall_customers, Genre == "Male")
mall_customers_female <- subset(mall_customers, Genre == "Female")

# Aplicar el algoritmo k-means para clientes masculinos
kmeans_male <- kmeans(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)
mall_customers_male$cluster <- kmeans_male$cluster

# Aplicar el algoritmo k-means para clientes femeninos
kmeans_female <- kmeans(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)
mall_customers_female$cluster <- kmeans_female$cluster

set.seed(123)
wcss_male <- vector()
for (i in 1:10) {
  kmeans_model_male <- kmeans(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")], centers = i, nstart = 10)
  wcss_male[i] <- kmeans_model_male$tot.withinss
}

plot(1:10, wcss_male, type = 'b', main = "Método del Codo", xlab = "Número de Clústeres (k)", ylab = "WCSS")

set.seed(123)
wcss_female <- vector()
for (i in 1:10) {
  kmeans_model_female <- kmeans(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")], centers = i, nstart = 10)
  wcss_female[i] <- kmeans_model_female$tot.withinss
}

plot(1:10, wcss_female, type = 'b', main = "Método del Codo", xlab = "Número de Clústeres (k)", ylab = "WCSS")

# Visualizar los resultados del agrupamiento para clientes masculinos
ggplot(mall_customers_male, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_male$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes masculinos utilizando k-means", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()

# Visualizar los resultados del agrupamiento para clientes femeninos
ggplot(mall_customers_female, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_female$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes femeninos utilizando k-means", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()


# Realizar agrupamientos jerárquicos para cada género
dendrogram_male_single <- hclust(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), method = "single")
dendrogram_male_centroid <- hclust(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), method = "centroid")
dendrogram_male_ward <- hclust(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), method = "ward.D")

dendrogram_female_single <- hclust(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), method = "single")
dendrogram_female_centroid <- hclust(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), method = "centroid")
dendrogram_female_ward <- hclust(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), method = "ward.D")

cophenetic_corr_single_male <- cophenetic(dendrogram_male_single)
cophenetic_corr_centroid_male <- cophenetic(dendrogram_male_centroid)
cophenetic_corr_ward_male <- cophenetic(dendrogram_male_ward)
cat("Coeficiente de correlación de Cophenetic para el método Single:", cor(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_single_male), "\n")
cat("Coeficiente de correlación de Cophenetic para el método Centroid:", cor(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_centroid_male), "\n")
cat("Coeficiente de correlación de Cophenetic para el método Ward:", cor(dist(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_ward_male), "\n")

cophenetic_corr_single_female <- cophenetic(dendrogram_female_single)
cophenetic_corr_centroid_female <- cophenetic(dendrogram_female_centroid)
cophenetic_corr_ward_female <- cophenetic(dendrogram_female_ward)
cat("Coeficiente de correlación de Cophenetic para el método Single:", cor(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_single_female), "\n")
cat("Coeficiente de correlación de Cophenetic para el método Centroid:", cor(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_centroid_female), "\n")
cat("Coeficiente de correlación de Cophenetic para el método Ward:", cor(dist(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")]), cophenetic_corr_ward_female), "\n")

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

# Cargar los datos
mall_customers <- read.csv("mallCustomers.csv")

# Separar los datos por género
mall_customers_male <- subset(mall_customers, Genre == "Male")
mall_customers_female <- subset(mall_customers, Genre == "Female")

# Aplicar el algoritmo k-means para clientes masculinos
kmeans_male <- kmeans(mall_customers_male[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)
mall_customers_male$cluster <- kmeans_male$cluster

# Aplicar el algoritmo k-means para clientes femeninos
kmeans_female <- kmeans(mall_customers_female[, c("Age", "Annual.Income", "Spending.Score")], centers = 5, nstart = 10)
mall_customers_female$cluster <- kmeans_female$cluster

# Visualizar los resultados del agrupamiento para clientes masculinos
ggplot(mall_customers_male, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_male$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes masculinos utilizando k-means", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()

# Visualizar los resultados del agrupamiento para clientes femeninos
ggplot(mall_customers_female, aes(x = Annual.Income, y = Spending.Score, color = factor(cluster))) +
  geom_point() +
  geom_point(data = kmeans_female$centers, aes(x = Annual.Income, y = Spending.Score), color = 'black', size = 3, shape = 16) +
  labs(title = "Segmentación de clientes femeninos utilizando k-means", x = "Ingresos Anuales", y = "Puntuación de Gasto") +
  theme_minimal()

table(mall_customers_male$cluster)
prop.table(table(mall_customers_male$cluster))

table(mall_customers_female$cluster)
prop.table(table(mall_customers_female$cluster))
