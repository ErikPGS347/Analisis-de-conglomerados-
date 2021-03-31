### Ejercicio práctico 3 : Análisis de cluster

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).

resnumclust<-NbClust(variables, distance = "euclidean", min.nc=2, max.nc=5, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)


data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data <- scale(my_data)
# View the firt 3 rows
head(my_data, n = 3)


### librería a utilizar
library(cluster)
library(factoextra)

## Matriz de distancias
res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## Determinar el número óptimo de clusters

fviz_nbclust(my_data, kmeans, method = "gap_stat")


## Cluster K medias
km.res <- kmeans(my_data, 3, nstart = 25)
fviz_cluster(km.res, data = my_data, frame.type = "convex")


## cluster PAM : partición alrededor de los medoids. Alternativa robusta a la agrupación de k-means, menos sensible a los valores atípicos.

library("cluster")
pam.res <- pam(my_data, 4)
# Visualize
fviz_cluster(pam.res)

## Cluster jerárquico

clarax <- clara(my_data, 3)
# Cluster plot
fviz_cluster(clarax, stand = T, geom = "point",
             pointsize = 1)

plot(silhouette(clarax),  col = 2:3, main = "Silhouette plot")  


fviz_cluster(clarax) # gráfico de cluster

fviz_silhouette(clarax)

## preparando datos para análisis jerárquico
# 1. Loading and preparing data
data("USArrests")
my_data <- scale(USArrests)
# 2. Compute dissimilarity matrix
d <- dist(my_data, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 4, border = 2:5) # add rectangle

# mismo gráfico pero más elegante
library("factoextra")
# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(USArrests, k = 4, stand = TRUE)
# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))

