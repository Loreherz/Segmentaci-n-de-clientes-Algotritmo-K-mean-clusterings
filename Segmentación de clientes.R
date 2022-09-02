
####### ANALISIS EXPLORATORIO DE LOS DATOS ###########

# El tipo de variables que contiene la base de datos 
str(Mall_Customers)

# El nombre de las variables
names(Mall_Customers)

# Las primeras seis filas de la base de datos
head(Mall_Customers)

# Resumen de la base de datos 
summary(Mall_Customers)
# Resumen de la variable edad 
summary(Mall_Customers$Age)

# Desviación estandar de la variable edad
sd(Mall_Customers$Age)

# Resumen de los ingresos mensuales 
summary(Mall_Customers$`Annual Income (k$)`)
# Desviación estandar de los ingresos mensuales 
sd(Mall_Customers$`Annual Income (k$)`)

# Desviación estandar de puntaje de gasto
sd(Mall_Customers$`Spending Score (1-100)`)

########## VISUALIZACIÓN DE GÉNERO DEL CLIENTE #########

# Gráfico de barras

a=table(Mall_Customers$Gender)
barplot(a,main="Using Barplot to display Gender comparision",
        ylab= "Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

# Gráfico de sector

pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct, " % ",sep=" " )
library(plotrix)
pie3D(a,labels=lbs,
      main="Pie chart depicting ratio of female and male")


# Visualización de la distribución por edades 
# por medio de un histograma 

summary(Mall_Customers$Age)

# gráfica de histograma

hist(Mall_Customers$Age,
     col="blue",
     main=" Histogram to show count of age class",
     xlab="Age class",
     ylab="Frecuency",
     labels=TRUE)

# Gráfica de Boxplot 
boxplot(Mall_Customers$Age,
        col="#ff0066",
        main="Boxplot for descriptive analisys of age")

######## VISUALIZACIÓN PARA ANALIZAR LOS INGRESOS ANUALES DE LOS CLIENTES ##########

summary(Mall_Customers$`Annual Income (k$)`)

# Histograma 
hist(Mall_Customers$`Annual Income (k$)`,
     col="#660033",
     main=" Histograma for annual income",
     xlab="Annual Income class",
     ylab="Frecuency",
     labels=TRUE)

plot(density(Mall_Customers$`Annual Income (k$)`),
     col="Yellow",
     main="Density plot for annual income",
     xlab="Annual Income class",
     ylab= "Density")
polygon(density(Mall_Customers$`Annual Income (k$)`),
        col="#ccff66")

##### ANALISIS DEL PUNTAJE DE GASTO ###########

summary(Mall_Customers$`Spending Score (1-100)`)

# Diagrama de cajas
boxplot(Mall_Customers$`Spending Score (1-100)`,
        horizontal = TRUE,
        col="#990000",
        main="Boxplot for descriptive analysis of spending score")

# Histograma 

hist(Mall_Customers$`Spending Score (1-100)`,
     main="Histogram for spending score",
     xlab="Spending score class",
     ylab="Frecuency",
     col = "#6600cc",
     labels=TRUE)

######### MÉTODO DE CODO (ELBOW)###########

library(purrr)
set.seed(123)
# Funcion para calcular el intra-cluster total suma de cuadrados

iss<-function(k){
  kmeans(Mall_Customers[ ,3:5],k,iter.max = 100,nstart = 100,algorithm = "Lloyd")$tot.withins
  
  
}

k.values<-1:10
iss_values<- map_dbl(k.values,iss)

plot(k.values,iss_values,
     type="b",pch=19,frame=FALSE,
     xlab="Number of clusters K",
     ylab="Total intras-Clusters sum of square")

####### Método de silueta promedio ########

library(cluster)
library(gridExtra)
library(grid)

k2<-kmeans(Mall_Customers[ ,3:5],2,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s2<-plot(silhouette(k2$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k3<-kmeans(Mall_Customers[ ,3:5],3,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s3<-plot(silhouette(k3$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k4<-kmeans(Mall_Customers[ ,3:5],4,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s4<-plot(silhouette(k4$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k5<-kmeans(Mall_Customers[ ,3:5],5,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s5<-plot(silhouette(k5$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k6<-kmeans(Mall_Customers[ ,3:5],6,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s6<-plot(silhouette(k6$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k7<-kmeans(Mall_Customers[ ,3:5],7,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s7<-plot(silhouette(k7$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k8<-kmeans(Mall_Customers[ ,3:5],8,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s8<-plot(silhouette(k8$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k9<-kmeans(Mall_Customers[ ,3:5],9,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s9<-plot(silhouette(k9$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

k10<-kmeans(Mall_Customers[ ,3:5],10,iter.max = 100,nstart = 50,algorithm = "Lloyd")
s10<-plot(silhouette(k10$cluster,dist(Mall_Customers[ ,3:5],"euclidean")))

library(NbClust)
library(factoextra)

fviz_nbclust(Mall_Customers[ ,3:5],kmeans,method = "silhouette")

# compute gap statistic

set.seed(125)
stat_gap <- clusGap(Mall_Customers[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

k6<-kmeans(Mall_Customers[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

##### VISUALIZACIÓN DE LOS RESULTADOS DE LA AGRUPACIÓN #######

pcclust=prcomp(Mall_Customers[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

set.seed(1)
ggplot(Mall_Customers, aes(x =`Annual Income (k$)`, y =`Spending Score (1-100)`)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")


ggplot(Mall_Customers, aes(x =`Spending Score (1-100)`, y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}
digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters
plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))

