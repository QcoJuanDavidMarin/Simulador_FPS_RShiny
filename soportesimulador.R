bases <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                  dec = ".", stringsAsFactors = F)
row.names(bases)<-bases$X
bases$X<-NULL

bases <- data.frame(scale(bases)) # Escalar los datos 

###--- ACP -----------------------------------------------------------

acp<- prcomp(bases, center = TRUE, scale = TRUE) # Scale = T toma la matrz de correlacion, False es la matriz de covarianzas 
print(acp)
summary(apc) 
plot(acp)
plot(acp, type = "l")
biplot(acp, col = c('gray', 'red'))
head(acp$x)
acp$rotation

library(factoextra)
library(FactoMineR)
library(factorEx)

# Contributions of variables to PC1
fviz_contrib(acp, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(acp, choice = "var", axes = 2, top = 10)

### Grafico de los CP 
fviz_pca_var(acp, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, geom = c("point", "text") ) # Avoid text overlapping
fviz_pca_ind(acp, col.ind = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping

fviz_pca_biplot(acp, col.var = "contrib", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE, geom.ind = c("point", "text"), 
                geom.var = c("point", "text"), col.ind= "contrib")


###---para instalar ggbiplot en R -------------
# install.packages('digest', repos='http://cran.us.r-project.org')
# devtools::install_github("ropensci/plotly")
# devtools::install_github("vqv/ggbiplot", force = TRUE)
# install.packages("remotes")
# remotes::install_github("vqv/ggbiplot")
# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# library(devtools)
# remotes::install_github("vqv/ggbiplot")

bases <- read.csv("bases2.csv", header = TRUE, sep = ";", dec = ".")
row.names(bases)<-bases$X
bases$X<-NULL
acp<- prcomp(bases, center = TRUE, scale = TRUE)
library(ggbiplot)
library(ggplot2)
u <- data.frame(acp$x[,1],acp$x[,2],acp$x[,3])
acp$x
ggplot(acp)
summary(acp)
acp$rotation
fviz_screeplot(acp, addlebels = TRUE, ylim = c(0,25))


###--- CLUSTERS

##bases <- scale(bases) ## si se quiere hacer, en este ejercicio no se hizo
set.seed(123)
km <- kmeans(bases,7,nstart = 25, iter.max = 100)
bases$cluster <- km$cluster ## para agregar en una columna el cluster al que pertenece la muestra
km
####----------validacion del cluster ___________________--
library(factoextra)
library(factorEx)
library(FactoMineR)
library(cluster)
library(fpc)
library(NbClust)
bases <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                  dec = ".", stringsAsFactors = F)

row.names(bases)<-bases$X
bases$X<-NULL
dataesca <- data.frame(scale(bases))

set.seed(123)
km <- kmeans(bases,7,nstart = 25, iter.max = 100)

#Calcular la matriz de distancia
mdistancia <- get_dist(bases, method = 'euclidean')
fviz_dist(mdistancia, gradient = list(low = 'blue', mid = 'white', high = 'red'))

# Estimar el numero de clusters
fviz_nbclust(bases, kmeans, method = 'wss')
fviz_nbclust(bases, kmeans, method = 'silhouette') # Método de la silueta
  fviz_nbclust(dataesca, kmeans, method = 'gap_stat')

nb <-NbClust(bases, distance = 'euclidean',
             min.nc = 2, max.nc = 12,
             method = "kmeans", index = 'alllong')
### cON EL METODO DE WSS Y SILHOUTTE obtengo 7 clusters

set.seed(123)
km <- kmeans(dataesca,centers =7,nstart = 25, iter.max = 100)
km <- kmeans(bases,centers =7,nstart = 25, iter.max = 100)
sil.km <- silhouette(km$cluster, dist(bases))
sil.summari <- summary(sil.km)
sil.summari
fviz_silhouette(sil.km)

## Evaluación de la separación 
library(clValid)
dd <- dist(bases, method = 'euclidean')
km_stats <- cluster.stats(dd, km$cluster)
km_stats$within.cluster.ss
km_stats$clus.avg.silwidths
km_stats$dunn ## Este valor es el que compara las separaciones, entre mas lejos de cero mejor
# Con la libreria cValid se confirma lo obtenido en la linea de arriba
library(clValid)
dunn(clusters = km$cluster, Data = bases)
win.graph()
fviz_cluster(km, data = bases, repel = T, star.plot =T) 
win.graph()
fviz_cluster(km, data = bases, repel = T, star.plot =T, ellipse.type = 'euclid') 
win.graph()
fviz_cluster(km, data = bases, repel = T, star.plot =T, ellipse.type = 'norm') 

############ Visualizando como dendogramas 
datos <- bases
mat_distancia <- dist(datos, method = "euclidean")
hc_average <- hclust(d = mat_distancia, method = "ward.D")
# hc_average <- hclust(d = mat_distancia, method = "average")

### Con hc_average en method = "ward.D"  se obtiene un dendograma igual a de K-MEANS
##library("igraph")
win.graph()
set.seed(123)
fviz_dend(x= hc_average,
          k=7,
          k_colors = rainbow(7,alpha = 1),
          color_labels_by_k = TRUE,
          cex = 0.7,
          type = "phylogenic",
          rapel = TRUE, main = "Agrupamiento bases MQ")

data.frame(sort(km$cluster))


#### Comparando con el metodo PAM K Partitioning Around Medoids-------------------------------------------
#------------------------------------------------
set.seed(123)
kmpan <- pam(bases,7, metric = 'euclidean')
kmed_stad <- cluster.stats(dd, kmpan$clustering)
kmed_stad$dunn

sil.kmPAM <- silhouette(kmpan$cluster, dist(bases))
sil.summariPAM <- summary(sil.km)
sil.summariPAM
fviz_silhouette(sil.kmPAM)

#Graficos
fviz_cluster(km, data = bases)
fviz_cluster(kmpan, data = bases)

res.com <- cluster.stats(dd, km$cluster, kmpan$clustering)
res.com$corrected.rand
res.com$vi

hc_averagePAM <- hclust(d = mat_distancia, method = "mcquitty")
win.graph()
set.seed(123)
fviz_dend(x= hc_averagePAM, 
          k=7,
          k_colors = rainbow(7,alpha = 1),
          color_labels_by_k = TRUE,
          cex = 0.7,
          type = "phylogenic",
          rapel = TRUE, main = "Agrupamiento bases MQ")
data.frame(sort(kmpan$clustering))

############----------------graficos 3D de los clusters--------------------
set.seed(123)
plot_ly(x =acp$x[,1], y = acp$x[,2], z= acp$x[,3], text = rownames(bases),
        type = "scatter3d",mode='markers', color = ~km$cluster,
        colors = rainbow(12), frame = ~km$cluster)

#Clusters con kmpan  
plot_ly(x =~acp$x[,1], y = ~acp$x[,2], z= ~acp$x[,3],
        text = ~paste('Nombre:',rownames(bases), '<br>Cluster:',kmpan$cluster),
        type = "scatter3d",mode='markers', name = ~kmpan$cluster) %>% 
  layout(title = 'Agrupamiento por PAM') %>% 
  layout(scene = list(xaxis = list(title = 'CP1'),
                      yaxis = list(title = 'CP2'),
                      zaxis = list(title = 'CP3')))
# Clusters con km               
plot_ly(x =~acp$x[,1], y = ~acp$x[,2], z= ~acp$x[,3],
        text = ~paste('Nombre:',rownames(bases), '<br>Cluster:',km$cluster),
        type = "scatter3d",mode='markers', name = ~km$cluster) %>% 
  layout(title = 'Agrupamiento por K-Meas') %>% 
  layout(scene = list(xaxis = list(title = 'CP1'),
                      yaxis = list(title = 'CP2'),
                      zaxis = list(title = 'CP3')))

clustersKM <- data.frame(km$cluster)



##### ACP con GGBIPLOT#####################################################
basesclusters    
acp$x[[1]]
acpplot <- prcomp(bases, scale = T)   
biplot(acpplot, scale = 0)
singleplot <- ggbiplot(pcobj = acpplot, scale =0)
singleplot
ggplotly(singleplot)
#Add color & size ##________________________
acpplot <- prcomp(bases, scale = T)
p <- ggbiplot(acpplot, obs.scale  = 2,var.scale = 1,
              alpha = 0, varname.size = 2,var.axes=T, 
              color = 'gray',ellipse=T,
              ellipse.prob = 0.95, text =km$cluster)+
  
  geom_point(aes(color = rownames(bases)
  ))

ggplotly(p,dynamicTicks = TRUE) 


########### Heat Map interactivos 
library(d3heatmap)        


# if (!require("devtools")) install.packages("devtools")
# devtools::install_github("rstudio/d3heatmap")
# 
# install.packages("remotes")
# remotes::install_github("rstudio/d3heatmap", force = TRUE)

# https://rdrr.io/cran/d3heatmap/man/d3heatmapOutput.html  PARA SHINY
dataesca <- data.frame(scale(bases))
d3heatmap(x = dataesca, k_row = 7, k_col = 7, scale = "column", colors = "Blues",
          dendrogram = 'both',
          distfun = function(x){dist(x, method = "euclidean")},
          hclustfun = function(x){hclust(x, method = "average")},
          cexRow = 0.7, cexCol = 0.5, main = "Impacto Mps en bases", theme = "dark",
          brush_color = "#RRGGBB")


bases <- read.csv("bases2.csv", header = TRUE, sep = ";", dec = ".")############################################################

row.names(bases)<-bases$X
bases$X<-NULL

dist_bases <- as.matrix(dist(bases), method = "euclidean")
dist_bases2 <- cor(bases, method = "pearson")

# Correlacion entre variables MPs
d3heatmap(dist_bases2, scale = "none", 
          clustering_distance_rows = "euclidean",
          clustering_distance_cols = "euclidean",
          clustering_method = "average",cutree_rows = 14,
          fontsize = 9)
#Correlacion entre productos 
d3heatmap(dist_bases, scale = "none", 
          clustering_distance_rows = "euclidean",
          clustering_distance_cols = "euclidean",
          clustering_method = "average",cutree_rows = 14,
          fontsize = 9)





















##################CONVIRTIRNDO LA MATRIZ DE LOS CLUSTERS PARA REPRESENTARLOS EN UN GRAFICO

bases <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                  dec = ".", stringsAsFactors = F)

bases2 <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                  dec = ".", stringsAsFactors = F)

row.names(bases)<-bases$X
bases$X<-NULL
# bases <- data.frame(scale(bases))
set.seed(123)
km <- kmeans(bases,7,nstart = 25, iter.max = 100)
bases2$clus <- as.factor(km$cluster)


##Cambiar la matriz a la diagonal 
bases$clus <- factor(bases2$clus)
bases_long <- gather(bases2, materia_prima, cantidad, X10000196:X10002890, factor_key = T)
# bases_long <- bases_long %>% 
#                    arrange(bases_long$clus)

win.graph()
p<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                           colour = clus, label = bases_long$X))+
  stat_summary(fun = mean, geom = 'pointrange', size =1)+
  stat_summary(geom = 'line')+
  geom_point()+
  # geom_point(aes(x = X,group = clus))+
  theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
        axis.text.x = element_text(angle = 90, hjust = 1,size = 8))+
  labs(title = 'Representación de los grupos',
       subtitle = 'Materias primas en los clusters',
       x = '', y = '% Materia prima')


last(bases2)

ggplotly(p) 
  
data.frame(sort(km$cluster))

b<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                       colour = clus, label = bases_long$X))+
  geom_point(aes(x = X,group = clus))+
  theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  labs(title = 'Representación de los grupos',
       subtitle = 'Materias primas en los clusters',
       x = '', y = '% Materia prima')
ggplotly(b)
#Facet_wrap No servio 


library(gridExtra)
pb <- grid.arrange(p,b)  
ggplotly(pb) 










################################# analisis de los clusters con PAM

bases <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                  dec = ".", stringsAsFactors = F)
bases2 <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                   dec = ".", stringsAsFactors = F)

row.names(bases)<-bases$X
bases$X<-NULL

set.seed(123)
kmpan <- pam(bases,7, metric = 'euclidean')

bases$clus <- as.factor(kmpan$clustering)
bases2$clus <- as.factor(kmpan$clustering)
bases_long <- gather(bases, materia_prima, cantidad, X10000196:X10002890, factor_key = T)
bases_long <- gather(bases2, materia_prima, cantidad, X10000196:X10002890, factor_key = T)

pp<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                           colour = clus, label = bases_long$X))+
  stat_summary(fun = mean, geom = 'pointrange', size =1)+
  stat_summary(geom = 'line')+
  geom_point()+
  # geom_point(aes(x = X,group = clus))+
  theme(text = element_text(size = 14), legend.position = 'bottom', 
        axis.text.x = element_text(angle = 90, hjust = 1,size = 6))+
  labs(title = 'Representación de los grupos',
       subtitle = 'Materias primas en los clusters',
       x = '', y = '% Materia prima')

ggplotly(pp) 


bb<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                           colour = clus, label = bases_long$X))+
  geom_point(aes(x = X,group = clus))+
  theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
  labs(title = 'Representación de los grupos',
       subtitle = 'Materias primas en los clusters',
       x = '', y = '% Materia prima')
ggplotly(bb)








bases <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                  dec = ".", stringsAsFactors = F)

bases22 <- read.csv("bases2.csv", header = TRUE, sep = ";", 
                   dec = ".", stringsAsFactors = F)

bases <- data_final()
row.names(bases)<-bases$X
bases$X<-NULL

set.seed(123)
kmpan <- pam(bases,7, metric = 'euclidean')
set.seed(123)
km <- kmeans(bases,7,nstart = 25, iter.max = 100)
bases$clus <- as.factor(km$cluster)
bases22$clus <- as.factor(kmpan$clustering)
##Cambiar la matriz a la diagonal para PAM
bases22$clus <- factor(bases22$clus)
bases_long2 <- gather(bases22, materia_prima, cantidad, X10000196:X10002890, factor_key = T)
##Cambiar la matriz a la diagonal para KMEAN
bases$clus <- factor(bases$clus)
bases_long <- gather(bases, materia_prima, cantidad, X10000196:X10002890, factor_key = T)

bb<- ggplot(bases_long2, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                             colour = clus, label = bases_long2$materia_prima))+
  geom_point(aes(x = X,group = clus))+
  theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
  labs(title = 'Representación de los grupos',
       subtitle = 'Materias primas en los clusters',
       x = '', y = '% Materia prima')
ggplotly(bb) 

b<- ggplot(bases_long, aes(as.factor(x=materia_prima), y = cantidad, group = clus, 
                           colour = clus, label = bases_long$X))+
  geom_point(aes(x = X,group = clus))+
  theme(text = element_text(size = 14), legend.position = 'bottom', # Cambiar tamaño del texto y la posición de la leyenda
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))+
  labs(title = 'Representación de los grupos',
       subtitle = 'Materias primas en los clusters',
       x = '', y = '% Materia prima')
ggplotly(b)





#################### Costos #################---------------------##################

rownames()



nombres <- c('EMH', 'ZnO', 'In-viv', 'Costo_EMH', 'Costo_ZnO', 'Costo_total')





nombres <- c('EMH', 'ZnO', 'In-vivo', 'Costo_EMH', 'Costo_ZnO', 'Costo_total')

ESK_PRO_INSTAN_FPS25 <- c(6.5,3.6,29,2681,3198,5879)
BB_CRAM_FPS30 <- c(7,9,32,2887,7996,10883)
LB_BASE_GOTERO_FPS25 <- c(7,7.2,29,2887,6397,9284)
LB_CLARITE_ANTIMANC_FPS30 <- c(7.5,7.84,37,3093,6965,10059)
MOD1bases <- data.frame(nombres,ESK_PRO_INSTAN_FPS25,BB_CRAM_FPS30 ,
                        LB_BASE_GOTERO_FPS25,LB_CLARITE_ANTIMANC_FPS30)
row.names(MOD1bases) <- MOD1bases$nombres
MOD1bases$nombres <- NULL


COSTxKgEMH <- 41242
COSTxKgZnO <- 88845



# Calcular el costo del simulador 
entradassimulador <- data.frame(EMH = 6, ZnO = 6)
E<- (entradassimulador[1,][1])/100*COSTxKgEMH
Zn <-(entradassimulador[1,][2])/100*COSTxKgZnO 
COSTOSIMULADOR <- data.frame(E,Zn)
COSTOSIMULADOR<-data.frame(t(COSTOSIMULADOR))
COSTOSIMULADOR <- COSTOSIMULADOR %>% 
  summarise_each(funs(
    TOTAL = sum(.,na.rm = T)
  ))
TABLACOSTOS <- data.frame(Costo_EMH = E, 
                          CostoZnO_ZnO = Zn, 
                          Costo_total_xKg = COSTOSIMULADOR)




