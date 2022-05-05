library(FactoMineR)
library(tidyverse)
library(factoextra)
library(Rtsne)
library(microbenchmark)
library(gridExtra)
library(RColorBrewer)
library(grDevices)
library(ggpointdensity)
library(plotrix)

load("./Projet-6_tSNE/otu.Rdata")
Y <- as.data.frame(X)
Y.pca <- PCA(X, ncp=150)

#En commentaire pour eviter que ca se lance par accident car il prend beaucoup de temps
# t.p5.time <- microbenchmark(tsne.out.p5 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 5), times=1L)
# t.p10.time <- microbenchmark(tsne.out.p10 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 10), times=1L)
# t.p30.time <- microbenchmark(tsne.out.p30 <- Rtsne(Y.pca$ind$coord, pca=F), times=1L)
# t.p60.time <- microbenchmark(tsne.out.p60 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60), times=1L)
# t.p120.time <- microbenchmark(tsne.out.p120 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 120), times=1L)
# t.p300.time <- microbenchmark(tsne.out.p300 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 300), times=1L)
# 
# t.60.100.time <- microbenchmark(tsne.out.p60.iter100 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60, max_iter = 100), times=1L)
# t.60.500.time <- microbenchmark(tsne.out.p60.iter500 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60, max_iter = 500), times=1L)
# t.60.1000.time <- microbenchmark(tsne.out.p60.iter1000 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60, max_iter = 1000), times=1L)
# t.60.2000.time <- microbenchmark(tsne.out.p60.iter2000 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60, max_iter = 2000), times=1L)
# t.60.3000.time <- microbenchmark(tsne.out.p60.iter3000 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60, max_iter = 3000), times=1L)
# t.60.4000.time <- microbenchmark(tsne.out.p60.iter4000 <- Rtsne(Y.pca$ind$coord, pca=F, perplexity = 60, max_iter = 4000),times=1L)
#

# Mettre le temps qu'il prends chaque lancer de tsne dans un data.frame
time.data <- data.frame(iterations=c(100,500,1000,2000,3000,4000), time.in.seconds = c((t.60.100.time[2]/10^9)[[1]], (t.60.500.time[2]/10^9)[[1]], (t.60.1000.time[2]/10^9)[[1]],
                                                                                       (t.60.2000.time[2]/10^9)[[1]], (t.60.3000.time[2]/10^9)[[1]], (t.60.4000.time[2]/10^9)[[1]]))
time.date2<- data.frame(perplexity=c(5,10,30,60,120,300), time.in.seconds = c((t.p5.time[2]/10^9)[[1]], (t.p10.time[2]/10^9)[[1]], (t.p30.time[2]/10^9)[[1]],
                                                                                       (t.p60.time[2]/10^9)[[1]], (t.p120.time[2]/10^9)[[1]], (t.p300.time[2]/10^9)[[1]]))

# Changer tout en data.frame
df.p5.it1000 <- as.data.frame(tsne.out.p5$Y)
df.p10.it1000 <- as.data.frame(tsne.out.p10$Y)
df.p30.it1000 <- as.data.frame(tsne.out.p30$Y)
df.p60.it1000 <- as.data.frame(tsne.out.p60$Y)
df.p120.it1000 <- as.data.frame(tsne.out.p120$Y)
df.p300.it1000 <- as.data.frame(tsne.out.p300$Y)

df.p60.it100 <- as.data.frame(tsne.out.p60.iter100$Y)
df.p60.it500 <- as.data.frame(tsne.out.p60.iter500$Y)
df.p60.it1000 <- as.data.frame(tsne.out.p60.iter1000$Y)
df.p60.it2000 <- as.data.frame(tsne.out.p60.iter2000$Y)
df.p60.it3000 <- as.data.frame(tsne.out.p60.iter3000$Y)
df.p60.it4000 <- as.data.frame(tsne.out.p60.iter4000$Y)

df.pca <- as.data.frame(Y.pca$ind$coord[,1:2])

#Creation du fonction pour les plots
plottsne <- function(data, titre, palette) {
  ggplot(data, aes(x=V1, y=V2)) +
    geom_point(size=0.25) +
    guides(colour=guide_legend(override.aes=list(size=6))) +
    xlab("") + ylab("") +
    ggtitle(titre) +
    theme_light(base_size=5) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank()) +
    scale_colour_brewer(palette = palette)
}

#Plot des differents perplexite
grid.arrange(plottsne(df.p5.it1000, 'Tsne perplexite = 5, Max Iteration = 1000', 'Set2'),
             plottsne(df.p10.it1000, 'Tsne perplexite = 10, Max Iteration = 1000', 'Set2'),
             plottsne(df.p30.it1000, 'Tsne perplexite = 30, Max Iteration = 1000', 'Set2'),
             plottsne(df.p60.it1000, 'Tsne perplexite = 60, Max Iteration = 1000', 'Set2'),
             plottsne(df.p120.it1000, 'Tsne perplexite = 120, Max Iteration = 1000', 'Set2'),
             plottsne(df.p300.it1000, 'Tsne perplexite = 300, Max Iteration = 1000', 'Set2'),nrow=2)
#Plot des differents max iteration
grid.arrange(plottsne(df.p60.it100, 'Tsne perplexite = 60, Max Iteration = 100', 'Set2'),
             plottsne(df.p60.it500, 'Tsne perplexite = 60, Max Iteration = 500', 'Set2'),
             plottsne(df.p60.it1000, 'Tsne perplexite = 60, Max Iteration = 1000', 'Set2'),
             plottsne(df.p60.it2000, 'Tsne perplexite = 60, Max Iteration = 2000', 'Set2'),
             plottsne(df.p60.it3000, 'Tsne perplexite = 60, Max Iteration = 3000', 'Set2'),
             plottsne(df.p60.it4000, 'Tsne perplexite = 60, Max Iteration = 4000', 'Set2'), nrow=2)

dcols <- densCols(df.pca)

#Plot du Tsne vs. PCA densité     
grid.arrange(ggplot(df.p60.it3000, aes(x=V1, y=V2)) +
               geom_pointdensity(size=0.5) +
               guides(colour=guide_legend(override.aes=list(size=6))) +
               xlab("") + ylab("") +
               ggtitle('tsne') +
               theme_light(base_size=5) +
               theme(axis.text.x=element_blank(),
                     axis.text.y=element_blank())+
               scale_color_continuous(trans='reverse'), 
             ggplot(df.pca, aes(x=Dim.1, y=Dim.2)) +
               geom_pointdensity(size=0.5) +
               guides(colour=guide_legend(override.aes=list(size=6))) +
               xlab("") + ylab("") +
               ggtitle('pca') +
               theme_light(base_size=5) +
               theme(axis.text.x=element_blank(),
                     axis.text.y=element_blank())+
               scale_color_continuous(trans='reverse'), nrow=1)

### III-CLUSTERING

## keeping original data
tsne_kmeans=df.p60.it3000
pca_kmeans<-as.data.frame(Y.pca$ind$coord)
X_kmeans<-as.data.frame(X)

## Creating k-means clustering model on initial space, and assigning the result to the data 
k<-15
set.seed(1)
fit_cluster_kmeans=kmeans(scale(X_kmeans), k)  
X_kmeans$cl_kmeans = factor(fit_cluster_kmeans$cluster)


## Creating k-means clustering model on t-sne, and assigning the result to the data 
set.seed(1)
fit_cluster_kmeans=kmeans(scale(tsne_kmeans[,1:2]), k)  
tsne_kmeans$cl_kmeans = factor(fit_cluster_kmeans$cluster)
set.seed(3)
fit_cluster_kmeans=kmeans(scale(tsne_kmeans[,1:2]), k)  
tsne_kmeans$cl_kmeans_2 = factor(fit_cluster_kmeans$cluster)
tsne_kmeans$cl_X_kmeans=X_kmeans$cl_kmeans
## Adding tsne cluster color to pca coordinates
#d_pca_1_original$cl_kmeans = factor(fit_cluster_kmeans$cluster)

## Creating k-means clustering model on pca, and assigning the result to the data 
set.seed(1)
fit_cluster_kmeans=kmeans(scale(pca_kmeans), k)  
pca_kmeans$cl_kmeans = factor(fit_cluster_kmeans$cluster)
pca_kmeans$cl_X_kmeans=X_kmeans$cl_kmeans
set.seed(1)
fit_cluster_kmeans=kmeans(scale(pca_kmeans[,1:2]), k)  
pca_kmeans$cl_2_kmeans = factor(fit_cluster_kmeans$cluster)
set.seed(3)
fit_cluster_kmeans=kmeans(scale(pca_kmeans[,-c(dim(pca_kmeans)[2],dim(pca_kmeans)[2]-1,dim(pca_kmeans)[2]-2)]), k)  
pca_kmeans$cl_kmeans_2 = factor(fit_cluster_kmeans$cluster)

# Define the number of colors you want

mycolors <- c(brewer.pal(7, "Set1"),brewer.pal(8, "Set2"))

ploth=ggplot(pca_kmeans, aes_string(x="Dim.1", y="Dim.2", color="cl_X_kmeans")) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("PCA") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")

plotk=ggplot(tsne_kmeans, aes_string(x="V1", y="V2", color="cl_X_kmeans")) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")


## and finally: putting the plots side by side with gridExtra lib...
grid.arrange(plotk, ploth,  ncol=2)

plotv=ggplot(pca_kmeans, aes_string(x="Dim.1", y="Dim.2", color="cl_kmeans")) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("PCA, toutes dimensions, seed=1") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")

plotx=ggplot(pca_kmeans, aes_string(x="Dim.1", y="Dim.2", color="cl_2_kmeans")) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("PCA, 2 premières dimensions") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")

plotw=ggplot(tsne_kmeans, aes_string(x="V1", y="V2", color="cl_kmeans")) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE perplexity = 60, max_iter = 3000, seed=1") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")


ploty=ggplot(pca_kmeans, aes_string(x="Dim.1", y="Dim.2", color="cl_kmeans_2")) +  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("Dim1") + ylab("Dim2") +
  ggtitle("PCA, toutes dimensions, seed=3") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")

plotz=ggplot(tsne_kmeans, aes_string(x="V1", y="V2", color="cl_kmeans_2"))+  
  geom_point(size=0.25) +
  guides(colour=guide_legend(override.aes=list(size=1))) +
  xlab("") + ylab("") +
  ggtitle("t-SNE perplexity = 60, max_iter = 3000, seed=3") +
  theme_light(base_size=5) + scale_color_manual(values = mycolors)+
  theme(legend.position = "none")


grid.arrange(plotw, plotv, plotz, ploty,  ncol=2)

swap <- function(matrixRow,x,y){
  #x is diagonal index
  #y is max of the row
  indexY <- which(matrixRow == y)
  valX <- matrixRow[x]
  matrixRow[x] <- y
  matrixRow[indexY] <- valX
  return(matrixRow)
}

mat.order=function(mat){
  for(i in 1:nrow(mat)){
    rowI <- mat[i,]
    y <- max(rowI)
    mat[i,] <- swap(rowI, i, y)
  }
  mat
}

test<-data.frame(espace_initial=X_kmeans$cl_kmeans, PCA_toutes_dimensions=pca_kmeans$cl_kmeans, PCA_2_dimensions=pca_kmeans$cl_2_kmeans, tSNE=tsne_kmeans$cl_kmeans)
test1<-mat.order(table(test[,1:2]))
test1<-mat.order(t(test1))
test2<-mat.order(table(test[,c(1,3)]))
test2<-mat.order(t(test2))
test3<-mat.order(table(test[,c(1,4)]))
test3<-mat.order(t(test3))

mat.order(table(test[,1:2]))

par(mar = c(0.5, 6, 5.5, 0.5))
color2D.matplot(test3, show.values = F, axes=F,xlab = "t-SNE",main= "t-SNE",ylab = "espace initial",vcol = "black",extremes = c("white", "black"))
axis(3, at = seq_len(ncol(test3)) - 0.5,labels = unlist(dimnames(test3)[2]), tick = FALSE, cex.axis = 2)
axis(2, at = seq_len(nrow(test3)) -0.5,labels = rev(unlist(dimnames(test3)[1])),las = 1, tick = FALSE, cex.axis = 2)

color2D.matplot(test2, show.values = F, axes=F,xlab = "PCA, 2 dimensions",main= "PCA, 2 dimensions",ylab = "espace initial",vcol = "black",extremes = c("white", "black"))
axis(3, at = seq_len(ncol(test2)) - 0.5,labels = unlist(dimnames(test2)[2]), tick = FALSE, cex.axis = 2)
axis(2, at = seq_len(nrow(test2)) -0.5,labels = rev(unlist(dimnames(test2)[1])),las = 1, tick = FALSE, cex.axis = 2)

color2D.matplot(test1, show.values = F, axes=F,xlab = "PCA, toutes dimensions",main= "PCA, toutes dimensions",ylab = "espace initial",vcol = "black",extremes = c("white", "black"))
axis(3, at = seq_len(ncol(test1)) - 0.5,labels = unlist(dimnames(test1)[2]), tick = FALSE, cex.axis = 2)
axis(2, at = seq_len(nrow(test1)) -0.5,labels = rev(unlist(dimnames(test1)[1])),las = 1, tick = FALSE, cex.axis = 2)












 