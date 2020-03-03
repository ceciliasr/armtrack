setwd("c:/TFM/Data")
load("BDistrophyD.rda")

se<-seq(0,nrow(BMD),10)
BMD1<-BMD[rownames(BMD) %in% se,]
rm(se)

data<-BMD1[,2:18]

#hc<-hclust(d = dist(data, method = "euclidea"), method = "ward.D2")
#plot(hc, hang = -0.01, cex = 0.7,main="BMD")
#rect.hclust(hc, k=6, border="blue")

pca<-prcomp(data, center = TRUE)
summary(pca)
rm(data)

#Creamos varios data frames que guardaremos como output del ACP:
pca_x<-data.frame(pca$x)


#Creamos las 5 variables correspondientes a los 5 componentes. 
post_acp<-pca_x[,1:5]

#k-means
t<-proc.time()
max<-c(0,0,0,0,0,0,0,0,0,0)
for(j in 5:5)
{
  for (i in 1:1000)
  {
    clus<-kmeans(post_acp,centers=j)
    if (((clus$betweenss/clus$totss)==max[j])=="TRUE")
    {
      clus_bueno <- clus
      max[j] <- clus$betweenss/clus$totss
      }   
  }
}

proc.time()-t

rm(max,i,t,clus)

#GUARDA EL CLUSTER BUENO
setwd("s:/AEDEMO_2020/Segmentaciones/Como/Codigos_sin_covisionado/output2")
save(clus_bueno, file="20200110-AEDEMO_output_kmeans.rda")

#CREACIÃN FICHERO OUTPUTS DEL KMEANS:
clus_output<-data.frame(clus_bueno$size, clus_bueno$withinss, c(rep(clus_bueno$totss)), c(rep(clus_bueno$betweenss)) )
names(clus_output)<-c("tamaÃ±o_cluster", "suma_cuadrados_intra_clusters", "suma_total_cuadrado", "suma_cuadrados_entre_clusters")
save(clus_output, file="20200110-AEDEMO_output_sum_kmeans.rda")
write.table(clus_output, file="20200110-AEDEMO_output_sum_kmeans.txt", row.names=TRUE, quote=FALSE, sep="\t")

dades2<-cbind(como_rel, data.frame(clus_bueno$cluster))
names(dades2)[11]<-"clusters"

rm(como_rel,clus_bueno,post_acp,post_acp_con_audi,clus_output)

#Cargamos demogrÃ¡ficos
load('s:/AEDEMO_2020/Segmentaciones/Generico/Ficheros/dem_panel_seleccion.rda')
como_dem<-como_dem[,c(1,10)]
dades2<-merge(dades2,como_dem,by='audindi')
dades2<-merge(dades2,dem,by='audindi')
rm(dem,como_dem)
dades2$Total<-dades2$Total/60/31

setwd("s:/AEDEMO_2020/Segmentaciones/Como/Codigos_sin_covisionado/output2")
save(dades2,file='20200110-AEDEMO_bbdd_completa_para_promedios_como_sin_covisionado.rda')
write.table(dades2,file='20200110-AEDEMO_bbdd_completa_para_promedios_como_sin_covisionado.txt', quote=F, sep="\t", row.names=FALSE)

