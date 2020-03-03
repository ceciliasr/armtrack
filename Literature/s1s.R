############################################################
# Ejercicio
############################################################
# Aplicar la clusterizacion jerarquica al conjunto de datos de 
# wine2.txt para clasificarlos segun tipologia (variable Type)
# 1. Lee los datos
setwd('...')
wine <- read.table('wine2.txt',sep=',',header=TRUE)

# 2. Haz una descriptiva de los datos sin escalar
summary(wine)
pairs(wine)

# 3. Escala los datos y haz un heatmap
wine2 <- scale(wine)
heatmap(as.matrix(wine2))

# 4. Compara los 4 dendogramas resultantes de usar la distancia euclidea/manhattan y el metodo de wald y completo
# Que influye mas el tipo de distancia o el tipo de agrupacion?
d1 <- dist(wine2,method='euclidean')
d2 <- dist(wine2,method='manhattan')
hc1 <- hclust(d1,method = "ward.D2") ; cor(d1, cophenetic(hc1))    
hc2 <- hclust(d1,method = "complete") ; cor(d1, cophenetic(hc2))    
hc3 <- hclust(d2,method = "ward.D2") ; cor(d2, cophenetic(hc3))    
hc4 <- hclust(d2,method = "complete") ; cor(d2, cophenetic(hc4))

windows()
par(mfrow=c(4,1),las=1)
plot(hc1,cex=0.7)
plot(hc2,cex=0.7)
plot(hc3,cex=0.7)
plot(hc4,cex=0.7)

# comparacion grafica 2 a 2
library(dendextend)
tanglegram(hc1,hc2)
tanglegram(hc1,hc3)
tanglegram(hc1,hc4)
tanglegram(hc2,hc3)
tanglegram(hc2,hc4)
tanglegram(hc3,hc4)

# correlaciones 2 a 2
library(corrplot)
dend_list <- dendlist("RC" = as.dendrogram(hc1), "AC" = as.dendrogram(hc2),
                      "RT" = as.dendrogram(hc3), "AT" = as.dendrogram(hc4))
cors <- cor.dendlist(dend_list)
par(mfrow=c(1,1))
corrplot(cors, "pie", "lower")

# 5. Segun los 4 dendogramas, Cuantos grupos de vino crees que hay? Haz la particion segun 
# el numero de grupos que creas conveniente para cada uno de los sistemas
ct1 <- cutree(hc1,k=3)
ct2 <- cutree(hc2,k=3)
ct3 <- cutree(hc3,k=3)
ct4 <- cutree(hc4,k=3)

# 6. Representa las 4 clasificaciones en las 2 primeras componentes principales
# Que combinacion te parece mas apropiada?
pr <- princomp(wine2)
x <- pr$scores[,1]
y <- pr$scores[,2]
par(mfrow=c(2,2),las=1)
plot(x,y,col=ct1,pch=19)
plot(x,y,col=ct2,pch=19)
plot(x,y,col=ct3,pch=19)
plot(x,y,col=ct4,pch=19)

# 7. Escoge una particion de las anteriores y calcula el % de variabilidad explicada
##-- Inercia entre (con wine)
IB <- 0
for(i in 1:3){
  w <- wine[ct1==i,]
  n <- sum(ct1==i)
  ymean <- apply(wine,2,mean)
  ymeangroup <- apply(w,2,mean)
  ib <- n*sum((ymeangroup-ymean)^2)
  IB <- IB + ib
}

##-- Inercia global (con wine)
IT <- sum(apply(scale(wine,scale=FALSE)^2,2,sum,na.rm=TRUE))

##-- Varianza explicada
VE <- round(100*IB/IT,2)
VE


# 8. Define las caracteristicas mas relevantes de cada grupo de vinos segun la clasificacion escogida. 
# ¿En que caracteristica difieren menos?
apply(wine,2,tapply,ct1,summary)

pca <- PCA(wine,graph=FALSE)
par(mfrow=c(1,2))
plot(pca,col.ind=ct1,label='none')
plot(pca,choix = "var",cex=0.7)

par(mfrow=c(4,4),las=1)
for(i in 1:13) boxplot(wine2[,i]~ct1,main=colnames(wine2)[i])
