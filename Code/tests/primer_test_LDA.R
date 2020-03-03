data<-dd[,c(3:12,16:21)]
pca<-prcomp(data,center=T,scale=T)
comp<-cbind.data.frame(data.frame(dd[,1]),data.frame(pca$x[,1:5]))
names(comp)[1]<-"Timestamp"

#Possible activities: Walk, pc, eat, reach up, movements, shower 
#First we put label on the actions:
comp$action<-ifelse(comp$Timestamp>=0 & comp$Timestamp<=140,"walk",
                    ifelse(comp$Timestamp>140 & comp$Timestamp<=260,"pc",
                           ifelse(comp$Timestamp>260 & comp$Timestamp<=380,"eat",
                                  ifelse(comp$Timestamp>380 & comp$Timestamp<=500,"reachup",
                                         ifelse(comp$Timestamp>500 & comp$Timestamp<=550,"movements",
                                                ifelse(comp$Timestamp>550,"shower","error"))))))

#Divide dataset into train and test. 
#1st option: We take the same number of registers of each action (75%):
walk<-comp[which(comp$action=="walk"),]
walk<-walk[c(1:(round(nrow(walk)/2)+round(nrow(walk)/4))),]
pc<-comp[which(comp$action=="pc"),]
pc<-pc[c(1:(round(nrow(pc)/2)+round(nrow(pc)/4))),]
eat<-comp[which(comp$action=="eat"),]
eat<-eat[c(1:(round(nrow(eat)/2)+round(nrow(eat)/4))),]
reachup<-comp[which(comp$action=="reachup"),]
reachup<-reachup[c(1:(round(nrow(reachup)/2)+round(nrow(reachup)/4))),]
movements<-comp[which(comp$action=="movements"),]
movements<-movements[c(1:(round(nrow(movements)/2)+round(nrow(movements)/4))),]
shower<-comp[which(comp$action=="shower"),]
shower<-shower[c(1:(round(nrow(shower)/2)+round(nrow(shower)/4))),]

train<-rbind.data.frame(walk,pc,eat,reachup,movements,shower)
walk<-comp[which(comp$action=="walk"),]
walk<-walk[c(((round(nrow(walk)/2)+round(nrow(walk)/4))+1):nrow(walk)),]
pc<-comp[which(comp$action=="pc"),]
pc<-pc[c(((round(nrow(pc)/2)+round(nrow(pc)/4))+1):nrow(pc)),]
eat<-comp[which(comp$action=="eat"),]
eat<-eat[c(((round(nrow(eat)/2)+round(nrow(eat)/4))+1):nrow(eat)),]
reachup<-comp[which(comp$action=="reachup"),]
reachup<-reachup[c(((round(nrow(reachup)/2)+round(nrow(reachup)/4))+1):nrow(reachup)),]
movements<-comp[which(comp$action=="movements"),]
movements<-movements[c(((round(nrow(movements)/2)+round(nrow(movements)/4))+1):nrow(movements)),]
shower<-comp[which(comp$action=="shower"),]
shower<-shower[c(((round(nrow(shower)/2)+round(nrow(shower)/4))+1):nrow(shower)),]

test<-rbind.data.frame(walk,pc,eat,reachup,movements,shower)


library(MASS)

#Performing discriminant analysis
#We discard PC3 because it is constant always.
mod <- lda(action ~ .,train[,c(2,3,5,6,7)])

pred<-predict(mod,newdata=test[,c(2,3,5,6)])
res_pred<-data.frame(pred$class,pred$posterior*100)

test_result<-cbind.data.frame(test,res_pred)
names(test_result)[8]<-"action_pred"
test_result$well_pred<-ifelse(test_result$action==test_result$action_pred,0,1)
table(as.factor(test_result$action),as.factor(test_result$action_pred))

#clus<-kmeans(comp[,2:6],5)
#clus$size
