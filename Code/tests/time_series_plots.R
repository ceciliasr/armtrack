
setwd("c:/TFM/Data")

if (!require("ggplot2"))
{
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("plotly"))
{
  install.packages("plotly")
  library(plotly)
}
if (!require("ggpubr"))
{
  install.packages("ggpubr")
  library(ggpubr)
}
if (!require("gganimate"))
{
  install.packages("gganimate")
  library(gganimate)
}

load("BDistrophyD.rda")

BMD<-BMD[1:180002,]

se<-seq(0,nrow(BMD),50)
dd1<-BMD[rownames(BMD) %in% se,]
rm(se)

LElb_2<-subset(dd1,select=c(Timestamp,LElbPos_2))
LElb_2$parte<-"X"
names(LElb_2)<-c("Timestamp","Pos_x","parte")
LElb_3<-subset(dd1,select=c(Timestamp,LElbPos_3))
LElb_3$parte<-"Y"
names(LElb_3)<-c("Timestamp","Pos_x","parte")
LElb_4<-subset(dd1,select=c(Timestamp,LElbPos_4))
LElb_4$parte<-"Z"
names(LElb_4)<-c("Timestamp","Pos_x","parte")

RElb_2<-subset(dd1,select=c(Timestamp,RElbPos_2))
RElb_2$parte<-"X"
names(RElb_2)<-c("Timestamp","Pos_x","parte")
RElb_3<-subset(dd1,select=c(Timestamp,RElbPos_3))
RElb_3$parte<-"Y"
names(RElb_3)<-c("Timestamp","Pos_x","parte")
RElb_4<-subset(dd1,select=c(Timestamp,RElbPos_4))
RElb_4$parte<-"Z"
names(RElb_4)<-c("Timestamp","Pos_x","parte")


LEdd<-rbind.data.frame(LElb_2,LElb_3,LElb_4)
REdd<-rbind.data.frame(RElb_2,RElb_3,RElb_4)

p1 <- ggplot(LEdd, aes(x=Timestamp/1000, y=Pos_x, color=parte)) +
  geom_line() + transition_reveal(Timestamp)+
  scale_x_continuous(name ="Timestamp")+
  scale_y_continuous(name="Posición")
p1<-animate(p1,500)
anim_save("c:/TFM/Results/BMDT_1aHora_LElb.gif",p1)

p2 <- ggplot(REdd, aes(x=Timestamp/1000, y=Pos_x, color=parte)) +
  geom_line() + transition_reveal(Timestamp)+
  scale_x_continuous(name ="Timestamp")+
  scale_y_continuous(name="Posición")
p2<-animate(p2,500)
anim_save("c:/TFM/Results/BMDT_1aHora_RElb.gif",p2)


LHand_2<-subset(dd1,select=c(Timestamp,LHandPos_2))
LHand_2$parte<-"X"
names(LHand_2)<-c("Timestamp","Pos_x","parte")
LHand_3<-subset(dd1,select=c(Timestamp,LHandPos_3))
LHand_3$parte<-"Y"
names(LHand_3)<-c("Timestamp","Pos_x","parte")
LHand_4<-subset(dd1,select=c(Timestamp,LHandPos_4))
LHand_4$parte<-"Z"
names(LHand_4)<-c("Timestamp","Pos_x","parte")

RHand_2<-subset(dd1,select=c(Timestamp,RHandPos_2))
RHand_2$parte<-"X"
names(RHand_2)<-c("Timestamp","Pos_x","parte")
RHand_3<-subset(dd1,select=c(Timestamp,RHandPos_3))
RHand_3$parte<-"Y"
names(RHand_3)<-c("Timestamp","Pos_x","parte")
RHand_4<-subset(dd1,select=c(Timestamp,RHandPos_4))
RHand_4$parte<-"Z"
names(RHand_4)<-c("Timestamp","Pos_x","parte")


LEdd<-rbind.data.frame(LHand_2,LHand_3,LHand_4)
REdd<-rbind.data.frame(RHand_2,RHand_3,RHand_4)

p1 <- ggplot(LEdd, aes(x=Timestamp/1000, y=Pos_x, color=parte)) +
  geom_line()+
  scale_x_continuous(name ="Timestamp")+
  scale_y_continuous(name="Posición")
p1<-animate(p1,500)
anim_save("c:/TFM/Results/BMDT_1aHora_LHand.gif",p1)

p2 <- ggplot(REdd, aes(x=Timestamp/1000, y=Pos_x, color=parte)) +
  geom_line() + transition_reveal(Timestamp)+
  scale_x_continuous(name ="Timestamp")+
  scale_y_continuous(name="Posición")
p2<-animate(p2,500)
anim_save("c:/TFM/Results/BMDT_1aHora_RHand.gif",p2)
