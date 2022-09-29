# load packages

library("ggplot2")
library("ggrepel")


# load table for mammals or birds

data2<-read.csv(file.choose(),head=T)


data2$Name=factor(data2$Name, levels=unique(data2$Name), ordered=TRUE)


# Diurnality shift with HF plot


# For mammals
ggplot(data2, aes(x=Name, y=Diurnality_Low, color=Name))+
  geom_point(size=5, pch=16, color="black")+
  geom_point(aes(x=Name, y=Diurnality_High), size=6, pch="\u25C4")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_segment(aes(x = Name, y = Diurnality_Low, xend = Name, yend = Diurnality_High), size=3)+
  labs(x=" ", y="Diurnality (%)")+
  theme(axis.text.x = element_text(color="black", angle=0,size = 13),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 15, face="italic"))+
  theme(axis.title = element_text(color="black", size=15, face="bold"))+
  scale_color_manual(values=c("dodgerblue4", "dodgerblue4","dodgerblue4","dodgerblue4",
                              "dodgerblue4","dodgerblue4", "gold2"))+
  geom_point(aes(x=Name, y=Diurnality_Low), size=5, pch=16, color="black")+
  coord_flip()+
  geom_abline(slope=0, intercept=0,  col = "grey30",lty=2, size=0.5)+
  geom_abline(slope=0, intercept=100,  col = "grey30",lty=2, size=0.5)+
  ylim(0, 100)+ theme(legend.position = "none")


# For Birds

ggplot(data2, aes(x=Name, y=Diurnality_Low, color=Name))+
  geom_point(size=5, pch=16, color="black")+
  geom_point(aes(x=Name, y=Diurnality_High), size=6, pch="\u25BA")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_segment(aes(x = Name, y = Diurnality_Low, xend = Name, yend = Diurnality_High), size=3)+
  labs(x=" ", y="Diurnality (%)")+
  theme(axis.text.x = element_text(color="black", angle=0,size = 13),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 15, face="italic"))+
  theme(axis.title = element_text(color="black", size=15, face="bold"))+
  scale_color_manual(values=c("dodgerblue4", "gold2", "gold2", "gold2", "gold2"))+
  geom_point(aes(x=Name, y=Diurnality_Low), size=5, pch=16, color="black")+
  coord_flip()+
  geom_abline(slope=0, intercept=0,  col = "grey30",lty=2, size=0.5)+
  geom_abline(slope=0, intercept=100,  col = "grey30",lty=2, size=0.5)+
  ylim(0, 100)+ theme(legend.position = "none")



## arrows https://stackoverflow.com/questions/30742379/creating-new-shape-palettes-in-ggplot2-and-other-r-graphics




ggplot(data2, aes(x=Species, y=Diurnality_High, color=Species))+
  geom_point(pch="\u25BC", size=2.5)+
  geom_point(aes(x=Species, y=Diurnality_Low), size=2.5, pch=16, color="black")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_segment(aes(x = Species, y = Diurnality_Low, xend = Species, yend = Diurnality_High), size=1.2)+
  labs(x=" ", y="Diurnality (%)")+
  theme(axis.text.x = element_text(color="black", angle=0),
        axis.title.y = element_text(color="black", size=15, face="bold", angle = 0))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 10, face="italic"))+
  scale_color_manual(values=c("dodgerblue4", "dodgerblue4","dodgerblue4","dodgerblue4",
                              "dodgerblue4","dodgerblue4","gold2"))+
  coord_flip()+
  geom_abline(slope=0, intercept=0,  col = "grey30",lty=2, size=0.5)+
  geom_abline(slope=0, intercept=100,  col = "grey30",lty=2, size=0.5)+
  ylim(0, 100)+ theme(legend.position = "none")







# ggplot(data2, aes(x=Species, y=Diurnality_High, color=Species))+
  geom_point(pch="\u25BC", size=2.5, color="grey")+
  geom_point(aes(x=Species, y=Diurnality_Low), size=2.5, pch=16, color="black")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
  geom_segment(aes(x = Species, y = Diurnality_Low, xend = Species, yend = Diurnality_High), size=1.2)+
  labs(x=" ", y="Diurnality (%)")+
  theme(axis.text.x = element_text(color="black", angle=0),
        axis.title.y = element_text(color="black", size=10, face="bold"))+
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 10))+
  scale_color_manual(values=c("dodgerblue1", "dodgerblue1","dodgerblue1","dodgerblue1",
                              "dodgerblue1","dodgerblue1","gold2"))+
  coord_flip()+
  geom_abline(slope=0, intercept=0,  col = "black",lty=2, size=0.8)+
  geom_abline(slope=0, intercept=100,  col = "black",lty=2, size=0.8)+
  ylim(0, 100)

