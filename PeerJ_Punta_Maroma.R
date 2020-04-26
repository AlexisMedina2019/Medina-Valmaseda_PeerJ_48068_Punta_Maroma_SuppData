rm(list=ls(all=TRUE))
getwd()
#Load libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(wesanderson)
# original data for all 1456 coral colonies surveyed in 2019
DenMorf3= read.csv("CSV Morphometric_data1.csv")
# transformations (Zona:geomorphic zone)
DenMorf3$Zona= factor(DenMorf3$Zona, levels=c("RF", "HG"))
levels(DenMorf3$Zona) <- c("RF 2019", "HG 2019")
# Suppl. figure S1.B 
# Plot coral colonies by diameter maximum and by geomorphic zone,
# RF 2019: reef front in 2019,
# HG 2019: hard ground community in 2019)
ggplot(DenMorf3) +
  aes(x = D.max_cm, fill = Morph) +
  theme (axis.title = element_text(face="bold", colour="black", angle=90, size=rel(1)))+
  theme (axis.text.y = element_text(face="bold", colour="black", size=rel(2), hjust=0.5))+
  labs(x= " max.diameter( Ø,cm)", y = "N")+
  geom_histogram(bins = 30L, color="black")+
  scale_fill_manual(values = wes_palette(n=5, name="Cavalcanti1"))+
  theme_bw()+ 
  facet_wrap(vars(Zona))+
  theme(strip.text = element_text(face = "bold", size=rel(1.2)))+
  theme(legend.position = "bottom")+
  theme(strip.text = element_text(face = "bold"))+
  labs(x = "Diameter Maximum of Colonies(cm)")+
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1.2), hjust=0.5))+
  theme (axis.text.y = element_text(face="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))+
  theme (axis.title = element_text(face="bold", colour="black", size=rel(1.2)))
# original data representing 1456 colonies by densities of each species in 2019
DenMorfN=read.csv("DenSpp-Mar.csv")
# transformations
DenMorfN$Zona = factor(DenMorfN$Zona, levels=c("RF_2019", "HG_2019"))
levels(DenMorfN$Zona) <- c("RF 2019", "HG 2019")
levels(DenMorfN$Zona)
morfo1<-ggplot(DenMorfN) +
  aes(x = key_sp, y = Den10, fill = Colony_morphology) +
  scale_fill_manual(values = wes_palette(n=5, name="Cavalcanti1"))+
  theme (axis.text.y = element_text(face="bold", colour="black", size=rel(2), angle=90, hjust=0.5))+
  geom_boxplot() +
  theme_bw()+ 
  facet_wrap(vars(Zona))+
  theme(strip.text = element_text(face = "bold", size=rel(1.2)))+
  labs(y = "Density(ind 10 m-2)")+
  theme (axis.text.y = element_text(face="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
graA11<-morfo1 + scale_y_continuous(breaks=c(0,10,20,30))+
  theme (axis.title = element_text(face="bold", colour="black", size=rel(1.2)))
# original data of life coral coverage of each species for 1979 and 1985 years
DenMorf0=read.csv("Historico.79.85_MarSp.csv")
# trasnformations
DenMorf0$Zona = factor(DenMorf0$Zona, levels=c("RF before 1990s", "HG before 1990s"))
levels(DenMorf0$Zona) <- c("RF before 1990s", "HG before 1990s")
levels(DenMorf0$Zona)
morfo2<-ggplot(DenMorf0) +
  aes(x = key_sp, y = LCC, fill = Colony_morphology) +
  scale_fill_manual(values = wes_palette(n=5, name="Cavalcanti1"))+
  theme (axis.text.y = element_text(face="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))+
  geom_boxplot() +
  theme_bw()+ 
  facet_wrap(vars(Zona))+
  theme(strip.text = element_text(face = "bold", size=rel(1.2)))+
  labs(x = "Accretion-functional groups")+
  labs(y = "Live Coral Coverage (%)")+
  theme(legend.position = "bottom")+
  theme (axis.title = element_text(face="bold", colour="black", size=rel(1.2)))
graA12<-morfo2+ scale_y_continuous(breaks=c(0,10,20,30))+
  theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1.2)),axis.text.y = element_text(face ="bold", colour="black", size=rel(1.2), angle=90, hjust=0.5))
# Figure 2 of manuscript
# plot of total sample arranged coral colonies, accretion-functional groups and
# by periods,before the 1990s:data of 1979 and 1985 years and 2019
plot_grid( graA11, graA12, labels = "AUTO",align = "v",label_size = 12, rel_heights = c(1,1.18), ncol=1)+
  theme (legend.text = element_text(face="bold", colour="black", size=rel(1), hjust=0.5))+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# calculated index value of importance (IVI) 
# for each species by periods,
# before the 1990s(represented by 1985 year) and 2019 
library("readxl")
ivi<- read_xlsx("Data S3 IVI.xlsx")
# trasformations
ivi$Year = as.factor(ivi$Year)
ivi$Year=factor(ivi$Year, levels = c("1985", "2019"))
levels(ivi$Year) <- c("1985", "2019")
RIIRF<- ivi[ivi$Zone=="RF",]
RIIHG<- ivi[ivi$Zone=="HG",]
# Source for Figure 3 of manuscript
# plot of IVI of each species by geomorphic zone and period of time
require("gridExtra")
library(grid)
library(likert)
my_text1 <- "RF"
my_text2 <- "HG"
my_grob1 = grid.text(my_text1, x=0.1,  y=0.95, gp=gpar(col="black", fontsize=14, fontface="bold"))
my_grob2 = grid.text(my_text2, x=0.90,  y=0.95, gp=gpar(col="black", fontsize=14, fontface="bold"))
ggplot() + 
    geom_bar(data=RIIRF, aes(x = Spp., y=-IVI, fill=Year), position = position_stack(reverse = TRUE), stat="identity") +
    geom_bar(data=RIIHG, aes(x = Spp., y=IVI, fill=Year), position = position_stack(reverse = TRUE), stat="identity") +
    geom_hline(yintercept = 0, color =c("black"))+
    scale_y_continuous(breaks=c(-40,-20,0,20,40), labels = abs(c(-40,-20,0,20,40)))+
    coord_flip()+
    theme_bw()+
    ggtitle("") +  
    theme(plot.title = element_text(size = 12, face = "bold", margin=margin(10,10,0,0)))+
    labs(y="Importance Value Index (%)", x = "Species")+
    theme(axis.text.y = element_text(hjust=0)) +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "bottom")+
    scale_fill_manual(values=c("#E69F00", "#D55E00"), 
                      name="Year",
                      breaks=c("1985", "2019"),
                      labels=c("Before 1990s", "2019"))+
    theme (axis.text.x = element_text(face="bold", colour="black", size=rel(1)),axis.text.y = element_text(face ="bold", colour="black", size=rel(1.2), hjust=0.5))+
    theme (axis.text.y = element_text(face="bold", colour="black", size=rel(0.8), hjust=0.5))+
    theme (axis.title = element_text(face="bold", colour="black", size=rel(1)))+
    annotation_custom(my_grob1)+
    annotation_custom(my_grob2)
