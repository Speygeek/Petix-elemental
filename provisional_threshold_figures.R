#load required R packages

library(ggplot2)
library(scales)
library(cowplot)

#load data

#data for NOCA + OLYM [PLAGLA + ALESAR]
elemental_3 <- read.csv("C:/Users/meagh/Desktop/PhD Research/DATA -- ELEMENTAL ANALYSIS/LICHEN_ELEMENTAL/NOCA_OLYM_ELEMENTAL_2.csv")

elemental_3 <- na.omit(elemental_3)

#data for NOCA [LETVUL + LETCOL] and OLYM [LOBORE + USNEA]
per_N_2 <- read.csv("C:/Users/meagh/Desktop/PhD Research/DATA -- ELEMENTAL ANALYSIS/LICHEN_ELEMENTAL/NOCA_OLYM_N_CONTENT_2.csv")

per_N_2 <- na.omit(per_N_2)

###

#subsetting data by park
elem_noca <- elemental_3[elemental_3$PARK=='NOCA',]

elem_olym <- elemental_3[elemental_3$PARK=='OLYM',]

per_N_noca_2 <- per_N_2[per_N_2$PARK=='NOCA',]

per_N_olym_2 <- per_N_2[per_N_2$PARK=='OLYM',]

###

#defining species names for legend
elem_noca$SPECIES <- as.factor(elem_noca$SPECIES)

elem_noca$Species <- elem_noca$SPECIES

elem_noca$Species <- as.character(elem_noca$Species)

elem_noca$Species[elem_noca$Species=="ALESAR"] <- "Alectoria sarmentosa"

elem_noca$Species[elem_noca$Species=="PLAGLA"] <- "Platismatia glauca"

elem_olym$SPECIES <- as.factor(elem_olym$SPECIES)

elem_olym$Species <- elem_olym$SPECIES

elem_olym$Species <- as.character(elem_olym$Species)

elem_olym$Species[elem_olym$Species=="ALESAR"] <- "Alectoria sarmentosa"

elem_olym$Species[elem_olym$Species=="PLAGLA"] <- "Platismatia glauca"


###

#defining species names for legend
per_N_olym_2$SPECIES <- as.factor(per_N_olym_2$SPECIES)

per_N_olym_2$Species <- per_N_olym_2$SPECIES

per_N_olym_2$Species <- as.character(per_N_olym_2$Species)

per_N_olym_2$Species[per_N_olym_2$Species=="USNEA"] <- "Usnea cornuta"

per_N_olym_2$Species[per_N_olym_2$Species=="LOBORE"] <- "Lobaria oregana"

###

per_N_noca_2$SPECIES <- as.factor(per_N_noca_2$SPECIES)

per_N_noca_2$Species <- per_N_noca_2$SPECIES

per_N_noca_2$Species <- as.character(per_N_noca_2$Species)

per_N_noca_2$Species[per_N_noca_2$Species=="LETVUL"] <- "Letharia vulpina"

per_N_noca_2$Species[per_N_noca_2$Species=="LETCOL"] <- "Letharia columbiana"

#creating multi-panel figure to display lichen %N for each species in both parks

####OLYM_ELEMENTAL --> Lichen %N vs. Longitude (decimal degrees)

###PLAGLA + ALESAR

olym_N <- ggplot(elem_olym, aes(x=LONG, y=X.N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  ggtitle('Olympic')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  geom_hline(yintercept=0.59, linetype="dashed", color="#999999",size=1.2)+
  geom_hline(yintercept=0.65, linetype="dashed", color="#009E73", size=1.2)+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.35,0.9))+
  scale_color_manual(values=c("#009E73","#999999"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab("Lichen %N")+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(0,0.8))
plot(olym_N)

###SHRUBBY USNEA + LOBORE

olym_N_2 <- ggplot(per_N_olym_2, aes(x=LONG, y=X.N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.3,0.9))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  geom_hline(yintercept=2.36, linetype="dashed", color="#66CC99",size=1.2)+
  geom_hline(yintercept=0.75, linetype="dashed", color="#FF6666", size=1.2)+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#66CC99", "#FF6666"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab("Lichen %N")+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(0,3))
plot(olym_N_2)

####NOCA_ELEMENTAL --> Lichen %N vs. Longitude (decimal degrees)

###PLAGLA + ALESAR

noca_N <- ggplot(elem_noca, aes(x=LONG, y=X.N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  ggtitle('North Cascades')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.35,0.9))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  geom_hline(yintercept=0.59, linetype="dashed", color="#999999",size=1.2)+
  geom_hline(yintercept=0.65, linetype="dashed", color="#009E73", size=1.2)+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#009E73","#999999"))+
  ylab("Lichen %N")+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(0,0.8))
plot(noca_N)

###LETVUL + LETCOL

noca_N_2 <- ggplot(per_N_noca_2, aes(x=LONG, y=X.N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.35,0.95))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  geom_hline(yintercept=1.03, linetype="dashed", color="#CC79A7",size=1.2)+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#CC79A7","#0072B2"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab("Lichen %N")+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(0,1.2))
plot(noca_N_2)


###combine 4 plots
###exporting plot to high resolution jpeg file

jpeg(filename="N_threshold_2.jpeg",bg="transparent",res=1200,units="in",height=10,width=10)

p2 <- plot_grid(olym_N,noca_N,olym_N_2,noca_N_2,ncol=2,align="hv",label_size=20,labels=c('(a)','(b)','(c)','(d)'))
p2
dev.off()

#######

#creating multi-panel figure to display lichen d15N for each species in both parks

####OLYM_ELEMENTAL --> Lichen d15N vs. Longitude (decimal degrees)

###PLAGLA + ALESAR

olym_d15N <- ggplot(elem_olym, aes(x=LONG, y=d15N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  ggtitle('Olympic')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.36,0.9))+
  scale_color_manual(values=c("#009E73","#999999"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab(expression(paste('Lichen'~ delta^{15},'N'~'(\u2030)')))+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(-10,-2))
plot(olym_d15N)

###SHRUBBY USNEA + LOBORE

olym_d15N_2 <- ggplot(per_N_olym_2, aes(x=LONG, y=d15N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.3,0.95))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#66CC99", "#FF6666"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab(expression(paste('Lichen'~ delta^{15},'N'~'(\u2030)')))+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(-10,0))
plot(olym_d15N_2)

####NOCA_ELEMENTAL --> Lichen d15N vs. Longitude (decimal degrees)

###PLAGLA + ALESAR

noca_d15N <- ggplot(elem_noca, aes(x=LONG, y=d15N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  ggtitle('North Cascades')+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.375,0.9))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#009E73","#999999"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab(expression(paste('Lichen'~ delta^{15},'N'~'(\u2030)')))+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(-12,0))
plot(noca_d15N)

###LETVUL + LETCOL

noca_d15N_2 <- ggplot(per_N_noca_2, aes(x=LONG, y=d15N, shape=Species, color=Species)) +
  geom_point(size=5) +
  theme_classic() +
  theme(legend.title = element_blank())+
  theme(legend.position = c(0.36,0.95))+
  theme(legend.background = element_rect(colour="black",fill="white",linetype="solid"))+
  theme(legend.margin=ggplot2::margin(c(-5,5,5,5)))+
  theme(text = element_text(size=20), legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#CC79A7","#0072B2"))+
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  ylab(expression(paste('Lichen'~ delta^{15},'N'~'(\u2030)')))+
  xlab("Longitude (decimal degrees)")+
  coord_cartesian(ylim=c(-10,0))
plot(noca_d15N_2)

###combine 4 plots
###exporting plot to high resolution jpeg file

jpeg(filename="d15N_noca_olym_2.jpeg",bg="transparent",res=1200,units="in",height=10,width=10)

p3 <- plot_grid(olym_d15N,noca_d15N,olym_d15N_2,noca_d15N_2,ncol=2,align="hv",label_size=20,labels=c('(a)','(b)','(c)','(d)'))
p3
dev.off()
