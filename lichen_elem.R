#load required R packages

library(ggplot2)
library(dplyr)
library(jtools)
library(outliers)
library(interactions)

#load data

###load data --> this data frame has 'dist_interstate' in km and 'ELEV_2' in m
###easier interpretation of regression coefficients

elemental_4 <- read.csv("C:/Users/meagh/Desktop/PhD Research/DATA -- ELEMENTAL ANALYSIS/LICHEN_ELEMENTAL/NOCA_OLYM_ELEMENTAL_4.csv")

elemental_4 <- na.omit(elemental_4)

#remove outlier [NOCA -- RC LOW -- ALESAR]
#see 'Grubb's Outlier Test' below
elem_reduced3 <- elemental_4 %>%  filter(!row_number() %in% c(14))

###

#subsetting data by species and park

elemental_a <- elem_reduced3[elem_reduced3$SPECIES=='ALESAR',]

elemental_b <- elem_reduced3[elem_reduced3$SPECIES=='PLAGLA',]

elemental_c <- elem_reduced3[elem_reduced3$PARK=='NOCA',]

elemental_d <- elem_reduced3[elem_reduced3$PARK=='OLYM',]


###

##Grubb's Outlier Test

grubbs.test(elemental_4$X.N)
grubbs.test(elemental_4$d15N, opposite = TRUE)

###

#linear model for lichen %N
p7 <- lm(X.N ~ PARK * SPECIES * ELEV_2 * dist_interstate, data=elem_reduced3)

#look at distribution of data
hist(elem_reduced3$X.N)
#model diagnostics
qqPlot(resid(p7))
shapiro.test(resid(p7))

#summary table
summ(p7, digits=3)
export_summs(p7, digits=3)
export_summs(p7,
             error_format = "({statistic}, p = {p.value})",
             error_pos = c("below"),
             ci_level = 0.95,
             statistics = NULL,
             model.names = NULL,
             coefs = NULL,
             to.file = "docx",
             file.name = "model_summary_3.docx",
             digits=3
)

#linear model for lichen d15N
p8 <- lm(d15N ~ PARK * SPECIES * ELEV_2 * dist_interstate, data=elem_reduced3)
summary(p8)

#look at distribution of data
hist(elem_reduced3$d15N)
#model diagnostics
qqPlot(resid(p8))
shapiro.test(resid(p8))

#summary table
summ(p8, digits=3)
export_summs(p8, digits=3)
export_summs(p8,
             error_format = "({statistic}, p = {p.value})",
             error_pos = c("below"),
             ci_level = 0.95,
             statistics = NULL,
             model.names = NULL,
             coefs = NULL,
             to.file = "docx",
             file.name = "model_summary.docx",
             digits=3
)

###

#interaction plot

s8 <- interact_plot(p8, pred = dist_interstate, modx = ELEV_2, modx.values = c(500, 1000, 1500), plot.points = TRUE, jitter = 0.1, point.size = 4, line.thickness = 1.5, centered = "none", johnson_neyman = FALSE,
                    x.label = "Distance from Interstate (km)", y.label = "Lichen d15N",
                    legend.main = "Elevation (m)", colors=c("#999999", "#333333", "#000000")) +
  scale_shape_manual(values=c(0,16,3))+
  theme_classic() +
  theme(text = element_text(size=20))+
  theme(legend.position = c(0.85, 0.2))+
  ylab(expression(paste("Lichen ", delta^{15}, "N (\u2030)")))+
  xlab("Distance from Interstate (km)")

s8

###exporting plot to high resolution jpeg file
jpeg(filename="interact_plot.jpeg",bg="transparent",res=600,units="in",height=10,width=10)
dev.off()

#simple slopes analysis

s2 <- sim_slopes(p8, pred = dist_interstate, modx = ELEV_2, modx.values = c(500, 1000, 1500),
                 johnson_neyman = FALSE)
s2
plot(s2)

###

##NOCA_OLYM_ELEMENTAL --> Lichen d15N vs. %N

N_d15N <- ggplot(elemental_2, aes(x=X.N, y=d15N)) +
  geom_point(size=4) +
  theme_classic() +
  theme(text = element_text(size=20))+
  scale_color_manual(values=c("#009E73","#999999"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylab(expression(paste("Lichen ", delta^{15}, "N (\u2030)")))+
    xlab("%N")
plot(N_d15N)

jpeg(filename="lichen_d15N_N.jpeg",bg="transparent",res=600,units="in",height=6,width=8)
dev.off()

##NOCA_OLYM_ELEMENTAL --> Lichen d15N vs. %N
#BY LICHEN SPECIES

ggplot(elemental_2, aes(x=X.N, y=d15N, shape=SPECIES, color=SPECIES)) +
  geom_point()

N_d15N <- ggplot(elemental_2, aes(x=X.N, y=d15N, color=SPECIES)) +
  geom_point(size=3) +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#009E73","#999999"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylab(expression(paste("Lichen ", delta^{15}, "N (\u2030)")))+
   xlab("%N")
plot(N_d15N)

##

##NOCA_OLYM_ELEMENTAL --> Lichen d15N vs. %N
#BY PARK

##NOCA

N_d15N_noca <- ggplot(elemental_c, aes(x=X.N, y=d15N)) +
  geom_point(size=3) +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#009E73","#999999"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylab(expression(paste("Lichen ", delta^{15}, "N (\u2030)")))+
  xlab("%N")
plot(N_d15N_noca)

##OLYM

N_d15N_olym <- ggplot(elemental_d, aes(x=X.N, y=d15N)) +
  geom_point(size=3) +
  theme_classic() +
  theme(legend.text = element_text(face = "italic"))+
  scale_color_manual(values=c("#009E73","#999999"))+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ylab(expression(paste("Lichen ", delta^{15}, "N (\u2030)")))+
  xlab("%N")
plot(N_d15N_olym)

