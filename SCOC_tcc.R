rm(list=ls())
library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(marmap)
library(maptools)
library(raster)
library(viridis)
setwd("C:/Users/user/Downloads/oc remineralization")
GPSC_TOU <- read_csv("GPSC_TOU.csv")

loc <- read_excel("GPSC_macro_sorting_2022.06.20.xlsx", sheet=1)
site<-loc %>% 
  group_by(Cruise, Station)%>%
  summarise(Latitude=mean(Latitude),
            Longitude=mean(Longitude))

GPSC_TOU<-full_join(GPSC_TOU,site,by=c("Cruise","Station")) %>% filter(!is.na(Date))
GPSC_TOU$Latitude[is.na(GPSC_TOU$Latitude)==T&GPSC_TOU$Station=="GC2"]<-22.40067
GPSC_TOU$Longitude[is.na(GPSC_TOU$Longitude)==T&GPSC_TOU$Station=="GC2"]<-120.3348
GPSC_TOU$Longitude[is.na(GPSC_TOU$Longitude)==T&GPSC_TOU$Station=="GC3"]<-120.2665
GPSC_TOU$Latitude[is.na(GPSC_TOU$Latitude)==T&GPSC_TOU$Station=="GC3"]<-22.34978
GPSC_TOU$Habitat[GPSC_TOU$Station %in% c( "L1", "S3", "S4", "S5", "S6", "S7", "S1", "S2")]<-"Shelf"
TOU<-GPSC_TOU %>%  dplyr::select(c("Cruise","Habitat","Region","Depth","Station",
                                   "Deployment","Tube","In_situ_temperature","In_situ_DO_flux"))
sed<-read_excel("GPSC_sediment_2021.08.16_ysl.xlsx", sheet=1)
sed<-sed %>% filter(Section=="0-1") %>%  dplyr::select(c("Cruise","Station","Deployment","Section",
                                                 "Clay","Silt","Sand","CN","TOC","TN")) %>% 
  filter(!is.na(Clay))

TOU<-full_join(TOU,sed,by=c("Cruise","Station")) %>% filter(!is.na(In_situ_DO_flux)) %>% 
  dplyr::select(c(1:5,8:9,12:17))

#map
Station<-GPSC_TOU %>% 
  dplyr::select(c("Station","Latitude","Longitude","Habitat","Region")) %>%
  group_by(Station,Habitat,Region) %>% 
  summarise(long=mean(Longitude),
            lat=mean(Latitude)) %>% 
  filter(long > 120 & long < 120.8) %>% 
  filter(lat  > 21.6 & lat < 22.6)
Station<-unique(Station)
GPSC<-read.table('119_121_21_23.xyz',
                 col.names = c("long", "lat", "depth")) %>%
  data.frame %>% 
  filter(long > 120 & long < 120.8) %>% 
  filter(lat  > 21.6 & lat < 22.6)

ggplot()+
  xlab("Longitude")+ylab("Latitude")+
  geom_tile(data = GPSC[GPSC$depth >= 0,], 
            aes(x = long, y = lat), fill = "grey65") +
  geom_raster(data = GPSC[GPSC$depth < 0 & GPSC$depth > -3000 ,],
              aes(x =long, y = lat, fill = depth)) +
  scale_fill_viridis(option = "G",limits = c(-3000, 0),
                     breaks = seq(0,-3000, -500),
                     labels = abs) +
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=0,size=0.8,colour="black")+
  geom_contour(data =GPSC, aes(x=long, y=lat, z=depth),
               breaks=c(-300,-600,-900,-1200,
                        -1500,-1800,-2100,-2400),size=0.3,colour="bisque4")+
  coord_fixed(expand = FALSE) +
  labs(fill = "Depth (m)")+
  geom_point(data = Station,aes(x = long, y =lat,
                                color=Habitat,shape=Region),size=3)+
  scale_color_manual(values=c('#610345','#F34213', '#E3B505'))+
  geom_text_repel(data=Station,aes(x = long, y = lat,
                                   label = Station),fontface = "bold", 
                  size=3.5,color="grey30",max.overlaps=Inf) +
  theme_bw()

#fill NA value with mean value from the same station####
TOU$Clay[is.na(TOU$Clay)==T&TOU$Station=="GC1"]<-12.49412
TOU$Clay[is.na(TOU$Clay)==T&TOU$Station=="GS1"]<-22.92222
TOU$Clay[is.na(TOU$Clay)==T&TOU$Station=="L1"]<-8.75
TOU$Clay[is.na(TOU$Clay)==T&TOU$Station=="GC3"]<-22.53333

TOU$Silt[is.na(TOU$Silt)==T&TOU$Station=="GC1"]<-47.76471
TOU$Silt[is.na(TOU$Silt)==T&TOU$Station=="GS1"]<-74.12222
TOU$Silt[is.na(TOU$Silt)==T&TOU$Station=="L1"]<-68.75
TOU$Silt[is.na(TOU$Silt)==T&TOU$Station=="GC3"]<-65.92333

TOU$Sand[is.na(TOU$Sand)==T&TOU$Station=="GC1"]<-39.74118
TOU$Sand[is.na(TOU$Sand)==T&TOU$Station=="GS1"]<-2.955556
TOU$Sand[is.na(TOU$Sand)==T&TOU$Station=="L1"]<-22.5
TOU$Sand[is.na(TOU$Sand)==T&TOU$Station=="GC3"]<-11.54333

TOU$CN[is.na(TOU$CN)==T&TOU$Station=="GC1"]<-9.551817
TOU$CN[is.na(TOU$CN)==T&TOU$Station=="GS1"]<-9.955464
TOU$CN[is.na(TOU$CN)==T&TOU$Station=="L1"] <-13.68226
TOU$CN[is.na(TOU$CN)==T&TOU$Station=="GC3"]<-10.02279

TOU$TOC[is.na(TOU$TOC)==T&TOU$Station=="GC1"]<-6.192887
TOU$TOC[is.na(TOU$TOC)==T&TOU$Station=="GS1"]<-6.090248
TOU$TOC[is.na(TOU$TOC)==T&TOU$Station=="L1"] <-11.65578
TOU$TOC[is.na(TOU$TOC)==T&TOU$Station=="GC3"]<-6.047289

TOU$TN[is.na(TOU$TN)==T&TOU$Station=="GC1"]<-6.026109
TOU$TN[is.na(TOU$TN)==T&TOU$Station=="GS1"]<-5.800928
TOU$TN[is.na(TOU$TN)==T&TOU$Station=="L1"] <-11.51224
TOU$TN[is.na(TOU$TN)==T&TOU$Station=="GC3"]<-0.1188548
#####
TOU <-TOU %>% filter(Station!='FS5C')
library(ggplot2)
TOU%>% 
  ggplot(aes(x = Depth, y = -In_situ_DO_flux))+
  geom_point(aes(color=Region,shape=Region))+
  facet_wrap(~Habitat,scale="free_x")+
  scale_y_log10()+
  stat_smooth(method="lm", formula=y~x, alpha=0.2)
TOU%>% 
  ggplot(aes(x =In_situ_temperature , y = -In_situ_DO_flux))+
  geom_point(aes(color=Region,shape=Region))+
  facet_wrap(~Habitat,scale="free_x")+
  scale_y_log10()+
  stat_smooth(method="lm", formula=y~x, alpha=0.2)

TOU_stat<-TOU %>% group_by(Region,Habitat,Station) %>% 
  summarise(mean_TOU=mean(In_situ_DO_flux),
            sd_TOU=sd(In_situ_DO_flux))

TOU_stat %>% 
  ggplot(aes(x=Station,y=-mean_TOU))+
  geom_bar(aes(fill =Region),
           stat = "identity",
           position = position_dodge(),width = 0.6)+
  facet_wrap(~Habitat,scales = "free_x")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  geom_errorbar(aes(ymin = -(mean_TOU - sd_TOU), ymax = -(mean_TOU + sd_TOU)), width = 0.1)+
  geom_point(data=TOU,aes(x=Station,y=-In_situ_DO_flux), color = "darkblue",size=1,
             position = position_jitter(0.1))
#####

library(vegan)
set.seed(100)
#check variance:PERMDISP####
#1 calculate distance
tou_dist<-vegdist(TOU$In_situ_DO_flux,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(tou_dist,group = TOU$Station)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
permu_station#nosd

#2 group->calculate data dispersion:Habitat
beta_habitat<-betadisper(tou_dist,group = TOU$Habitat)
#3 permutation grouping and calculate variance difference
permu_habitat<-permutest(beta_habitat,permutations = 9999)
permu_habitat#sd

#2 group->calculate data dispersion:Region
beta_region<-betadisper(tou_dist,group = TOU$Region)
#3 permutation grouping and calculate variance difference
permu_region<-permutest(beta_region,permutations = 9999)
permu_region#sd

#2 group->calculate data dispersion:Depth
beta_depth<-betadisper(tou_dist,group = TOU$Depth)
#3 permutation grouping and calculate variance difference
permu_depth<-permutest(beta_depth,permutations = 9999)
permu_depth#nosd

#PERMANOVA####
permanova_tou<-adonis2(TOU$In_situ_DO_flux~Habitat*Region*Station,
                       data = TOU, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_tou#allsd

library(corrplot)
TOU_cor<-TOU[2:12]
TOU_cor$Station<-as.numeric(as.factor(TOU$Station))
TOU_cor$Habitat<-as.numeric(as.factor(TOU$Habitat))
TOU_cor$Region<-as.numeric(as.factor(TOU$Region))
cor<-cor(TOU_cor)
testRes<-cor.mtest(TOU_cor, conf.level = 0.95)
testRes$p
corrplot(cor, p.mat = testRes$p, method = 'circle', type = 'lower',insig = "n",
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

#PCA method 1####
TOU_pca<-TOU[2:12]
TOU_pca$Station<-as.numeric(as.factor(TOU$Station))
TOU_pca$Habitat<-as.numeric(as.factor(TOU$Habitat))
TOU_pca$Region<-as.numeric(as.factor(TOU$Region))
pca<-prcomp(TOU_pca,scale = T)
plot(pca$x[,1],pca$x[,2])#x=PCs
pca.var<-pca$sdev^2#how much variation each PC account for
pca.var.per<-round(pca.var/sum(pca.var)*100,1)
barplot(pca.var.per,main="Scree Plot",xlab="PCs",ylab="% variation")
pca.data<-data.frame(Station=TOU$Station,
                     Region=TOU$Region,
                     Habitat=TOU$Habitat,
                     X=pca$x[,1],Y=pca$x[,2])
ggplot(data = pca.data,aes(x=X,y=Y,color=Habitat))+
  geom_text(aes(label=Station))+
  xlab(paste("PC1-",pca.var.per[1],"%",sep=""))+
  ylab(paste("PC2-",pca.var.per[2],"%",sep=""))+
  theme_bw()+
  ggtitle("PCA of TOU")
loading_score<-pca$rotation[,1]#rotation:loading score for each PC
pc<-names(sort(abs(loading_score),decreasing = T))
pca$rotation[pc,1]
pca$x[,1]
#PCA  method 2####
library(vegan)
library(ggfortify)
library(factoextra)
library(emmeans)
library(FactoMineR)
tou.pca<-PCA(TOU_pca,graph=T)
print(tou.pca)
get_eigenvalue(tou.pca)# Extract the eigenvalues/variances of principal components
fviz_eig(tou.pca,addlabels = TRUE, ylim = c(0, 50))#Visualize the eigenvalues
get_pca_ind(tou.pca)#Extract the results for individuals
var<-get_pca_var(tou.pca)#Extract the results for variables
# Coordinates
head(var$coord)
head(var$coord, 4)

# Cos2: used to estimate the quality of the representation
head(var$cos2,4)
#high cos2: good representation of the variable on the PC.
#low cos2:the variable is not perfectly represented by the PC
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Contributions:account for the variability in a given PC
# Variables that are correlated with PC1 (Dim.1) and PC2 (Dim.2) are the most important 
# in explaining the variability in the data set.
# The larger the value of the contribution, the more the variable contributes to the component.
head(var$contrib,4)
corrplot(var$contrib, is.corr=FALSE)
fviz_contrib(tou.pca, choice = "var", axes = 1,top=5)#top5 variable contribute to PC1
fviz_contrib(tou.pca, choice = "var", axes = 1:2,top=5)#total contribution to PC1&2

fviz_pca_ind(tou.pca)#Visualize the results individuals
fviz_pca_var(tou.pca,col.var = "black")# Visualize the results variables
fviz_pca_biplot(tou.pca)#Make a biplot of individuals and variables.
fviz_pca_var(tou.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
fviz_pca_var(tou.pca, alpha.var = "cos2")
fviz_pca_var(tou.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
#kmeans
set.seed(100)
tou.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(tou.km$cluster)
# Color variables by groups
fviz_pca_var(tou.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")
tou.desc <- dimdesc(tou.pca, axes = c(1,2), proba = 0.05)
#Description of dimension: identify the most significantly associated variables 
#sorted by p-value of the correlation
tou.desc$Dim.1
tou.desc$Dim.2
#individual results
ind<-get_pca_ind(tou.pca)
ind$coord
ind$cos2
ind$contrib
fviz_pca_ind(tou.pca,col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F # Avoid text overlapping (slow if many points)
)#individuals that are similar are grouped together on the plot
#individuals colored by groups
set.seed(100)
fviz_pca_ind(tou.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = TOU$Habitat, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
# Add confidence ellipses
fviz_pca_ind(tou.pca, geom.ind = "point", col.ind = TOU$Region, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07","#868686FF","green"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)
#biplot
fviz_pca_biplot(tou.pca, 
                col.ind = TOU$Habitat, 
                palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Habitat") 

#MDS
dist.mat<-dist(scale(TOU_pca,center=T,scale=T),
               method = "euclidean")#6 methods
mds.stuff<-cmdscale(dist.mat,#cmd=classical multi-dimensional scaling
                    eig = T,#eigenvalue
                    x.ret = T)
mds.var.per<-round(mds.stuff$eig/sum(mds.stuff$eig)*100,1)
mds.value<-mds.stuff$points
mds.data<-data.frame(Station=TOU$Station,
                     Region=TOU$Region,
                     Habitat=TOU$Habitat,
                     X=mds.value[,1],Y=mds.value[,2])

ggplot(data = mds.data,aes(x=X,y=Y,color=Habitat))+
  geom_text(aes(label=Station))+
  xlab(paste("MDS1-",mds.var.per[1],"%",sep=""))+
  ylab(paste("MDS2-",mds.var.per[2],"%",sep=""))+
  theme_bw()+
  ggtitle("MDS plot using Euclidean distance")

#nmds
TOU_nmds<-TOU[2:12]
TOU_nmds$Station<-as.numeric(as.factor(TOU$Station))
TOU_nmds$Habitat<-as.numeric(as.factor(TOU$Habitat))
TOU_nmds$Region<-as.numeric(as.factor(TOU$Region))
set.seed(100)
nmds<-metaMDS(TOU_nmds,distance = "bray",k=2)
stressplot(nmds,main = "Shepard plot")

nmds.df<-scores(nmds) %>% 
  as_tibble(rownames="samples")
TOU$samples<-as.character(c(1:nrow(TOU)))
nmds.df<-inner_join(nmds.df,TOU,by="samples")
colnames(nmds.df)
ggplot(data=nmds.df,aes(x=NMDS1,y=NMDS2,
                        color=Habitat))+
  geom_point()+
  stat_ellipse(geom = "polygon",aes(group=Habitat,fill=Habitat), alpha = 0.3) +
  annotate("text", x = -2, y = 0.95, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0)
