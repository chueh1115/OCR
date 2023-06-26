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
GPSC_DOU <- read_csv("GPSC_DOU.csv")

loc <- read_excel("GPSC_macro_sorting_2022.06.20.xlsx", sheet=1)
site<-loc %>% 
  group_by(Cruise, Station)%>%
  summarise(Latitude=mean(Latitude),
            Longitude=mean(Longitude))
GPSC_DOU<-full_join(GPSC_DOU,site,by=c("Cruise","Station")) %>% 
  filter(!is.na(.id))
GPSC_DOU$Station[is.na(GPSC_DOU$Longitude)]
GPSC_DOU$Latitude[GPSC_DOU$Station=="SC2a"]<-22.4519
GPSC_DOU$Longitude[GPSC_DOU$Station=="SC2a"]<-120.18591 #chech raw data
unique(GPSC_DOU$Station[GPSC_DOU$Habitat=="Shelf"])
#map
Station<-GPSC_DOU %>% 
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
#Diffusive Oxygen Uptake

#nmol/cm2/s -> mmol/m2/d
#1e4*3600*24/1e6 = 864
GPSC_DOU$In_situ_Integrated_Prod<-GPSC_DOU$In_situ_Integrated_Prod*864

DOU<-GPSC_DOU %>% 
  dplyr::select(c("Station","Latitude","Longitude","Habitat","Region","In_situ_Integrated_Prod")) 
library(vegan)
set.seed(100)

#check variance:PERMDISP####
#1 calculate distance
dou_dist<-vegdist(DOU$In_situ_Integrated_Prod,method = "euclidean")
#2 group->calculate data dispersion:Station
beta_station<-betadisper(dou_dist,group = DOU$Station)
#3 permutation grouping and calculate variance difference
permu_station<-permutest(beta_station,permutations = 9999)
plot(permu_station,hull=F,ellipse=T)

#2 group->calculate data dispersion:Region
beta_habitat<-betadisper(dou_dist,group = DOU$Habitat)
#3 permutation grouping and calculate variance difference
permu_habitat<-permutest(beta_habitat,permutations = 9999)
permu_habitat

#PERMANOVA####
permanova_dou<-adonis2(DOU$In_situ_Integrated_Prod~Habitat*Region,
                       data = DOU, permutations = 9999,
                       method = "euclidean",sqrt.dist = F)
permanova_dou
