# by Christophe Botella

library(raster)
library(dplyr)
library(sf)
require(ggplot2)
require(viridis)
library(stringr)
#library(countrycode)
#library(foreach)
#library(doParallel)

dir="C:/Users/user/pCloud local/boulot/data/Future acacia impacts SA/future_Acacia_impacts_SA/script_R_aggregation_impact_occurence"

#### INPUT DATA ####
setwd(dir)

# List of acacia species alien in South-Africa
acacia <- readRDS("data/01_acacia_records.rds") 

# Records in SA
botella = read.csv('data/acaciaSA_forSab.csv',sep=";",header=T)

# Impact score (max and mean per species)
eicat <- read.csv("output/R_data_eicat_mean_max.csv", header = T, sep = ";", dec = ",")
eicat$species_name <- str_to_title(eicat$species_name)
eicat$species=gsub(pattern = '\\.',replacement =' ',x = eicat$species_name)
  
# Climatic variables raster stack (bio1, bio2, bio12 and bio15) from https://chelsa-climate.org/
rasto <- stack("data/clim_stack/04_CHELSA_stack_1981-2010.tif")
rastoSA=crop(rasto,raster::extent(c(15,37,-37,-20)))[[1]]

# South-Africa boundaries from https://data.humdata.org/dataset/cod-ab-zaf
SA <- read_sf("data/South_Africa/zaf_admbnda_adm0_sadb_ocha_20201109.shp")

# Get raster of cells in SA
masked=raster::mask(rastoSA,mask=SA)
masked[!is.na(masked[])]=1:sum(!is.na(masked[]))

botella$cellId=raster::extract(masked,botella[,c('longitude','latitude')])

# Make observed presence raster per species
sps=unique(botella$species[!is.na(botella$cellId) & botella$species%in%acacia$Species]);sps=sps[order(sps)]
observedSA=list()
templato=rasterToPoints(masked);colnames(templato)[3]='cellId'
for(sp in sps){
  spObs=botella%>%
    filter(species==sp & !is.na(cellId))%>%
    merge(templato,by="cellId",all.x=T)%>%
    group_by(cellId,x,y)%>%
    summarise()
  tmp=0.*masked
  tmp[masked[]%in%spObs$cellId]=1
  observedSA[[length(observedSA)+1]]=tmp
  names(observedSA)[length(observedSA)]=sp
}
save(observedSA,file="output/observed_SA/observedSA_per_species.RData")

totalImpact=0.*masked
obsRichness=0.*masked
for(sp in sps){
  tmp=observedSA[[which(sps==sp)]][[1]]
  toPlot=rasterToPoints(tmp);colnames(toPlot)[3]='pres'
  toPlot=toPlot%>%
    as.data.frame()%>%
    mutate(observed=ifelse(pres==1,'True','False'))
  score=eicat$max_eicat[eicat$species==sp]
  
  p=ggplot()+
    geom_tile(data=toPlot,aes(x=x,y=y,fill=observed))+
    scale_fill_manual(values=c('white','chartreuse4'))+
    xlab('Longitude')+ylab('Latitude')+
    theme(text=element_text(size=30))
  png(paste0('output/observed_SA/per species/presence_',sp,'.png'),height=1200,width=1600)
  print(p)
  dev.off()
  
  if(length(score)>0){totalImpact=totalImpact+score*tmp}
  obsRichness=obsRichness+tmp
}

# Observed richness
toPlot=as.data.frame(rasterToPoints(obsRichness));colnames(toPlot)[3]='rich'
p=ggplot()+
  geom_tile(data=toPlot,aes(x=x,y=y,fill=rich))+
  scale_fill_viridis('Observed richness') +
  xlab('Longitude')+ylab('Latitude')+
  theme_bw()+
  theme(text=element_text(size=30))
png('output/observed_SA/richness.png',height=1200,width=1600)
print(p)
dev.off()

# max Impact of observed species
toPlot=as.data.frame(rasterToPoints(totalImpact));colnames(toPlot)[3]='Impact'
p=ggplot()+
  geom_tile(data=toPlot,aes(x=x,y=y,fill=Impact))+
  scale_fill_viridis('max. impact of \n observed species') +
  xlab('Longitude')+ylab('Latitude')+
  theme_bw()+
  theme(text=element_text(size=30))
png('output/observed_SA/maxImpact.png',height=1200,width=1600)
print(p)
dev.off()
