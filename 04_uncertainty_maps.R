# Uncertainty maps
library(raster)
library(dplyr)
library(sf)
require(ggplot2)
require(viridis)
library(stringr)

dir="C:/Users/user/pCloud local/boulot/data/Future acacia impacts SA/future_Acacia_impacts_SA/script_R_aggregation_impact_occurence"
dir="/home/cbotella/pcloud/boulot/data/Future acacia impacts SA/future_Acacia_impacts_SA/script_R_aggregation_impact_occurence"
setwd(dir)

####Functions
multiplot <- function(plots=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  numPlots = length(plots)
  print(numPlots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#### INPUT DATA ####
stackMid=stack('output/stack_mean_sd_per_species/sd_stack_mid.tif')
stackEnd=stack('output/stack_mean_sd_per_species/sd_stack_end.tif')
names(stackMid)

# Impact score (max and mean per species)
eicat <- read.csv("output/R_data_eicat_mean_max.csv", header = T, sep = ";", dec = ",")
eicat$species_name <- str_to_title(eicat$species_name)
eicat$species=gsub(pattern = '\\.',replacement =' ',x = eicat$species_name)
rownames(eicat)=eicat$species_name
names(stackMid)=sapply(strsplit(names(stackMid),split="_"),function(el)el[2])
names(stackEnd)=sapply(strsplit(names(stackEnd),split="_"),function(el)el[2])

#### SD Impact score Mid/End-Century
pImp=lapply(list(stackMid,stackEnd),function(stacko){
  impSD=0*stacko[[1]]+1e-14
  for(sp in eicat$species_name[eicat$max_eicat>0]){
    impSD=impSD+eicat[sp,'max_eicat']^2*(stacko[[which(names(stacko)==sp)]]/1000)^2
  }
  impSD=sqrt(impSD)
  
  toPlot=as.data.frame(rasterToPoints(impSD));colnames(toPlot)[3]='SD_Impact'
  p=ggplot()+
    geom_tile(data=toPlot,aes(x=x,y=y,fill=SD_Impact))+
    scale_fill_viridis('S.D. of \n projected \n impact',limits=c(0,3.996594)) +
    xlab('Longitude')+ylab('Latitude')+
    theme_bw()+
    theme(text=element_text(size=30))
  return(p)
})

png('output/observed_SA/maxImpact.png',height=1200,width=1600)
print(pImp[[2]])
dev.off()

#### SD Richness Impact score End-Century
pRich=lapply(list(stackMid,stackEnd),function(stacko){
  rich=0*stacko[[1]]+1e-14
  for(sp in names(stacko)){
    rich=rich+(stacko[[which(names(stacko)==sp)]]/1000)^2
  }
  rich=sqrt(rich)
  
  toPlot=as.data.frame(rasterToPoints(rich));colnames(toPlot)[3]='SD_Rich'
  p=ggplot()+
    geom_tile(data=toPlot,aes(x=x,y=y,fill=SD_Rich))+
    scale_fill_viridis('S.D. of \n projected \n richness',limits=c(0,1.8837905)) +
    xlab('Longitude')+ylab('Latitude')+
    theme_bw()+
    theme(text=element_text(size=30))
  return(p)
})



png('output/uncertainty_maps.png',height=1200,width=1600)
multiplot(c(pRich,pImp),cols = 2)
dev.off()
