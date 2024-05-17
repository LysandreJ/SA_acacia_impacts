# Objective: Aggregation of eicat values (max and mean) to the different models of current and future distribution of Acacia in South Africa. 
# Name: Océane Boulesnane Guengant
# Date: 16/12/2022

################################## LIBRARY #####################################
install.packages("stringr") # For the function str_to_title
install.packages("sf") # For the function read_sf
install.packages("raster") # For thr function rast
install.packages("ggplot2") # For the function ggplot
install.packages("viridis") # For the function scale_fill_viridis


library(stringr)
library(sf)
library(raster)
library(ggplot2)
library(viridis)

dir="C:/Users/user/pCloud local/boulot/data/Future acacia impacts SA/future_Acacia_impacts_SA/script_R_aggregation_impact_occurence/"
setwd(dir)

# Function to call
plot_final <- function(donnees) {
	tot_rast_df <- as.data.frame(as(donnees, "SpatialPixelsDataFrame"))
	colnames(tot_rast_df) <- c("value", "x", "y")
	ggp <- ggplot(tot_rast_df) + 
		geom_tile(aes(x = x, y = y, fill = value), alpha = 1) + 
		geom_sf(data = South_africa$geometry, color = "black", fill = NA, alpha = 0.7) +
		scale_fill_viridis() +
		xlab("")+
		ylab("")+
		theme_bw()
	print(ggp)
	print("done")
}


############################# IMPORTATION DATA #################################

# Impact score (max and mean per species)
eicat <- read.csv("output/R_data_eicat_mean_max.csv", header = T, sep = ";", dec = ",")
eicat$species_name <- str_to_title(eicat$species_name)

# South Africa map
South_africa <- read_sf("data/South_Africa/zaf_admbnda_adm0_sadb_ocha_20201109.shp")

# Stack of the current distribution
stack_current <- stack("data/SA_proj_stack/SA_proj_stack_1981_2010.tif")

if(F){
  for(i in 1:length(names(stack_current))){
    png(paste0('C:/Users/user/Documents/Downloads/test_acacia/',names(stack_current)[i],'.png'),height=500,width=500)
    plot(stack_current[[i]])
    dev.off()
  }
}

# Stack of the potential distribution at mid-century
stack_mid <- stack("output/stack_mean_sd_per_species/mean_stack_mid.tif")
stack_mid_sd <- stack("output/stack_mean_sd_per_species/sd_stack_mid.tif")

# Stack of the potential distribution at end-century
stack_end <- stack("output/stack_mean_sd_per_species/mean_stack_end.tif")
stack_end_sd <- stack("output/stack_mean_sd_per_species/sd_stack_end.tif")

#####
# Species richness maps
#####


multiplot <- function(plots=NULL, file, cols=1, layout=NULL){
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

maxRich=25 # over all periods and cells

pList=lapply(c('current','mid','end'),function(period){
  eval(parse(text=paste0('stacko=stack_',period)))
  toPlot=stacko[[1]]
  for(i in 2:length(names(stacko))){toPlot=toPlot+stacko[[i]]}
  toPlot=toPlot/1000
  df=as.data.frame(rasterToPoints(toPlot))
  max(df$layer)
  if(period=="current"){
    titlo="Current (1981-2010)"
  }else if(period=="mid"){
    titlo="Mid-century (2041-2070)"
  }else if(period=="end"){
    titlo="End-century (2071-2100)"
  }
  p = ggplot() + 
    geom_tile(data=df,aes(x = x, y = y, fill = layer), alpha = 1) + 
    geom_sf(data = South_africa$geometry, color = "black", fill = NA, alpha = 0.7) +
    scale_fill_viridis('Predicted \n richness',begin = min(df$layer),end=max(df$layer)/maxRich)+
    xlab("Longitude")+
    ylab("Latitude")+
    theme_bw()+
    theme(text=element_text(size=35),
          legend.key.size = unit(2, 'cm'))
  return(p)})

png(paste0('output/mean_richness_map_allperiods.png'),height=1800,width=3000)
multiplot(pList,cols=3)
dev.off()
    
png(paste0('output/mean_richness_map_',period,'.png'),height=2000,width=1500)
print(p)
dev.off()

#####
# Agregate Impact score 
#####

pListImpact=lapply(c('current','mid','end'),function(period){
  eval(parse(text=paste0('stacko=stack_',period)))
  namo=sapply(strsplit(names(stacko),split=".(\\d)(\\d)(\\d)(\\d)_"),function(el)el[2])
  for(i in 1:length(names(stacko))){
    tmp=eicat$max_eicat[eicat$species_name==namo[i]]*(stacko[[i]]>500)
    if(i==1){toPlot=tmp}else{toPlot=toPlot+tmp}
  }
  df=as.data.frame(rasterToPoints(toPlot))
  max(df$layer)
  p = ggplot() + 
    geom_tile(data=df,aes(x = x, y = y, fill = layer), alpha = 1) + 
    geom_sf(data = South_africa$geometry, color = "black", fill = NA, alpha = 0.7) +
    scale_fill_viridis('Projected \n Impact',begin = min(df$layer),end=max(df$layer)/29)+
    xlab("Longitude")+
    ylab("Latitude")+
    theme_bw()+
    theme(text=element_text(size=35),
          legend.key.size = unit(2, 'cm'))
  return(p)})

print(pListImpact[[1]])

png(paste0('output/map_rich_impact_allperiods.png'),height=3000,width=2000)
multiplot(c(pList,pListImpact),cols=2)
dev.off()



pListWeightedImpact=lapply(c('current','mid','end'),function(period){
  eval(parse(text=paste0('stacko=stack_',period)))
  namo=sapply(strsplit(names(stacko),split=".(\\d)(\\d)(\\d)(\\d)_"),function(el)el[2])
  for(i in 1:length(names(stacko))){
    tmp=eicat$max_eicat[eicat$species_name==namo[i]]*(stacko[[i]]/1000)
    if(i==1){toPlot=tmp}else{toPlot=toPlot+tmp}
  }
  df=as.data.frame(rasterToPoints(toPlot))
  max(df$layer)
  p = ggplot() + 
    geom_tile(data=df,aes(x = x, y = y, fill = layer), alpha = 1) + 
    geom_sf(data = South_africa$geometry, color = "black", fill = NA, alpha = 0.7) +
    scale_fill_viridis('Projected \n Impact',begin = min(df$layer),end=max(df$layer)/29)+
    xlab("Longitude")+
    ylab("Latitude")+
    theme_bw()+
    theme(text=element_text(size=35),
          legend.key.size = unit(2, 'cm'))
  return(p)})

print(pListImpact[[1]])

png(paste0('output/map_rich_Wimpact_allperiods.png'),height=3000,width=2000)
multiplot(c(pList,pListWeightedImpact),cols=2)
dev.off()

### Save (weighted) impact score raster per time horizon
for(period in c('current','mid','end')){
  eval(parse(text=paste0('stacko=stack_',period)))
  namo=sapply(strsplit(names(stacko),split=".(\\d)(\\d)(\\d)(\\d)_"),function(el)el[2])
  for(i in 1:length(names(stacko))){
    tmp=eicat$max_eicat[eicat$species_name==namo[i]]*(stacko[[i]]/1000)
    if(i==1){outputRaster=tmp}else{outputRaster=outputRaster+tmp}
  }
  writeRaster(outputRaster,filename = paste0("output/proj_impact_rasters/potential_impact_score_",period,".tif"),overwrite=TRUE)
}

# With DD = 0 #############################

########################## CURRENT DISTRIBUTION ################################

# Without taking into account the impact ----------------------------------
tot_rast <- sum(stack_current)/1000
plot_final(tot_rast) # Species richness

# dev.print(png, "docs/DD = 0/current_climate/Species_richness_Acacia_current_climate.jpg", width = 895)

# Taking into account the max impact per species --------------------------

stack_current_max <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_current[[paste("proj_1981.2010_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$max_eicat
	stack_current_max <- stack(stack_current_max, rast)
}

# Maximum of the maximum score per species

tot_rast_max_max <- max(stack_current_max)/1000 
plot_final(tot_rast_max_max)

# dev.print(png, "docs/DD = 0/current_climate/Max of the impact max at current century.jpg", width = 895)


# Sum of the maximum score per species

tot_rast_sum_max <- sum(stack_current_max)/1000
plot_T <- plot_final(tot_rast_sum_max)

# dev.print(png, "docs/DD = 0/current_climate/Sum of the impact max at current century.jpg", width = 895)

# Taking into account the mean impact per species -------------------------
stack_current_mean <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_current[[paste("proj_1981.2010_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$mean_eicat
	stack_current_mean <- stack(stack_current_mean, rast)
}

# Maximum of the mean score per species
tot_rast_max_mean <- max(stack_current_mean)/1000 
plot_final(tot_rast_max_mean)

# dev.print(png, "docs/DD = 0/current_climate/Max of the impact mean at current century.jpg", width = 895)


# Sum of the mean score per species
tot_rast_sum_mean <- sum(stack_current_mean)/1000
plot_final(tot_rast_sum_mean)

dev.print(png, "docs/DD = 0/current_climate/Sum of the impact mean at current century.jpg", width = 895)


######################### MID-CENTURY DISTRIBUTION #############################

# Without taking into account the impact ----------------------------------
# Species richness
sp_rich_mid <- sum(stack_mid)/1000
plot_T <- plot_final(sp_rich_mid)
# plot
# dev.print(png, "docs/DD = 0/mid_century/Species richness of Acacia at mid-century.jpg", width = 895)

# Standard deviation
sd_mid <- mean(stack_mid_sd)/100 # Calculate the mean of the species standard deviation
plot_T <- plot_final(sd_mid)

# dev.print(png, "docs/DD = 0/mid_century/Standard deviation of Acacia at mid-century.jpg", width = 895)


# Taking into account the max impact per species --------------------------

stack_mid_max <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_mid[[paste("X2041.2070_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$max_eicat
	stack_mid_max <- stack(stack_mid_max, rast)
}

# Maximum of the maximum score per species

stack_mid_max_max <- max(stack_mid_max)/1000 
plot_final(stack_mid_max_max)
# dev.print(png, "docs/DD = 0/mid_century/Max of the impact max at mid-century.jpg", width = 895)


# Sum of the maximum score per species

stack_mid_sum_max <- sum(stack_mid_max)/1000
plot_final(stack_mid_sum_max)

dev.print(png, "docs/DD = 0/mid_century/Sum of the impact max at mid-century.jpg", width = 895)


# Taking into account the mean impact per species -------------------------

stack_mid_mean <- raster()

for (i in eicat$species_name){
	print(i)
	rast <- stack_mid[[paste("X2041.2070_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$mean_eicat
	stack_mid_mean <- stack(stack_mid_mean, rast)
}

# Maximum of the mean score per species

stack_mid_max_mean <- max(stack_mid_mean)/1000 
plot_final(stack_mid_max_mean)

# dev.print(png, "docs/DD = 0/mid_century/Max of the impact mean at mid-century.jpg", width = 895)


# Sum of the mean score per species

stack_mid_sum_mean <- sum(stack_mid_mean)/1000
plot_final(stack_mid_sum_mean)

# dev.print(png, "docs/DD = 0/mid_century/Sum of the impact mean at mid-century.jpg", width = 895)


######################### END-CENTURY DISTRIBUTION #############################

# Without taking into account the impact ----------------------------------
# Species richness
sp_rich_end <- sum(stack_end)/1000
plot_final(sp_rich_end)
dev.print(png, "docs/DD = 0/end_century/Species richness of Acacia in end-century.jpg", width = 895)


# Standard deviation
sd_end <- mean(stack_end_sd)/100 # Calculate the mean of the species standard deviation
plot_final(sd_end)
# dev.print(png, "docs/DD = 0/end_century/Standard deviation of Acacia distribution in end-century.jpg", width = 895)


# Taking into account the max impact per species --------------------------

stack_end_max <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_end[[paste("X2071.2100_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$max_eicat
	stack_end_max <- stack(stack_end_max, rast)
}

# Maximum of the maximum score per species

stack_end_max_max <- max(stack_end_max)/1000 
plot_final(stack_end_max_max)

# dev.print(png, "docs/DD = 0/end_century/Max of the impact max at end-century.jpg", width = 895)

# Sum of the maximum score per species

stack_end_sum_max <- sum(stack_end_max)/1000
plot_final(stack_end_sum_max)

# dev.print(png, "docs/DD = 0/end_century/Sum of the impact max at end-century.jpg", width = 895)

# Taking into account the mean impact per species -------------------------

stack_end_mean <- raster()

for (i in eicat$species_name){
	print(i)
	rast <- stack_end[[paste("X2071.2100_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$mean_eicat
	stack_end_mean <- stack(stack_end_mean, rast)
}

# Maximum of the mean score per species

stack_end_max_mean <- max(stack_end_mean)/1000 
plot_final(stack_end_max_mean)

# dev.print(png, "docs/DD = 0/end_century/Max of the impact mean at end-century.jpg", width = 895)


# Sum of the mean score per species

stack_end_sum_mean <- sum(stack_end_mean)/1000
plot_final(stack_end_sum_mean)

# dev.print(png, "docs/DD = 0/end_century/Sum of the impact mean at end-century.jpg", width = 895)


# With DD = NA #############################

########################## CURRENT DISTRIBUTION ################################

# Taking into account the max impact per species --------------------------

stack_current_max_DDNA <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_current[[paste("proj_1981.2010_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$max_eicat_DDNA
	stack_current_max_DDNA <- stack(stack_current_max_DDNA, rast)
}

# Maximum of the maximum score per species

tot_rast_max_max_DDNA <- max(stack_current_max_DDNA)/1000 
plot_final(tot_rast_max_max_DDNA)

# dev.print(png, "docs/DD = NA/current_climate/Max of the impact max at current climate.jpg", width = 895)

# Sum of the maximum score per species

tot_rast_sum_max <- sum(stack_current_max)/1000
plot_final(tot_rast_sum_max)

# dev.print(png, "docs/DD = NA/current_climate/Sum of the impact max at current climate.jpg", width = 895)

# Taking into account the mean impact per species -------------------------
stack_current_mean_DDNA <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_current[[paste("proj_1981.2010_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$mean_eicat_DDNA
	stack_current_mean_DDNA <- stack(stack_current_mean_DDNA, rast)
}

# Maximum of the mean score per species
tot_rast_max_mean_DDNA <- max(stack_current_mean_DDNA)/1000 
plot_final(tot_rast_max_mean_DDNA)

# dev.print(png, "docs/DD = NA/current_climate/Max of the impact mean at current climate.jpg", width = 895)

# Sum of the mean score per species
tot_rast_sum_mean_DDNA <- sum(stack_current_mean_DDNA)/1000
plot_final(tot_rast_sum_mean_DDNA)

# dev.print(png, "docs/DD = NA/current_climate/Sum of the impact mean at current climate.jpg", width = 895)



######################### MID-CENTURY DISTRIBUTION #############################

# Taking into account the max impact per species --------------------------

stack_mid_max_DDNA <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_mid[[paste("X2041.2070_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$max_eicat_DDNA
	stack_mid_max_DDNA <- stack(stack_mid_max_DDNA, rast)
}

# Maximum of the maximum score per species

stack_mid_max_max_DDNA <- max(stack_mid_max_DDNA)/1000 
plot_final(stack_mid_max_max_DDNA)

# dev.print(png, "docs/DD = NA/mid_century/Max of the impact max at mid-century.jpg", width = 895)

# Sum of the maximum score per species

stack_mid_sum_max_DDNA <- sum(stack_mid_max_DDNA)/1000
plot_final(stack_mid_sum_max_DDNA)

# dev.print(png, "docs/DD = NA/mid_century/Sum of the impact max at mid-century.jpg", width = 895)


# Taking into account the mean impact per species -------------------------

stack_mid_mean_DDNA <- raster()

for (i in eicat$species_name){
	print(i)
	rast <- stack_mid[[paste("X2041.2070_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$mean_eicat_DDNA
	stack_mid_mean_DDNA <- stack(stack_mid_mean_DDNA, rast)
}

# Maximum of the mean score per species

stack_mid_max_mean_DDNA <- max(stack_mid_mean_DDNA)/1000 
plot_final(stack_mid_max_mean_DDNA)

# dev.print(png, "docs/DD = NA/mid_century/Max of the impact mean at mid-century.jpg", width = 895)

# Sum of the mean score per species

stack_mid_sum_mean_DDNA <- sum(stack_mid_mean_DDNA)/1000
plot_final(stack_mid_sum_mean_DDNA)

# dev.print(png, "docs/DD = NA/mid_century/Sum of the impact mean at mid-century.jpg", width = 895)


######################### END-CENTURY DISTRIBUTION #############################

# Taking into account the max impact per species --------------------------

stack_end_max_DDNA <- raster()
for (i in eicat$species_name){
	print(i)
	rast <- stack_end[[paste("X2071.2100_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$max_eicat_DDNA
	stack_end_max_DDNA <- stack(stack_end_max_DDNA, rast)
}

# Maximum of the maximum score per species

stack_end_max_max_DDNA <- max(stack_end_max_DDNA)/1000 
plot_final(stack_end_max_max_DDNA)

# dev.print(png, "docs/DD = NA/end_century/Max of the impact max at end-century.jpg", width = 895)


# Sum of the maximum score per species

stack_end_sum_max_DDNA <- sum(stack_end_max_DDNA)/1000
plot_final(stack_end_sum_max_DDNA)

# dev.print(png, "docs/DD = NA/end_century/Sum of the impact max at end-century.jpg", width = 895)


# Taking into account the mean impact per species -------------------------

stack_end_mean_DDNA <- raster()

for (i in eicat$species_name){
	print(i)
	rast <- stack_end[[paste("X2071.2100_", i, sep = "")]]
	rast <- rast*eicat[eicat$species_name==i,]$mean_eicat_DDNA
	stack_end_mean_DDNA <- stack(stack_end_mean_DDNA, rast)
}

# Maximum of the mean score per species

stack_end_max_mean_DDNA <- max(stack_end_mean_DDNA)/1000 
plot_final(stack_end_max_mean_DDNA)

# dev.print(png, "docs/DD = NA/end_century/Max of the impact mean at end-century.jpg", width = 895)


# Sum of the mean score per species

stack_end_sum_mean_DDNA <- sum(stack_end_mean_DDNA)/1000
plot_final(stack_end_sum_mean_DDNA)

# dev.print(png, "docs/DD = NA/end_century/Sum of the impact mean at end-century.jpg", width = 895)


