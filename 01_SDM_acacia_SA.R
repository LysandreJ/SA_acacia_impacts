# Script for occurrence retrieving and SDM building for alien acacia species in South-Africa 

library(raster)
library(dplyr)
library(rgbif)
library(sf)
library(countrycode)
library(foreach)
library(doParallel)

#### INPUT DATA ####

# List of acacia species alien in South-Africa
acacia <- readRDS("Data/01_acacia_records.rds") 

# Administrative bondaries from https://data.humdata.org/dataset/global-international-boundaries-usgs
world_polygons <- read_sf("Data/shapefiles/adm0_polygons.shp") 

# Verified GBIF datasets from Botella et al. 2023
verif_datasets <- read.csv("Data/gbif_datasets.csv", sep =";")

# Native and introduced countries of each species from Botella et al. 2023
regions <- read.csv("Data/alien_acacia_countries_intro_natur.csv", sep ="\t") 

# Urban areas from http://due.esrin.esa.int/page_globcover.php
urb_area <- raster("Data/Globcover/GLOBCOVER_L4_200901_200912_V2.3.tif") 

# Climatic variables raster stack (bio1, bio2, bio12 and bio15) from https://chelsa-climate.org/
myExpl <- stack("Data/clim_stack/04_CHELSA_stack_1981-2010.tif")
names(myExpl) <- c('X1', 'X2', 'X3', 'X4')

# South-Africa boundaries from https://data.humdata.org/dataset/cod-ab-zaf
SA <- read_sf("Data/shapefiles/zaf_admbnda_adm0_sadb_ocha_20201109.shp")


#### Retrieve GBIF occurrences and filter them ####
cl <- makeCluster(2, type = "PSOCK") 
registerDoParallel(cl)
foreach(myRespName = acacia$Species[], .multicombine=TRUE,
        .packages = c("rgbif", "raster", "dplyr", "sf", "countrycode"), 
        .errorhandling = "pass") %dopar% {
          
          # Get corresponding presence/absence data by querying GBIF database 
          myResp <- try(occ_data(taxonKey=name_backbone(name = myRespName)$speciesKey,
                                 hasCoordinate = T,
                                 hasGeospatialIssue = F, 
                                 limit = 10000)$data[,c('decimalLongitude', 'decimalLatitude', "basisOfRecord",
                                                        "occurrenceStatus", "datasetKey")])
          if(inherits(myResp, "try-error"))next
          else {
            if(is.null(myResp)==T)next
            
            # Keep only presence and trustable occurrences
            myResp <- myResp[myResp$basisOfRecord %in%c("HUMAN_OBSERVATION", "LITERATURE ", "MACHINE_OBSERVATION ",
                                                        "OBSERVATION ", "OCCURRENCE")& myResp$occurrenceStatus=="PRESENT", ]
            if(nrow(myResp)<10) next
            myResp <- unique(myResp) 
            if(nrow(myResp)<10) next
            
            # Keep only one occurrence per 16km² pixel per specie 
            temp_rast<- myExpl$X1
            values(temp_rast) <- c(1:length(temp_rast))
            myResp$pix <- raster::extract(temp_rast, myResp[,1:2])
            if(nrow(myResp)<10) next
            myResp <- myResp %>% 
              group_by(pix) %>% 
              summarise(decimalLongitude = mean(decimalLongitude), decimalLatitude = mean(decimalLatitude), datasetKey = unique(datasetKey))
            myResp$presence <- rep(1, length(myResp$pix))
           
             # Keep occurences from verified datasets 
            myResp <- myResp[myResp$datasetKey %in% verif_datasets$datasetKey, ]
            
            # Keep occurences in countries with known introductions or native countries
            code <- countrycode(c(strsplit(regions[regions$Species==i,]$countries_intro, ","))[[1]], 'country.name', 'iso3c')
            if (i=="Acacia koa"){ # Adding missing countries for this species 
              code <- c(code, "USA")
            } else {
              code <- c(code, "AUS")
            }
            polygons <- world_polygons[world_polygons$iso_3%in% code,]
            polygons <- sf::st_make_valid(polygons)
            myResp_sf <- myResp %>%
              st_as_sf(
                coords = c("decimalLongitude", "decimalLatitude"),
                agr = "constant",
                crs = "+proj=longlat +datum=WGS84",
                stringsAsFactors = FALSE,
                remove = TRUE) %>%
              mutate(keep = NA)
            result <- st_join(myResp_sf, polygons, left = F)
            myResp_intro <- myResp[myResp$pix%in%result$pix, ]
            
            # Filtering urban areas 
            myResp$urb <- extract(urb_area, myResp_sf)
            myResp_intro$urb <- myResp[myResp$pix%in%myResp_intro$pix, ]$urb
            saveRDS(unique(myResp_intro[myResp_intro$urb!=190,c(1:3,5)]), paste0("Data/occ_data/", myRespName, "_acacia_no_urban_intro.rds", sep = ""))
          }
        }

stopCluster(cl)


#### SDM per species ####
for (myRespName in acacia$Species){
  
  # Load previously filtered occurrences for one species
  myResp <- try(readRDS(paste0("Data/occ_data/", myRespName, "_acacia_no_urban_intro.rds", sep="")))
  if(inherits(myResp, "try-error")) next

  # Get corresponding XY coordinates
  myRespXY <- myResp[, c('decimalLongitude', 'decimalLatitude')]
  n_pres <- length(myResp$presence)
  
  if(n_pres<10) next
  
  if (n_pres < 100) {
    # Format data with pseudo-absences : sre method, 100 pseudo-absences, 10 repetitions
    PA <- BIOMOD_FormatingdDta(resp.var = myResp$presence,
                               expl.var = myExpl,
                               resp.xy = myRespXY,
                               resp.name = myRespName,
                               PA.nb.rep = 10,
                               PA.nb.absences = 100,
                               PA.strategy = 'sre',
                               na.rm = T)
    
    CV_block <- BIOMOD_CrossValidation(PA, do.stratification = T, method = "x",
                                       balance = "presences", do.full.models = F, nb.rep = 1, k=4)
  } else {
    # Format data with pseudo-absences : sre method, n pseudo_abs = n presence, 10 repetitions
    PA <- BIOMOD_Formatingdata(resp.var = myResp$presence,
                               expl.var = myExpl,
                               resp.xy = myRespXY,
                               resp.name = myRespName,
                               PA.nb.rep = 10,
                               PA.nb.absences = n_pres,
                               PA.strategy = 'sre',
                               na.rm = T)
    
    CV_block <- BIOMOD_CrossValidation(PA, do.stratification = T, method = "x",
                                       balance = "presences", do.full.models = F, nb.rep = 1, k=4)
  }
  
  # Sub-model computation :
  # Modelling with RandomForest algo. 3 blocks train, 1 block test, Classification model (presence/absence),
  # 500 trees, nodesize = 5, mtry = sqrt(number of Expl var)
  # 3 permutations to estimate variables importance
  # evaluation with KAPPA and TSS
  myBiomodModelOut <- try(BIOMOD_Modeling(bm.format =  PA,
                                          models = 'RF',
                                          modeling.id = 'model_no_urban',
                                          data.split.table = CV_block,
                                          var.import = 3,
                                          metric.eval = c("KAPPA", "TSS"),
                                          do.full.models = FALSE,
                                          save.output = T))
  if(inherits(myBiomodModelOut, "try-error")) next
  
  # Ensemble model :
  # Based on the 40 sub-models, keeping only those with Kappa & TSS > 0.7
  # Ensemble probability computed using committee averaging
  myBiomodEM <- BIOMOD_EnsembleModeling(myBiomodModelOut,
                                        models.chosen = 'all',
                                        var.import = 3,
                                        metric.eval = c("KAPPA", "TSS"),
                                        metric.select.thresh = c(0.7, 0.7),
                                        prob.mean = F,
                                        committee.averaging = TRUE,
                                        prob.mean.weight = F,
                                        em.by = "all")
  
  # Project ensemble model on current and future climate in South-Africa
  for (time in c("1981-2010", "2041-2070", "2071-2100")){
    if (time == "1981-2010"){
      SA_rast <- stack(crop(mask(myExpl, mask = SA), extent(SA)))
      myBiomodEMProj <- BIOMOD_EnsembleForecasting(myBiomodEM,
                                                   proj.name = paste("model_no_urban_", time, sep=""),
                                                   new.env = SA_rast,
                                                   models.chosen = "all" )
    } else {
      for (model in c("gfdl-esm4", "ipsl-cm6a-lr", "mpi-esm1-2-hr", "mri-esm2-0", "ukesm1-0-ll")){
        for (ssp in c("ssp126", "ssp370", "ssp585")){
          # For future climate, one projection for each socio-economic pathway of each global climate model
          future <- stack(paste("data/04_CHELSA_stack", time, model, ssp, ".tif", sep="_"))
          SA_rast <- stack(crop(mask(future, mask = SA), extent(SA)))
          names(SA_rast)<- c('X1', 'X2', 'X3', 'X4')
          myBiomodEMProj <- BIOMOD_EnsembleForecasting(myBiomodEM,
                                                       proj.name = paste("model_no_urban", time, model, ssp, sep="_"),
                                                       new.env = SA_rast,
                                                       models.chosen = "all" )
        }
      }
    }
  }
}

