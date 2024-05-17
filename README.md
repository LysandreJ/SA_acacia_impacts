# Mapping impacts of alien species

Scripts and data for reproducing the results obtained by Kumschichk et al. in the paper **Mapping impacts of alien species on biodiversity in the face of climate change**.


## Description of the Data
All information related to data are detailed in `Data/README.md` (TO BE DONE). 

## How to use the scripts
The script `01_SDM_acacia_SA.R` retrieves and filters occurence data from GBIF and performs Species Distribution Models for each of the species. `02_impact_per_species.R` aggregates the EICAT impact score per species. `03_map_predicted_richness_and_impact.R` and `04_uncertainty_maps.R` plot cumulative probability of presence and associated projected impacts (Fig. 2) and uncertainty maps (Fig. S3). `05_map_observed_richness_and_impact.R` plots maximum recorded impacts (Fig. 3).

