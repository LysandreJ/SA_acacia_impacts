# Objectif: Preparation of the aggregate impact score per species's data. Including the maximum recorded impact per species and the mean impact per species, with DD = 0 and DD = 1
# Name: Océane Boulesnane-Guengant
# Date: 14/12/2022

################################ LIBRARY #######################################
install.packages("readxl")	# for the function "read_xlsx"
install.packages("tidyr")		# for the function "%>%"
install.packages("dplyr")		# for the function "select"
install.packages("stringr")	# for the function "str_to_lower"
install.packages("reshape2")# for the function "dcast"
install.packages("pastecs") # for the function "stat.desc"
install.packages("ggplot2")

library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(reshape2)
library(pastecs)
library(ggplot2)

################################ IMPORATAION ###################################

# Importation -------------------------------------------------------------

data.distrib <- read.csv("data/acaciaSA_forSab.csv", header = T, sep = ";", dec = ".")
data.eicat <-  read_xlsx("data/220817_EICAT Acacias global.xlsx", sheet = "EICAT", na = c("NA", ""))

# Cleaning ---------------------------------------------------------------

# title in lower 
names(data.distrib) <- str_to_lower(names(data.distrib))
names(data.eicat) <- str_to_lower(names(data.eicat))

# In the original data: the title have a paragraph break
# names(data.eicat)[2] <- "alien_species_scientific_name" 
# names(data.eicat)[4] <- "country_of_impact"

################################ DATA EICAT ###################################

# Selection of columns --------------------------------------------------

data_col <- as.data.frame(data.eicat %>% 
                            select(Alien_species_scientific_name, 
                                   EICAT_impact_category, 
                                   Present_in_South_Africa))

# Data cleanup ------------------------------------------------------------

# Rename columns
colnames(data_col) <- c("species_name", "eicat", "present_SA")
data_col$species_name <- str_to_lower(data_col$species_name)
data_col$species_name <- str_replace_all(data_col$species_name, " ", ".") # Same format as the distribution rasters according to the climate (made by Lysandre Journiac)

# Delete species that are not present in South Africa (will not be in the distribution model)
data_col <- data_col[data_col$present_SA != "N", ]

tmp=unique(data_col$species_name)

# Delete Acacia crassiuscula, A. acuminata and A. koa because its distribution have not been realized (not enought data)
data_col <- data_col[data_col$species_name != "acacia.crassiuscula", ]
data_col <- data_col[data_col$species_name != "acacia.acuminata", ]
data_col <- data_col[data_col$species_name != "acacia.koa", ]

# Delete the column present in South Africa
data_col <- select(data_col, -present_SA)

# Transform eicat score into numerical values
data_col$eicat <- factor(data_col$eicat)			# CASE DD = 0 
data_col$eicat_DDNA <- factor(data_col$eicat)	# CASE DD = NA

levels(data_col$eicat) <- c(0, 0, 1, 2, 3)		# DD = 0 
data_col$eicat <- as.character(data_col$eicat) 
data_col$eicat <- as.numeric(data_col$eicat) # Need to go through the character step because otherwise the 0s are put in 1

levels(data_col$eicat_DDNA) <- c(NA, 0, 1, 2, 3) # DD = NA
data_col$eicat_DDNA <- as.character(data_col$eicat_DDNA) 
data_col$eicat_DDNA <- as.numeric(data_col$eicat_DDNA)

# Look for the NA
summary(data_col) # 53 NAs => 53 DDs
str(data_col) 

# data by species ---------------------------------------------------------

# Maximum eicat score by species

	# For DD = 0
data_sp_1 <- data_col %>% 
		group_by(species_name) %>%
		summarise_at(vars(eicat), funs(max(eicat, na.rm = T)))
names(data_sp_1)[2] <- "max_eicat"
	
data_sp_1$max_eicat_cat <- factor(data_sp_1$max_eicat)
levels(data_sp_1$max_eicat_cat) <- c("DD/MC","MN", "MR")

	# For DD = NA
data_sp_1.1 <- data_col %>% 
	group_by(species_name) %>%
	summarise_at(vars(eicat_DDNA), funs(max(eicat_DDNA, na.rm = T)))
names(data_sp_1.1)[2] <- "max_eicat_DDNA"

# Transform -Inf into NA
data_sp_1.1[data_sp_1.1 == "-Inf"] <- NA

data_sp_1.1$max_eicat_cat_DDNA <- factor(data_sp_1.1$max_eicat_DDNA)
levels(data_sp_1.1$max_eicat_cat_DDNA) <- c("MN", "MR")

# Mean eicat score by species

	# For DD = 0
data_sp_2 <- data_col %>%
		group_by(species_name) %>%
		summarise_at(vars(eicat), funs(mean(eicat)))
names(data_sp_2)[2] <- "mean_eicat"

	# For DD = NA
# Supprimer les lignes NA 
data_col_1 <- select(data_col, - eicat)
data_col_SS_NA <- drop_na(data_col_1)

data_sp_2.1 <- data_col_SS_NA %>%
	group_by(species_name) %>%
	summarise_at(vars(eicat_DDNA), funs(mean(eicat_DDNA)))
names(data_sp_2.1)[2] <- "mean_eicat_DDNA"

# Look for the missing species (must be added with mean = DD = NA)
missing_sp <- setdiff(data_sp_2$species_name, data_sp_2.1$species_name)

missing_sp <- as.data.frame(missing_sp)
names(missing_sp) <- "species_name"
missing_sp$mean_eicat_DDNA <- NA
data_sp_2.1 <- rbind(data_sp_2.1, missing_sp)

	# Merging		
eicat <- merge(data_sp_1, data_sp_2, by = "species_name")
eicat <- merge(eicat, data_sp_1.1, by = "species_name")
eicat <- merge(eicat, data_sp_2.1, by = "species_name")


eicat
summary(eicat)
str(eicat)


############################ COMPARAISON TEST ###############################
# Analysis on paired data as we want to compared if the maximum and sum methods are different. Both are on the same "topic"

# Between the mean and the mac with DD = 0 --------------------------------

# Hypothesis verification 

# For paired data
eicat.1 <- eicat %>% mutate(diffDD0 = max_eicat-mean_eicat) # For DD = 0
eicat.1 <- eicat.1 %>% mutate(diffDDNA = max_eicat_DDNA-mean_eicat_DDNA) # For DD = NA

	# Normality
shapiro.test(eicat.1$diffDD0) # p < 0.05: null hypothesis (follows normal distribution) is rejected
qqnorm(eicat.1$diffDD0)
qqline(eicat.1$diffDD0)

shapiro.test(eicat.1$diffDDNA) # p < 0.05: null hypothesis (follows normal distribution) is rejected
qqnorm(eicat.1$diffDDNA)
qqline(eicat.1$diffDDNA)

	# Assymetrie
stat.desc(eicat.1$diff, norm= F)

	# Outliers
ggplot(eicat.1, aes(y=diff, x=1))+
	geom_boxplot()

# Wilcoxon test

wilcox.test(eicat.1$mean_eicat, eicat.1$max_eicat, paired = T) # the difference between both methods is significative.

# Between the mean and the mac with DD = 1 --------------------------------

# Hypothesis verification 

eicat.1.1 <- eicat %>% mutate(diff=max_eicat_DDNA-mean_eicat_DDNA) # for paired data

# Normality
shapiro.test(eicat.1.1$diff) # p < 0.05: null hypothesis (follows normal distribution) is rejected
qqnorm(eicat.1.1$diff)
qqline(eicat.1.1$diff)

# Assymetrie
stat.desc(eicat.1.1$diff, norm = F)

# Outliers
ggplot(eicat.1.1, aes(y = diff, x = 1))+
	geom_boxplot()

# Wilcoxon test

wilcox.test(eicat.1.1$mean_eicat_DDNA, eicat.1.1$max_eicat_DDNA, paired = T) # p > 0.5 the difference between both methods is significative.


# Transform NAs to 0 
# (if not, impossible to aggregate score eicat with the potential distribution of the species. See script "distrib_impct.R

eicat$max_eicat_cat_DDNA <- as.character(eicat$max_eicat_cat_DDNA)
eicat <- mutate_at(eicat, "max_eicat_cat_DDNA", ~replace(., is.na(.), "DD"))
eicat <- mutate_at(eicat, c("max_eicat_DDNA", "mean_eicat_DDNA"), ~replace(., is.na(.), 0))

# Save the data
# write.csv2(eicat, file = "output/R_data_eicat_mean_max.csv", row.names = F)