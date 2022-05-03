#################################################################################
#
#                                IMPORT DATA 
#                             RobBiodiv project 
#
#################################################################################
# Clean R space
graphics.off()
rm(list=ls())

# Load libraries
source("Scripts/01_Load_libraries.r")
#

###### Database ###### 
# Load Data Base
RobDataset <- read.xlsx("Data/Synthese_data.xlsx") %>% .[-c(148,149), ] 

RobDataset <- RobDataset %>% 
  mutate(classe_age3 = as.character(classe_age)) %>% 
  mutate(classe_age3 = replace(classe_age3, classe_age3 == '1', '<10 ans'),
         classe_age3 = replace(classe_age3, classe_age3 == '2', '10-15 ans'),
         classe_age3 = replace(classe_age3, classe_age3 == '3', '15-20 ans'),
         classe_age3 = replace(classe_age3, classe_age3 == '4', '20-30 ans'),
         classe_age3 = replace(classe_age3, classe_age3 == '5', '>30 ans')) 

## Keep only with propor rob > 0
RobDatafilt <- RobDataset %>%
  filter(RobDataset$proportion_robinier>0)

## Keep only with propor rob = 0
RobDatafilt2 <- RobDataset %>%
  filter(RobDataset$proportion_robinier==0)

# Classe age between 0 and 1
RobDataset$classe_age2 <- (RobDataset$classe_age-min(RobDataset$classe_age))/
  (max(RobDataset$classe_age)-min(RobDataset$classe_age))

# Struc vege entre 0 and 1
RobDataset$structure_vegetation2 <- (RobDataset$structure_vegetation-min(RobDataset$structure_vegetation))/
  (max(RobDataset$structure_vegetation)-min(RobDataset$structure_vegetation))

# Micro_hab entre 0 et 1
RobDataset$micro_habitats2 <- (RobDataset$micro_habitats-min(RobDataset$micro_habitats))/
  (max(RobDataset$micro_habitats)-min(RobDataset$micro_habitats))

# Regrouper classe d'age en 3 classe
RobDataset$classe_age4[RobDataset$classe_age == 1] <- 1
RobDataset$classe_age4[RobDataset$classe_age == 2] <- 1
RobDataset$classe_age4[RobDataset$classe_age == 3] <- 2
RobDataset$classe_age4[RobDataset$classe_age == 4] <- 2
RobDataset$classe_age4[RobDataset$classe_age == 5] <- 3

# New Classe age between 0 and 1
RobDataset$classe_age2 <- (RobDataset$classe_age4-min(RobDataset$classe_age4))/
  (max(RobDataset$classe_age4)-min(RobDataset$classe_age4))

# caractère
RobDataset <- RobDataset %>% 
  mutate(classe_age3 = as.character(classe_age4)) %>% 
  mutate(classe_age3 = replace(classe_age3, classe_age3 == '1', '<15 ans'),
         classe_age3 = replace(classe_age3, classe_age3 == '2', '15-30 ans'),
         classe_age3 = replace(classe_age3, classe_age3 == '3', '>30 ans'))



