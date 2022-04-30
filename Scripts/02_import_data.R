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

## Keep only with propor rob > 0
RobDatafilt <- RobDataset %>%
  filter(RobDataset$proportion_robinier>0)

## Keep only with propor rob = 0
RobDatafilt2 <- RobDataset %>%
  filter(RobDataset$proportion_robinier==0)
