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

RobDataset <- read.xlsx("Data/Synthese_data.xlsx") %>% 
  slice(-c(148, 149))

