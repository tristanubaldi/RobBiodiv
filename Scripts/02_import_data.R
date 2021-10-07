#################################################################################
#
#                                IMPORT DATA 
#                           robinia analysis project
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

robdata <- read.xlsx("Data/Synthese_data.xlsx")
