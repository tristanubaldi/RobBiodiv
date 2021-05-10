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
source("01_Load_libraries.r")

###### Database ###### 
# Load Data Base

robdata <- read.csv("Data/Rob_data.csv", stringsAsFactors=FALSE, header=TRUE,  
                 sep=";", dec=",")
