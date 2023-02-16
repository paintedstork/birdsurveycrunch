library(tidyverse)
library(dplyr)
library(tools)
library(xtable)
library(sp)
library(ggplot2)
library(utils)
library(rgdal)


source("shapepuller.R")

# File that will pull shapes and species attributes from Google Drive

# Read the list of species 
species <- read.csv('Speciesv2.csv', header = TRUE, sep = ",") 
# Try to overwrite from Google drive
try(species <- getSpecies(), silent=FALSE)

#unzip('Division&Range7.zip')
forestmap <- rgdal::readOGR('Division&Range7.shp', 'Division&Range7')
# Try to overwrite from Google drive
try(forestmap <- getShapes(), silent=FALSE) 
print(data.frame (forestmap$Division) %>% nrow())

# Obtain a unique list of forest divisions
divisions <-  data.frame (forestmap$Division)
divisions <-  divisions[!duplicated(divisions[c("forestmap.Division")]),]
print(divisions)
