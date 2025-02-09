library(tidyverse)
library(dplyr)
library(tools)
library(xtable)
#library(sp)
library(ggplot2)
library(utils)


source("shapepuller.R")

# File that will pull shapes and species attributes from Google Drive

# Read the list of species 
species <- read.csv('Species.csv', header = TRUE, sep = ",") 
# Try to overwrite from Google drive
try(species <- getSpecies(), silent=FALSE)

# Initialise a map that is available locally
forestmap <- st_read("Division&Range7.shp") %>% st_make_valid()
divisions <- data.frame(Division = forestmap$Division)
divisions <- divisions[!duplicated(divisions$Division), ]

shapelist <- data.frame(DivisionName = divisions, Polygon = divisions, Folder = 'Division&Range7', stringsAsFactors = FALSE)
# Obtain a unique list of forest divisions

# Try to overwrite from Google drive
try(shapelist <- getShapeList(), silent=FALSE)

shapelist <- shapelist %>% distinct(DivisionName, .keep_all = TRUE)

# Try to overwrite from Google drive
#try(forestmap <- getShapes(), silent=FALSE) 
#forestmap <- getShapes()
#print(data.frame (forestmap$Division) %>% nrow())

# Obtain a unique list of forest divisions
divisions <- shapelist$DivisionName

print(divisions)
