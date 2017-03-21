# Read the list of species 
species <- read.csv('Species.csv', header = TRUE, sep = ",") 

#Unzip kerala forest shape files and read it
unzip('keralaforest.zip')
forestmap <- rgdal::readOGR('keralaforest.shp', 'keralaforest')

# Obtain a unique list of forest divisions
divisions <-  data.frame (forestmap$Division)
divisions <-  divisions[!duplicated(divisions[c("forestmap.Division")]),]

