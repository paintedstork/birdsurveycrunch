
# Read the list of species 
species <- read.csv('Speciesv2.csv', header = TRUE, sep = ",") 

#Unzip kerala forest shape files and read it
#unzip('keralaforest.zip')
#forestmap <- rgdal::readOGR('keralaforest.shp', 'keralaforest')

#unzip('Division&Range7.zip')
forestmap <- rgdal::readOGR('Division&Range7.shp', 'Division&Range7')

# Obtain a unique list of forest divisions
divisions <-  data.frame (forestmap$Division)
divisions <-  divisions[!duplicated(divisions[c("forestmap.Division")]),]
print(divisions)
