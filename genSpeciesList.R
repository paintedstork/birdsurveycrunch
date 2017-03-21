library (plyr)
library (dplyr)

#################################################################
#           Generate Species List from ebd Data                 #
#                                                               #
# Param 1: ebd data                                             #
#################################################################

  
generateSpeciesList <- function(ebd) {

#Create species list by removing duplicate species entries
ebd_species   <- ebd[!duplicated(ebd$Taxonomic.Order),]

#Remove spuhs
ebd_species   <- ebd_species[ebd_species$Category != 'spuh',]

#Remove slashes
ebd_species   <- ebd_species[ebd_species$Category != 'slash',]

#Remove subspecies/issf
ebd_species   <- ebd_species[!duplicated(ebd_species[c("Genus.Name","Species.Name")]),]

ebd_species$SlNo <- seq.int(nrow(ebd_species))

# Strip unwanted columns from eBird records
ebd_species <- subset(ebd_species, select = c("SlNo", "English.India", "Scientific.Name"))

# Rename columns
colnames(ebd_species) <- c("SlNo", "English Name", "Scientific Name")

return (ebd_species)
}

# Test Code 
testHarness_generateSpeciesList <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEbirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- join (ebd, species, by = 'Scientific.Name')
  
  output <- generateSpeciesList(ebd)
  write.csv(output, 'testout.csv')
  print (nrow(output))
}

#testHarness_generateSpeciesList()

