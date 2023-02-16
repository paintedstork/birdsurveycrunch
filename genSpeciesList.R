library (dplyr)
library(reshape2)

#################################################################
#           Generate Species List from ebd Data                 #
#                                                               #
# Param 1: ebd data                                             #
#################################################################

  
generateSpeciesList <- function(ebd) {

print(nrow(ebd))
if (nrow(ebd) == 0)  { return (NULL) }
  
#Create species list by removing duplicate species entries
ebd_species   <- ebd[!duplicated(ebd[c("Taxonomic.Order","RANGE")]), ] 

#Remove spuhs
ebd_species   <- ebd_species[ebd_species$Category != 'spuh',]

#Remove slashes
ebd_species   <- ebd_species[ebd_species$Category != 'slash',]

#Remove subspecies/issf
ebd_species   <- ebd_species[!duplicated(ebd_species[c("Genus.Name","Species.Name", "RANGE")]),]

# Strip unwanted columns from eBird records
ebd_species <- subset(ebd_species, select = c("Taxonomic.Order","English.India", "Scientific.Name", "RANGE"))

if(length(unique(ebd_species$RANGE)) > 1)
{
  # Add a dummy column with Ones for using in dcast
  ebd_species$No  <- 1
  
  # Create a pivot with AllSpecies vs Forest Ranges for getting the Range wise checklist
  ebd_range_lists <- dcast(ebd_species, Taxonomic.Order + English.India + Scientific.Name ~ RANGE, value.var = "No", fun.aggregate = {function(x) if(length(x)>=1) 1 else 0} )
  
  # Make it "X" or " "
  ebd_range_lists[ebd_range_lists == 0] <- ''
  ebd_range_lists[ebd_range_lists == 1] <- 'X'
}
else
{
  ebd_range_lists <- subset(ebd_species, select = c("Taxonomic.Order", "English.India", "Scientific.Name"))
}

# Remove Taxonomic Order
ebd_range_lists$Taxonomic.Order <- NULL

# Add a Serial Number
ebd_range_lists$SlNo <- seq.int(nrow(ebd_range_lists))

# Move Serial Number as first column
ebd_range_lists <- ebd_range_lists[,c(ncol(ebd_range_lists), 1:ncol(ebd_range_lists)-1)]

# Rename columns
colnames(ebd_range_lists)[2:3] <- c("English Name", "Scientific Name")

return (ebd_range_lists)
}

# Test Code 
testHarness_generateSpeciesList <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEBirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:500] <- 'Sholayar'
  
  output <- generateSpeciesList(ebd)
  write.csv(output, 'testout.csv')
  print (nrow(output))
}

#testHarness_generateSpeciesList()

