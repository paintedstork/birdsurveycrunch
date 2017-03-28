library (plyr)
library (dplyr)
library(reshape2)
source("genBirdDensity.R")

generateThreatenedDensity <- function(ebd_density, numeral_col=11) {
  
  ebd_density  <- ebd_density[ebd_density$IUCN != "",]
  
  if (nrow(ebd_density) == 0)  { return (NULL) }
  
  # First numeral_col columns are metadata and not values. 
  ebd_density <- cbind (ebd_density["English Name"], ebd_density["IUCN"],ebd_density [numeral_col:ncol(ebd_density)])
  
  colnames(ebd_density)[1] <- "Species"
  return (ebd_density)
}

# Test Code 
testHarness_generateThreatenedDensity <- function () {
  unzip('..\\data\\ebird_1489816770850.zip')
  ebd     <- read.csv('MyEbirdData.csv', header = TRUE, sep = ",") 
  species <- read.csv('Species.csv', header = TRUE, sep = ",") 
  
  # Obtain details of birds by joining with species file
  ebd <- join (ebd, species, by = 'Scientific.Name')
  ebd$RANGE <- 'Vazhachal'
  ebd$RANGE [100:500] <- 'Sholayar'
  
  output <- generateBirdDensity(ebd) %>% generateThreatenedDensity(11)
  write.csv(output, 'testout.csv')
  print (nrow(output))
}

#testHarness_generateThreatenedDensity()

